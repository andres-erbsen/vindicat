#include "libvindicat.h"
#include "libvindicat.pb.h"

#include <system_error>

#include <cstring>

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <netinet/in.h>

libvindicat::Connection::Connection(const std::string& server_path,
                                    const std::string& client_path) 
try {
  if((_fd = socket(AF_UNIX, SOCK_DGRAM, 0)) == -1)
    throw std::system_error(errno, std::system_category());

  _server = new sockaddr_un;
  std::memset(_server, 0, sizeof(sockaddr_un));
  _server->sun_family = AF_UNIX;
  std::memcpy(_server->sun_path, server_path.data(),
              std::min(server_path.size(), sizeof(sockaddr_un::sun_path)));

  _client = new sockaddr_un;
  std::memset(_client, 0, sizeof(sockaddr_un));
  _client->sun_family = AF_UNIX;
  std::memcpy(_client->sun_path, client_path.data(),
              std::min(server_path.size(), sizeof(sockaddr_un::sun_path)));

  if(bind(_fd, reinterpret_cast<sockaddr*>(_client), sizeof(*_client)) == -1)
    throw std::system_error(errno, std::system_category());

  if(connect(_fd, reinterpret_cast<sockaddr*>(_server), sizeof(*_server)) == -1)
    throw std::system_error(errno, std::system_category());

  // Send a configuration request.
  send(std::string("\x00", 1));
} catch(...) {
  delete _client;
  delete _server;
  close(_fd);
  unlink(client_path.c_str());
}

libvindicat::Connection::~Connection() {
  for(auto socket : _sockets)
    delete socket;
  close(_fd);
  unlink(_client->sun_path);
  delete _client;
  delete _server;
}

void libvindicat::Connection::send(const std::string& payload) const {
  if(::send(_fd, payload.data(), payload.size(), 0) == -1)
    throw std::system_error(errno, std::system_category());
}

std::string libvindicat::Connection::recv() const {
  char *buf = new char[4096];
  auto res = ::recv(_fd, buf, 4096, 0);
  if(res == -1)
    throw std::system_error(errno, std::system_category());
  std::string ret(buf, res);
  delete[] buf;
  return ret;
}

void libvindicat::Connection::read() {
  std::string packet = recv();
  switch(packet[0]) {
    case 0x00: {  // Configuration response
			ConfigurationResponse conf;
			if(!conf.ParseFromString(packet.substr(1)))
			  throw std::runtime_error("Invalid ConfigurationResponse");
			if(conf.ipv6_prefix_length() % 8 != 0)
				throw std::runtime_error("Unalingned IPv6 prefixes not implemented");
			_ipv6 = (conf.ipv6_prefix()+conf.local_identifier()).substr(0, 16);
			if(_ipv6.size() < 16)
				_ipv6 += std::string(16-_ipv6.size(), '\0');
			_identifier = conf.local_identifier();
      break;
    }
		case 0x01:  // Forwarding request
		  throw std::runtime_error("Invalid forwarding request");
		case 0x02: {  // Packet
			Packet p;
			if(!p.ParseFromString(packet.substr(1)))
				throw std::runtime_error("Invalid Packet");
			for(auto socket : _sockets)
				socket->forward(p.identifier(), p.next_header(), p.payload());
		}
		case 0x03:  // Ping
      break;
		case 0x04:  // Remove forwarding
			throw std::runtime_error("Invalid forwarding removal request");
  }
}

std::string libvindicat::Connection::ipv6() const {
	return _ipv6;
}

std::string libvindicat::Connection::identifier() const {
	return _identifier;
}

int libvindicat::Connection::selectable_fd() const {
  return _fd;
}

libvindicat::RawSocket* libvindicat::Connection::forward(std::uint8_t proto) {
	ForwardRequest request;
	request.set_next_header(proto);
	std::string msg("\x01", 1);
	request.AppendToString(&msg);
	send(msg);
  request.ParseFromString(msg.substr(1));
	auto ret = new RawSocket(*this, proto);
	_sockets.push_back(ret);
	return ret;
}

libvindicat::UDPSocket* libvindicat::Connection::forwardUDP(std::uint16_t p) {
	ForwardRequest request;
	request.set_next_header(IPPROTO_UDP);
	request.mutable_udp()->set_port(p);
	std::string msg("\x01", 1);
	request.AppendToString(&msg);
	send(msg);
	auto ret = new UDPSocket(*this, p);
  _sockets.push_back(ret);
	return ret;
}

void libvindicat::RawSocket::forward(const std::string& id, std::uint8_t proto,
     	                               const std::string& payload) {
  if(proto == _proto)
    _cb(id, payload);
}

void libvindicat::UDPSocket::forward(const std::string& id, std::uint8_t proto,
                                     const std::string& payload) {
  const std::uint16_t *header =
      reinterpret_cast<const std::uint16_t*>(payload.data());
  if(proto == IPPROTO_UDP && header[1] == _port)
    _cb(id, header[1], payload.substr(8));
}

libvindicat::RawSocket::RawSocket(libvindicat::Connection& conn,
                                  std::uint8_t proto)
    : _conn(conn), _proto(proto) {
}

libvindicat::UDPSocket::UDPSocket(libvindicat::Connection& conn,
                                  std::uint16_t port)
    : _conn(conn), _port(port) {
}

libvindicat::RawSocket::~RawSocket() noexcept {
}

libvindicat::UDPSocket::~UDPSocket() noexcept {
}
