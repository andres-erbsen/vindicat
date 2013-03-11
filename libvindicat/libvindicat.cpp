#include "libvindicat.h"
#include "libvindicat.pb.h"

#include <system_error>

#include <cstring>

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

libvindicat::Connection::Connection(const std::string& server_path,
                                    const std::string& client_path) {
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

  if(bind(_fd, reinterpret_cast<sockaddr*>(_client), sizeof(*_client)) == -1) {
    delete _client;
    delete _server;
    close(_fd);
    throw std::system_error(errno, std::system_category());
  }

  if(connect(_fd, reinterpret_cast<sockaddr*>(_server),
             sizeof(sockaddr_un)) == -1) {
    delete _client;
    delete _server;
    close(_fd);
    unlink(client_path.c_str());
    throw std::system_error(errno, std::system_category());
  }

  send(std::string("\x00", 1));
  std::string packet;
  do
    packet = recv();
  while(packet[0] != '\x00');
  libvindicat::ConfigurationResponse conf;
  if(!conf.ParseFromString(packet.substr(1))) {
    delete _client;
    delete _server;
    close(_fd);
    unlink(client_path.c_str());
    throw std::runtime_error("Configuration message is corrupt");
  }
  _identificator = conf.local_identifier();
  if(conf.ipv6_prefix_length() % 8 != 0) {
    delete _client;
    delete _server;
    close(_fd);
    unlink(client_path.c_str());
    throw std::runtime_error("IPv6 address calculation not complete");
  }
  _ipv6 = (conf.ipv6_prefix()+_identificator).substr(0, 16);
  if(_ipv6.size() < 16) {
    delete _client;
    delete _server;
    close(_fd);
    unlink(client_path.c_str());
    throw std::runtime_error("IPv6 address too short");
  }
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
  std::size_t buflen = 0, buflen_len = sizeof(buflen);
  if(getsockopt(_fd, SOL_PACKET, SO_RCVBUF, &buflen, &buflen_len) == -1)
    throw std::system_error(errno, std::system_category());
  if(buflen_len != sizeof(buflen))
    throw std::runtime_error("Alignment error");
  char *buf = new char[buflen];
  auto res = ::recv(_fd, buf, buflen, 0);
  if(res == -1)
    throw std::system_error(errno, std::system_category());
  std::string ret(buf, res);
  delete[] buf;
  return ret;
}
