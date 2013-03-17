#include <libvindicat/UDPSocket.h>
#include <libvindicat/Connection.h>
#include "libvindicat.pb.h"

#include <netinet/in.h>

void libvindicat::UDPSocket::forward(const std::string& id, std::uint8_t proto,
                                     const std::string& payload) {
  const std::uint16_t *header =
      reinterpret_cast<const std::uint16_t*>(payload.data());
  if(proto == IPPROTO_UDP && header[1] == _port)
    _cb(id, header[1], payload.substr(8));
}

libvindicat::UDPSocket::UDPSocket(libvindicat::Connection& conn,
                                  std::uint16_t port)
    : _conn(conn), _port(port) {
}

libvindicat::UDPSocket::~UDPSocket() noexcept {
  ForwardRequest request;
  request.set_next_header(IPPROTO_UDP);
  request.mutable_udp()->set_port(_port);
  std::string msg("\x04", 1);
  request.AppendToString(&msg);
  try {
    _conn.send(msg);
  } catch(...) {
  }
}

void libvindicat::UDPSocket::set_callback(
    std::function<void(const std::string&, std::uint16_t, const std::string&)>
        &&cb) {
  _cb = cb;
}

void libvindicat::UDPSocket::sendto(const std::string& to, std::uint16_t port,
                                    const std::string& payload) const {
  std::uint16_t header[4] = {_port, port,
      static_cast<std::uint16_t>(sizeof(header)+payload.size()), 0};

  // Checksum:
  // Source IP:
  std::size_t checksum = 0;
  const std::uint16_t *data =
    reinterpret_cast<const std::uint16_t*>(_conn._ipv6.data());
  for(std::size_t i = 0; i < _conn._ipv6.size()/sizeof(std::uint16_t); i++)
    checksum += data[i];
  // Destination IP:
  std::string dest_ip = (_conn._ipv6_prefix+to).substr(0, 16);
  if(dest_ip.size() < 16)
    dest_ip += std::string(16-dest_ip.size(), '\x00');
  data = reinterpret_cast<const std::uint16_t*>(dest_ip.data());
  for(std::size_t i = 0; i< dest_ip.size()/sizeof(std::uint16_t); i++)
    checksum += data[i];
  // UDP header
  for(std::size_t i = 0; i < sizeof(header)/sizeof(std::uint16_t); i++)
    checksum += header[i];
  // Protocol number
  checksum += IPPROTO_UDP;
  // Payload
  data = reinterpret_cast<const std::uint16_t*>(payload.data());
  for(std::size_t i = 0; i < payload.size()/sizeof(std::uint16_t); i++)
    checksum += data[i];
  if(payload.size() % 2 == 1)
    checksum += payload.back();

  while(checksum > 0xFFFF)
    checksum = (checksum & 0xFFFF) + (checksum >> 16);
  header[3] = ~checksum;

  Packet packet;
  packet.set_identifier(to);
  packet.set_next_header(IPPROTO_UDP);
  packet.set_payload(
      std::string(reinterpret_cast<char*>(header), sizeof(header))+payload);
  std::string msg("\x02", 1);
  packet.AppendToString(&msg);
  _conn.send(msg);
}
