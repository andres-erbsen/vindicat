#include <libvindicat/RawSocket.h>
#include <libvindicat/Connection.h>
#include "libvindicat.pb.h"

void libvindicat::RawSocket::forward(const std::string& id, std::uint8_t proto,
     	                               const std::string& payload) {
  if(proto == _proto)
    _cb(id, payload);
}

libvindicat::RawSocket::RawSocket(libvindicat::Connection& conn,
                                  std::uint8_t proto)
    : _conn(conn), _proto(proto) {
}

libvindicat::RawSocket::~RawSocket() noexcept {
  ForwardRequest request;
  request.set_next_header(_proto);
  std::string msg("\x04", 1);
  request.AppendToString(&msg);
  try {
    _conn.send(msg);
  } catch(...) {
  }
}

void libvindicat::RawSocket::set_callback(
    std::function<void(const std::string&, const std::string&)> &&cb) {
  _cb = cb;
}

void libvindicat::RawSocket::sendto(const std::string& to,
                                    const std::string& payload) const {
  Packet packet;
  packet.set_identifier(to);
  packet.set_next_header(_proto);
  packet.set_payload(payload);
  std::string msg("\x02", 1);
  packet.AppendToString(&msg);
  _conn.send(msg);
}
