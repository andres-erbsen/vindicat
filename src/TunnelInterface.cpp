#include "TunnelInterface.h"
#include "Util.h"
#include <random>
#include <cstring>

#define TUNNEL_PROTOCOL 0xDD

TunnelInterface::TunnelInterface(std::unique_ptr<TUNInterface>& tun,
    NetworkMap& nm)
  : _tun(tun), _nm(nm) {
}

bool TunnelInterface::match(const std::string&, uint8_t protocol,
    const std::string&) {
  return protocol == TUNNEL_PROTOCOL;
}

void TunnelInterface::send(const std::string&, uint8_t,
    const std::string& payload) {
  IPv6::Packet packet(payload.size());
  std::memcpy(packet.data(), payload.data(), payload.size());
  _tun->send(packet);
}

struct rand64 {
  typedef std::uint64_t result_type;
  std::uint64_t operator()() {
    return randint64();
  }

  static constexpr std::uint64_t max() {
    return 0xFFFFFFFFFFFFFFFF;
  }

  static constexpr std::uint64_t min() {
    return 0;
  }
};

void TunnelInterface::tunnel(const IPv6::Packet& packet) {
  if(_connections.count(packet.destination()) == 0) {
    auto devices = _nm.has_capability("IPv6-Tunnel");
    if(devices.size() == 0) {
      // Reject packet
      // ICMPv6: Destination Unreachable: no route to destination
      _tun->send(IPv6::Packet::generate_ICMPv6(packet.destination(),
            packet.source(), 1, 0, std::string(4, 0) +
            std::string(reinterpret_cast<const char*>(packet.data()), 1232)));
    } else {
      rand64 rand64;
      std::uniform_int_distribution<std::size_t> distrib(0, devices.size());
      _connections[packet.destination()] = devices[distrib(rand64)]->id();
    }
  }

  _receive_cb(std::string(_connections[packet.destination()]),
      std::string(1, TUNNEL_PROTOCOL) +
          std::string(reinterpret_cast<const char*>(packet.data()), 
                      IPv6::Packet::header_length+packet.payload_length()));
}
