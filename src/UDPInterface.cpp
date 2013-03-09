#include "UDPInterface.h"
#include "Util.h"

const uint8_t UDP_PROTO_NUM = 0x11;
const uint8_t IPv6_ADDRESS_PREFIX = 0x04;

static uint16_t udpchecksum(const uint8_t* src, const uint8_t* dst, const std::string& packet) {
   // IPv6 header: "next header", length (order of additions does not matter)
  uint32_t checksum = UDP_PROTO_NUM + packet.size();
  // IPv6 header: both addresses at once
  for (int i = 0; i < 16; i+=2) {
    checksum += (src[i] << 8) & 0xFF00;
    checksum += (src[i+1]   ) & 0x00FF;
    checksum += (dst[i] << 8) & 0xFF00;
    checksum += (dst[i+1]   ) & 0x00FF;
  }
  for (size_t i = 0; i < packet.size(); i+=2) {
    checksum += (packet[i] << 8) & 0xFF00;
    checksum += (packet[i+1]   ) & 0x00FF;
  }
  while (checksum >> 16)
    checksum = (checksum & 0xFFFF) + ((checksum >> 16) & 0xFFFF);
  return ~checksum;
}

static uint16_t udpchecksum(const uint8_t* src, const char* dst, const std::string& packet) {
  return udpchecksum(src, reinterpret_cast<const uint8_t*>(dst), packet);
}

UDPInterface::UDPInterface(const std::string& our_id) {
  _address[0] = IPv6_ADDRESS_PREFIX;
  for (int i=0; i<15; ++i) _address[i+1] = our_id.at(i);
}

bool UDPInterface::match( const std::string& form,
                          uint8_t protocol,
                          const std::string& packet) {
  if ( protocol != UDP_PROTO_NUM || packet.size() < 8 ) return false;
  uint16_t dst_port = packet[2] << 8 | packet[3];
  return _port == dst_port;
}

void UDPInterface::send( const std::string& from,
                         uint8_t protocol,
                         const std::string& packet) {
  uint16_t src_port = packet[0] << 8 | packet[1];
  uint16_t dst_port = packet[2] << 8 | packet[3];
  if (udpchecksum(_address, from.data(), packet) != 0) return;
  _send_udp(from, src_port, dst_port, packet.substr(8));
}

void UDPInterface::_receive_cb_pack_udp( const std::string& to,
                                         uint16_t src_port,
                                         uint16_t dst_port,
                                         const std::string& payload ) {
  std::string packet;
  packet.reserve( 4*2 + payload.size() );
  packet += bytes(src_port) + bytes(dst_port);
  packet += bytes(static_cast<uint16_t>( payload.size()));
  packet += bytes(uint16_t(0));
  packet += payload;
  uint16_t cs = udpchecksum(_address, to.data(), packet);
  packet[6] = (cs >> 8) & 0xFF;
  packet[7] = (cs     ) & 0xFF;
  _receive_cb(to.substr(1), bytes(UDP_PROTO_NUM) + packet);
}
