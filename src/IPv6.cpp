#include "IPv6.h"

#include <arpa/inet.h>
#include <cstring>
#include <iomanip>

IPv6::Address::Address()
{
  std::memset(address, 0, 16);
}

std::ostream &IPv6::operator<<(std::ostream &out, const IPv6::Address &addr)
{
  const std::uint16_t *address = reinterpret_cast<const std::uint16_t*>(addr.address);
  auto flags = out.flags();
  auto fill = out.fill();
  out.fill('0');
  out << std::hex << std::setw(4) << ntohs(address[0]);
  for(int i = 1; i < 8; i++)
    out << ':' << std::setw(4) << ntohs(address[i]);
  out.fill(fill);
  out.flags(flags);
  return out;
}

IPv6::Packet::Packet(std::size_t len):
  _data(new std::uint8_t[len]), _data_length(len)
{
  version(6);
}

IPv6::Packet::Packet(const std::uint8_t *packet, std::size_t len):
  Packet(len)
{
  std::memcpy(_data, packet, len);
}

IPv6::Packet::Packet(const IPv6::Packet &packet):
  Packet(packet._data, packet._data_length)
{
}

IPv6::Packet::~Packet()
{
  delete[] _data;
}

IPv6::Packet &IPv6::Packet::operator=(const IPv6::Packet &packet)
{
  if(_data != packet._data)
  {
    std::uint8_t *newdata = new std::uint8_t[packet._data_length];
    delete[] _data;
    _data = newdata;
    _data_length = packet._data_length;
    std::memcpy(_data, packet._data, _data_length);
  }
  return *this;
}

IPv6::Packet IPv6::reassembleTCP(const IPv6::Address &src,
  const IPv6::Address &dst, const std::uint8_t *payload,
  std::uint32_t length)
{
  IPv6::Packet ret(length+40); // IPv6 header is 40 bytes
  ret.source(src);
  ret.destination(dst);
  ret.next_header(0x06);
  ret.payload_length(length);
  ret.hop_limit(64);
  std::memcpy(ret.payload(), payload, length);
 
  std::uint32_t checksum = 0;
  
  // IPv6 header
  for(int i = 0; i < 16; i+=2)
  {
    // Source address
    checksum += (src.address[i] << 8) & 0xFF00;
    checksum += (src.address[i+1]   ) & 0x00FF;
    // Destination address
    checksum += (dst.address[i] << 8) & 0xFF00;
    checksum += (dst.address[i+1]   ) & 0x00FF;
  }
  // Length
  checksum += length;
  // Next Header
  checksum += 0x06;
    
  // TCP header and data
  // Set checksum to zero
  ret.payload()[16] = 0;
  ret.payload()[17] = 0;
  for(std::size_t i = 0; i < length; i+=2)
  {
    checksum += (ret.payload()[i] << 8) & 0xFF00;
    checksum += (ret.payload()[i+1]   ) & 0x00FF;
  }
    
  while(checksum >> 16)
    checksum = (checksum & 0xFFFF) + ((checksum >> 16) & 0xFFFF);
  ret.payload()[16] = ((~checksum) >> 8) & 0xFF;
  ret.payload()[17] = (~checksum) & 0xFF;
    
  return ret;
}

IPv6::Packet IPv6::reassembleUDP(const IPv6::Address &src,
  const IPv6::Address &dst, const std::uint8_t *payload,
  std::uint32_t length)
{
  IPv6::Packet ret(length+40); // IPv6 header is 40 bytes

  ret.source(src);
  ret.destination(dst);
  ret.next_header(0x11);
  ret.payload_length(length);
  ret.hop_limit(64);
  std::memcpy(ret.payload(), payload, length);
  
  std::uint32_t checksum = 0;
  
  // IPv6 header
  for(int i = 0; i < 16; i+=2)
  {
    // Source address
    checksum += (src.address[i] << 8) & 0xFF00;
    checksum += (src.address[i+1]   ) & 0x00FF;
    // Destination address
    checksum += (dst.address[i] << 8) & 0xFF00;
    checksum += (dst.address[i+1]   ) & 0x00FF;
  }
  // Length
  checksum += length;
  // Next Header
  checksum += 0x11;
   
  // UDP header and data
  // Set checksum to zero
  ret.payload()[6] = 0;
  ret.payload()[7] = 0;
  for(std::size_t i = 0; i < length; i+=2)
  {
    checksum += (ret.payload()[i] << 8) & 0xFF00;
    checksum += (ret.payload()[i+1]   ) & 0x00FF;
  }
    
  while(checksum >> 16)
    checksum = (checksum & 0xFFFF) + ((checksum >> 16) & 0xFFFF);
  ret.payload()[6] = ((~checksum) >> 8) & 0xFF;
  ret.payload()[7] = (~checksum) & 0xFF;
  
  return ret;
}
