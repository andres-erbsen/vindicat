#include "IPv6.h"
#include "Log.h"

#include <arpa/inet.h>
#include <cstring>
#include <iomanip>
#include <cassert>
#include <system_error>
#include <unistd.h>

IPv6::Address::Address()
{
  std::memset(address, 0, 16);
}

bool IPv6::Address::operator==(const IPv6::Address& addr) const {
  return std::memcmp(address, addr.address, 16) == 0;
}

std::ostream &operator<<(std::ostream &out, const IPv6::Address &addr)
{
  char buf[40];
  out << inet_ntop(AF_INET6, addr.address, buf, 40);
  return out;
}

IPv6::Packet::Packet(std::size_t len):
  _data(new std::uint8_t[len]), _data_length(len)
{
  version(6);
  payload_length(len-header_length);
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

unsigned IPv6::Packet::version() const
{
  return (_data[0] & 0xF0) >> 4;
}
    
void IPv6::Packet::version(unsigned ver)
{
  _data[0] = (_data[0] & 0x0F) | ((ver << 4) & 0xF0);
}
    
unsigned IPv6::Packet::traffic_class() const
{
  return (_data[0] & 0x0F) | (_data[1] & 0xF0);
}
    
void IPv6::Packet::traffic_class(unsigned tr)
{
  _data[0] = (_data[0] & 0xF0) | ((tr >> 4) & 0x0F);
  _data[1] = (_data[1] & 0x0F) | ((tr << 4) & 0xF0);
}
   
std::uint32_t IPv6::Packet::flow_label() const
{
  return ntohl(((_data[1]&0x0F)<<8)|(_data[2]<<16)|(_data[3]<<24));
}
    
void IPv6::Packet::flow_label(std::uint32_t label)
{
  std::uint8_t label_n[4];
  label = htonl(label);
  std::memcpy(label_n, reinterpret_cast<void*>(&label), 4);
  _data[1] = (_data[1] & 0xF0) | (label_n[1] & 0x0F);
  _data[2] = label_n[2];
  _data[3] = label_n[3];
}
    
std::uint16_t IPv6::Packet::payload_length() const
{
  return ntohs(_data[4] | (_data[5] << 8));
}
    
void IPv6::Packet::payload_length(std::uint16_t len)
{
  len = htons(len);
  _data[4] = len & 0xFF;
  _data[5] = (len >> 8) & 0xFF;
}
    
unsigned IPv6::Packet::next_header() const
{
  return _data[6];
}
    
void IPv6::Packet::next_header(unsigned header)
{
  _data[6] = header;
}
    
unsigned IPv6::Packet::hop_limit() const
{
  return _data[7];
}
    
void IPv6::Packet::hop_limit(unsigned limit)
{
  _data[7] = limit;
}
    
IPv6::Address IPv6::Packet::source() const
{
  IPv6::Address ret;
  std::memcpy(ret.address, _data+8, 16);
  return ret;
}

void IPv6::Packet::source(const IPv6::Address &src)
{
  std::memcpy(_data+8, src.address, 16);
}
   
IPv6::Address IPv6::Packet::destination() const
{
  IPv6::Address ret;
  std::memcpy(ret.address, _data+24, 16);
  return ret;
}
    
void IPv6::Packet::destination(const IPv6::Address &dst)
{
  std::memcpy(_data+24, dst.address, 16);
}
    
std::uint8_t *IPv6::Packet::payload()
{
  return _data+header_length;
}

const std::uint8_t *IPv6::Packet::payload() const
{
  return _data+header_length;
}
   
std::uint8_t *IPv6::Packet::data()
{
  return _data;
}

const std::uint8_t *IPv6::Packet::data() const
{
  return _data;
}

IPv6::Packet IPv6::Packet::reassemble(const IPv6::Address &src,
  const IPv6::Address &dst, std::uint8_t next_header,
  const std::uint8_t *payload, std::size_t payload_length)
{
  IPv6::Packet ret(payload_length+header_length);
  ret.source(src);
  ret.destination(dst);
  ret.next_header(next_header);
  ret.payload_length(payload_length);
  ret.hop_limit(64);
  std::memcpy(ret.payload(), payload, payload_length);
  return ret;
}

IPv6::Packet IPv6::Packet::reassemble(const IPv6::Address &src,
  const IPv6::Address &dst, std::uint8_t next_header,
  const std::string &payload)
{
  return IPv6::Packet::reassemble(src, dst, next_header,
    reinterpret_cast<const std::uint8_t*>(payload.c_str()), payload.size());
}

IPv6::Packet IPv6::Packet::read(int fd)
{
  IPv6::Packet packet(IPv6::Packet::max_packet_size);
  if(::read(fd, packet.data(), IPv6::Packet::max_packet_size) == -1)
    FATAL().perror("read");
  assert(packet.version() == 6);
  return packet;
}

IPv6::Packet IPv6::Packet::generate_ICMPv6(const IPv6::Address& src, 
    const IPv6::Address& dest, std::uint8_t type, std::uint8_t code,
    const std::string& payload) {
  IPv6::Packet packet(IPv6::Packet::header_length+4+payload.size());
  packet.source(src);
  packet.destination(dest);
  packet.next_header(58);
  packet.payload()[0] = type;
  packet.payload()[1] = code;
  packet.payload()[2] = 0;
  packet.payload()[3] = 0;
  std::memcpy(packet.payload()+4, payload.data(), payload.size());

  std::uint32_t checksum = 0;
  for(int i = 0; i < 16; i += 2) {
    checksum += (src.address[i] << 8) & 0xFF00;
    checksum += src.address[i+1];
    checksum += (dest.address[i] << 8) & 0xFF00;
    checksum += dest.address[i+1];
  }
  checksum += packet.payload_length();
  checksum += 58;
  std::size_t length = packet.payload_length()/2*2;
  for(int i = 0; i < length; i += 2) {
    checksum += (packet.payload()[i] << 8) & 0xFF00;
    checksum += packet.payload()[i+1];
  }
  if(length != packet.payload_length())
    checksum += (packet.payload()[packet.payload_length()-1] << 8) & 0xFF00;
  while(checksum >> 16)
    checksum = (checksum & 0xFFFF) + (checksum >> 16);
  packet.payload()[2] = ((~checksum) >> 8) & 0xFF;
  packet.payload()[3] = (~checksum) & 0xFF;

  return packet;
}
