#include "IPv6.h"

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
  IPv6::Packet header(IPv6::Packet::header_length);
  ssize_t bytes = ::read(fd, header.data(), IPv6::Packet::header_length); 
  if(bytes == -1)
    throw std::system_error(errno, std::system_category());
  assert(bytes == IPv6::Packet::header_length);
  assert(header.version() == 6);
  IPv6::Packet packet(IPv6::Packet::header_length+header.payload_length());
  std::memcpy(packet.data(), header.data(), IPv6::Packet::header_length);
  bytes = ::read(fd, packet.payload(), header.payload_length());
  if(bytes == -1)
    throw std::system_error(errno, std::system_category());
  assert(bytes == header.payload_length());
  return packet;
}

