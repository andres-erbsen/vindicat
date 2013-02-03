#ifndef IPV6_H_
#define IPV6_H_

namespace IPv6
{
  struct Address
  {
    std::uint8_t address[16];
    Address();
  };
  
  std::ostream &operator<<(std::ostream &out, const Address &addr);
  
  class Packet
  {
    std::uint8_t *_data;
    std::size_t _data_length;
  public:
    // Allocate storage and set protocol version
    explicit Packet(std::size_t len);

    // Copy from buffer
    Packet(const std::uint8_t *packet, std::size_t len);
    Packet(const Packet &packet);
    
    virtual ~Packet();
    
    Packet &operator=(const Packet &packet);
    
    unsigned version() const
    {
      return (_data[0] & 0xF0) >> 4;
    }
    
    void version(unsigned ver)
    {
      _data[0] = (_data[0] & 0x0F) | ((ver << 4) & 0xF0);
    }
    
    unsigned traffic_class() const
    {
      return (_data[0] & 0x0F) | (_data[1] & 0xF0);
    }
    
    void traffic_class(unsigned tr)
    {
      _data[0] = (_data[0] & 0xF0) | ((tr >> 4) & 0x0F);
      _data[1] = (_data[1] & 0x0F) | ((tr << 4) & 0xF0);
    }
    
    std::uint32_t flow_label() const
    {
      return ntohl(((_data[1]&0x0F)<<8)|(_data[2]<<16)|(_data[3]<<24));
    }
    
    void flow_label(std::uint32_t label)
    {
      std::uint8_t label_n[4];
      label = htonl(label);
      memcpy(label_n, reinterpret_cast<void*>(&label), 4);
      _data[1] = (_data[1] & 0xF0) | (label_n[1] & 0x0F);
      _data[2] = label_n[2];
      _data[3] = label_n[3];
    }
    
    std::uint16_t payload_length() const
    {
      return ntohs(_data[4] | (_data[5] << 8));
    }
    
    void payload_length(std::uint16_t len)
    {
      len = htons(len);
      _data[4] = len & 0xFF;
      _data[5] = (len >> 8) & 0xFF;
    }
    
    unsigned next_header() const
    {
      return _data[6];
    }
    
    void next_header(unsigned header)
    {
      _data[6] = header;
    }
    
    unsigned hop_limit() const
    {
      return _data[7];
    }
    
    void hop_limit(unsigned limit) const
    {
      _data[7] = limit;
    }
    
    Address source() const
    {
      Address ret;
      std::memcpy(ret.address, _data+8, 16);
      return ret;
    }
    
    void source(const Address &src)
    {
      std::memcpy(_data+8, src.address, 16);
    }
    
    Address destination() const
    {
      Address ret;
      std::memcpy(ret.address, _data+24, 16);
      return ret;
    }
    
    void destination(const Address &dst)
    {
      std::memcpy(_data+24, dst.address, 16);
    }
    
    std::uint8_t *payload()
    {
      return _data+40;
    }
    
    std::uint8_t *data()
    {
      return _data;
    }
  };
  
  Packet reassembleTCP(const Address &src, const Address &dst,
    const std::uint8_t *payload, std::uint32_t length);  
  Packet reassembleUDP(const Address &src, const Address &dst,
    const std::uint8_t *payload, std::uint32_t length);
}
#endif
