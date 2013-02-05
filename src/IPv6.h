#ifndef IPV6_H_
#define IPV6_H_

#include <cstdint>
#include <ostream>

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
    const static int header_length = 40;
    // Allocate storage and set protocol version
    explicit Packet(std::size_t len);

    // Copy from buffer
    Packet(const std::uint8_t *packet, std::size_t len);
    Packet(const Packet &packet);
    
    virtual ~Packet();
    
    Packet &operator=(const Packet &packet);
    
    unsigned version() const;    
    void version(unsigned ver);
        
    unsigned traffic_class() const;    
    void traffic_class(unsigned tr);
        
    std::uint32_t flow_label() const;
    void flow_label(std::uint32_t label);
    
    std::uint16_t payload_length() const;
    void payload_length(std::uint16_t len);
    
    unsigned next_header() const;    
    void next_header(unsigned header);
        
    unsigned hop_limit() const;
    void hop_limit(unsigned limit);
    
    Address source() const;
    void source(const Address &src);
    
    Address destination() const;
    void destination(const Address &dst);
    
    std::uint8_t *payload();
    const std::uint8_t *payload() const;
    std::uint8_t *data();
    const std::uint8_t *data() const;
    
    static Packet reassemble(const Address &src, const Address &dst,
      std::uint8_t next_header, const std::uint8_t *payload,
      std::size_t payload_length);
    static Packet read(int fd);
  };
}
#endif
