#ifndef UDPINTERFACE_H_
#define UDPINTERFACE_H_

#include <unordered_map>
#include <memory>
#include "Interface.h"

class UDPInterface : public Interface {
 public:
  UDPInterface(const std::string&);
  virtual ~UDPInterface() = default;

  UDPInterface(const UDPInterface&) = delete;
  const UDPInterface& operator= (const UDPInterface&) = delete;

  bool match(const std::string&, uint8_t, const std::string&) override;
  void send(const std::string&, uint8_t, const std::string&) override;

 protected:
  virtual void _send_udp(const std::string&, uint16_t, uint16_t, const std::string&);
  void _receive_cb_pack_udp(const std::string&, uint16_t, uint16_t, const std::string&);
 private:
  uint8_t _address[16];
  uint16_t _port;
  uint16_t _cs_pre;
};

#endif // UDPINTERFACE_H_
