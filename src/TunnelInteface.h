#ifndef TUNNELINTERFACE_H_
#define TUNNELINTERFACE_H_

#include "Interface.h"

class TunnelInterface : public Interface {
 public:
  void send(const std::string& uint8_t, const std::string&);
  bool match(const std::string&, uint8_t, const std::string&);

  void tunnel(const IPv6::Packet&);

 private:
  std::unordered_map<std::string, std::string> _connections;
};

#endif  // TUNNELINTERFACE_H_
