#ifndef TUNNELINTERFACE_H_
#define TUNNELINTERFACE_H_

#include "Interface.h"
#include "IPv6.h"
#include "NetworkMap.h"

#include <string>
#include <unordered_map>
#include <functional>

class TunnelInterface : public Interface {
 public:
  TunnelInterface(NetworkMap&);
  void set_send(std::function<void(const IPv6::Packet&)>&&);
  void send(const std::string&, uint8_t, const std::string&);
  bool match(const std::string&, uint8_t, const std::string&);

  void tunnel(const IPv6::Packet&);

 private:
  NetworkMap& _nm;
  std::unordered_map<IPv6::Address, std::string> _connections;
  std::function<void(const IPv6::Packet&)> _send;
};

#endif  // TUNNELINTERFACE_H_
