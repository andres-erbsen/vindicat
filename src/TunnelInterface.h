#ifndef TUNNELINTERFACE_H_
#define TUNNELINTERFACE_H_

#include "Interface.h"
#include "IPv6.h"
#include "TUNInterface.h"
#include "NetworkMap.h"

#include <string>
#include <unordered_map>
#include <memory>

class TunnelInterface : public Interface {
 public:
  TunnelInterface(std::unique_ptr<TUNInterface>&, NetworkMap&);
  void send(const std::string&, uint8_t, const std::string&);
  bool match(const std::string&, uint8_t, const std::string&);

  void tunnel(const IPv6::Packet&);

 private:
  std::unique_ptr<TUNInterface>& _tun;
  NetworkMap& _nm;
  std::unordered_map<IPv6::Address, std::string> _connections;
};

#endif  // TUNNELINTERFACE_H_
