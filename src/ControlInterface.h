#ifndef UDPINTERFACE_H_
#define UDPINTERFACE_H_

#include <unordered_map>
#include <memory>
#include "Interface.h"
#include "NetworkMap.h"
#include "CryptoIdentity.h"

class ControlInterface : public Interface {
 public:
  ControlInterface(NetworkMap&, CryptoIdentity&);
  virtual ~ControlInterface() = default;

  ControlInterface(const ControlInterface&) = delete;
  const ControlInterface& operator= (const ControlInterface&) = delete;

  bool match(const std::string&, uint8_t, const std::string&) override;
  void send(const std::string&, uint8_t, const std::string&) override;

//private:
  void operator()(ev::timer&, int); // regular maintenance
 private:
  NetworkMap& _nm;
  CryptoIdentity& _ci;

  friend class ev::timer;
  ev::timer _w;  
};

#endif // UDPINTERFACE_H_
