#ifndef UDPINTERFACE_H_
#define UDPINTERFACE_H_

#include <unordered_map>
#include <memory>
#include "Interface.h"

class ControlInterface : public Interface {
 public:
  ControlInterface() = default;
  virtual ~ControlInterface() = default;

  ControlInterface(const ControlInterface&) = delete;
  const ControlInterface& operator= (const ControlInterface&) = delete;

  bool match(const std::string&, uint8_t, const std::string&) override;
  void send(const std::string&, uint8_t, const std::string&) override;
};

#endif // UDPINTERFACE_H_
