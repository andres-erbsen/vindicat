#ifndef FORWARDING_H_
#define FORWARDING_H_

#include "Device.h"
#include "Transport.h"
#include "NetworkMap.h"
#include <lemon/list_graph.h>

#include <stdint.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <assert.h>

class Device;

class Forwarding {
public:
  static void pair( std::shared_ptr<Forwarding>
                  , std::shared_ptr<Forwarding> );

    Forwarding(uint64_t);
    Forwarding(const Forwarding&) = delete;
    Forwarding& operator=(const Forwarding&) = delete;
  virtual ~Forwarding(); // remove the other in this pair

  uint64_t id();

  // Forwardings come in pairs...
  virtual void detatch() = 0;
  // remove registrered pointer to this forwarding
  virtual bool forward_out(const std::string&) = 0;
  // called by other forwarding in the pair to send the packet out
  virtual bool forward(const std::string&);
    // calls forward_out of the other forwarding in the pair

protected:
  std::weak_ptr<Forwarding> _pair_other;
  uint64_t _id;
};

class ForeignForwarding : public Forwarding {
public:
  ForeignForwarding(NetworkMap&, uint64_t);
  void owner(std::weak_ptr<Device>&&);
    void detatch();
protected:
    NetworkMap& _nm;
    std::weak_ptr<Device> _owner;
};

class SimpleForwarding : public ForeignForwarding {
public:
    SimpleForwarding(NetworkMap&, uint64_t);
    bool forward_out(const std::string&);
};

class NoForwarding : public ForeignForwarding {
public:
    bool forward_out(const std::string&);
};

#endif // FORWARDING_H_
