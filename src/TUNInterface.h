#ifndef TUNDEVICE_H_
#define TUNDEVICE_H_

#include "Interface.h"
#include "IPv6.h"
#include <ev++.h>
#include <string>
#include <memory>

class TUNInterface : public Interface
{
public:
  static std::unique_ptr<TUNInterface> open(const std::string &device_hash,
      const std::string &dev = std::string("tun%d"));
  TUNInterface(const TUNInterface &) = delete;
  ~TUNInterface();
  TUNInterface &operator=(const TUNInterface &) = delete;
  void send(const IPv6::Packet &packet);
  void send(const std::string&, uint8_t, const std::string&);
private:
  TUNInterface() = default;
  friend ev::io;
  ev::io _read_watcher;
  int _fd;
  void read_cb(ev::io&, int);
  IPv6::Address _address;
};

#endif
