#ifndef TUNDEVICE_H_
#define TUNDEVICE_H_

#include <string>

#include <ev++.h>

namespace IPv6
{
  class Packet;
}

class TUNDevice
{
  int _fd;
  ev::io _read_watcher;
public:
  TUNDevice(const std::string &device_hash, const std::string &dev = std::string("tun%d"));
  TUNDevice(const TUNDevice &) = delete;
  ~TUNDevice();
  TUNDevice &operator=(const TUNDevice &) = delete;
  int fd();
  void read_cb(ev::io &w, int revents);
  void send(const IPv6::Packet &packet);
};

#endif
