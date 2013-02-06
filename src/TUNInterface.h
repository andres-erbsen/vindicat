#ifndef TUNDEVICE_H_
#define TUNDEVICE_H_

#include <string>

#include <ev++.h>

namespace IPv6
{
  class Packet;
}

class TUNInterface
{
  int _fd;
  ev::io _read_watcher;
public:
  TUNInterface(const std::string &device_hash, const std::string &dev = std::string("tun%d"));
  TUNInterface(const TUNInterface &) = delete;
  ~TUNInterface();
  TUNInterface &operator=(const TUNInterface &) = delete;
  void read_cb(ev::io &w, int revents);
  void send(const IPv6::Packet &packet);
};

#endif
