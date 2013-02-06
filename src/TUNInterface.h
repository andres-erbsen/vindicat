#ifndef TUNDEVICE_H_
#define TUNDEVICE_H_

#include "Interface.h"
#include <ev++.h>
#include <string>

namespace IPv6
{
  class Packet;
}

class TUNInterface : public Interface
{
public:
  TUNInterface(const std::string &device_hash, const std::string &dev = std::string("tun%d"));
  TUNInterface(const TUNInterface &) = delete;
  ~TUNInterface();
  TUNInterface &operator=(const TUNInterface &) = delete;
  void send(const IPv6::Packet &packet);
  void send(const std::string &from_id, const std::string& packet);
private:
  friend ev::io;
  ev::io _read_watcher;
  int _fd;
  void read_cb(ev::io &w, int revents);
};

#endif
