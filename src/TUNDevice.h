#ifndef TUNDEVICE_H
#define TUNDEVICE_H

#include <string>

class TUNDevice
{
  int _fd;
public:
  TUNDevice(const std::string &dev = std::string("tun%d"));
  TUNDevice(const TUNDevice &) = delete;
  ~TUNDevice();
  TUNDevice &operator=(const TUNDevice &) = delete;
  int fd();
};

#endif
