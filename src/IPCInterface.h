#ifndef IPCINTERFACE_H_
#define IPCINTERFACE_H_

#include "Interface.h"
#include <string>

#include <unordered_map>
#include <sys/socket.h>
#include <sys/un.h>
#include <ev++.h>

/// Use Inter-process communication to give local applications access to
/// the network.

class IPCInterface: public Interface {
 public:
  IPCInterface(const std::string&);
  IPCInterface(IPCInterface&) = delete;
  IPCInterface& operator=(IPCInterface&) = delete;
  ~IPCInterface();
  virtual bool match(const std::string&, std::uint8_t, const std::string&);
  virtual void send(const std::string&, std::uint8_t, const std::string&);
 private:
  std::string _id;

  // Unix domain socket
  sockaddr_un _addr;
  int _fd;
  friend ev::io;
  ev::io _read_watcher;

  std::unordered_multimap<std::uint16_t, std::string> _tcp, _udp;
  std::unordered_multimap<std::uint8_t, std::string> _clients;

  void clear(const sockaddr_un&);
  void send(const sockaddr_un&, const std::string&);
  void read_cb(ev::io&, int);
};

#endif
