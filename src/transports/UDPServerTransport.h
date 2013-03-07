#ifndef UDPSERVERTRANSPORT_H_
#define UDPSERVERTRANSPORT_H_

#include "Transport.h"
#include <ev++.h>
#include <utility>
#include <memory>

#include <sys/socket.h>

class UDPTransport;

class UDPServerTransport : public Transport {
 public:
  UDPServerTransport(UDPTransport*, const std::string&,
                     const std::string&);
  virtual ~UDPServerTransport();

  void enable();
  void to_unknown(const std::string&);
 private:
  UDPTransport *_clients;
  // UDPv6 shares port space with UDPv4
  struct sockaddr* _group[2];
  socklen_t _group_length[2];
  int _fd;
  ev::io _read_watcher;
};

#endif // UDPSERVERTRANSPORT_H_
