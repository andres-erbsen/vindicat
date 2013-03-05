#ifndef LINKLOCALDISCOVERY_H_
#define LINKLOCALDISCOVERY_H_

#include "PacketHandler.h"
#include <ev++.h>

class UDPClientTransport;

class LinkLocalDiscovery
{
 public:
  LinkLocalDiscovery(UDPClientTransport*, const PacketHandler&);
  virtual ~LinkLocalDiscovery();
  void enable();
  void read_cb(ev::io&, int);
 private:
  UDPClientTransport *_clients;
  const PacketHandler &_phn;
  int _fd;
  ev::io _read_watcher;
};

#endif
