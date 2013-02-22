#ifndef LINKLOCALDISCOVERY_H_
#define LINKLOCALDISCOVERY_H_

#include "PacketHandler.h"

#include <ev++.h>
#include <vector>

class Transport;

class LinkLocalDiscovery
{
 public:
  LinkLocalDiscovery(std::vector<Transport*>&, const PacketHandler&);
  virtual ~LinkLocalDiscovery();
  void enable();
  void read_cb(ev::io&, int);
 private:
  std::vector<Transport*> &_transports;
  PacketHandler _phn;
  int _fd;
  ev::io _read_watcher;
};

#endif
