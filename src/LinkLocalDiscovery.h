#ifndef LINKLOCALDISCOVERY_H_
#define LINKLOCALDISCOVERY_H_

#include <ev++.h>

class UDPTransport;
class NetworkMap;

class LinkLocalDiscovery
{
 public:
  LinkLocalDiscovery(UDPTransport*, const NetworkMap&);
  virtual ~LinkLocalDiscovery();
  void enable();
  void read_cb(ev::io&, int);
 private:
  UDPTransport *_clients;
  const NetworkMap &_nm;
  int _fd;
  ev::io _read_watcher;
};

#endif
