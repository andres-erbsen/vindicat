#ifndef LINKLOCALDISCOVERY_H_
#define LINKLOCALDISCOVERY_H_

#include <ev++.h>

class UDPClientTransport;
class NetworkMap;

class LinkLocalDiscovery
{
 public:
  LinkLocalDiscovery(UDPClientTransport*, const NetworkMap&);
  virtual ~LinkLocalDiscovery();
  void enable();
  void read_cb(ev::io&, int);
 private:
  UDPClientTransport *_clients;
  const NetworkMap &_nm;
  int _fd;
  ev::io _read_watcher;
};

#endif
