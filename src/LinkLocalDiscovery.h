#ifndef LINKLOCALDISCOVERY_H_
#define LINKLOCALDISCOVERY_H_

#include <ev++.h>
#include <vector>

class Transport;

class LinkLocalDiscovery
{
 public:
  LinkLocalDiscovery(std::vector<Transport*> &transports);
  virtual ~LinkLocalDiscovery();
  void enable();
  void read_cb(ev::io &w, int revents);
 private:
  std::vector<Transport*> &_transports;
  int _fd;
  ev::io _read_watcher;
};

#endif
