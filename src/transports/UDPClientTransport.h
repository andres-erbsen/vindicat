#ifndef UDPCLIENTTRANSPORT_H_
#define UDPCLIENTTRANSPORT_H_

#include "Transport.h"
#include <ev++.h>

#include <sys/socket.h>

#include <unordered_set>

class UDPClient {
 public:
  UDPClient(int fd, const std::string& host, const std::string& port);
  UDPClient(int fd, const std::shared_ptr<sockaddr>& addr, socklen_t len);
  bool send(const std::string&) const;
  std::hash<std::string>::result_type hash() const;
  bool operator==(const UDPClient&) const;
 private:
  std::shared_ptr<sockaddr> _addr;
  socklen_t _len;
  int _fd;
};

namespace std {
  template<> struct hash<UDPClient> {
    hash<string>::result_type operator()(const UDPClient& client) const {
      return client.hash();
    }
  };
}

class UDPClientTransport : public Transport {
 public:
  UDPClientTransport();
  void connect(const std::string& host, const std::string& port);
  void connect(const std::string& host, const std::string& port, int fd);
  void connect(const std::shared_ptr<sockaddr>& addr, socklen_t len);
  void connect(const std::shared_ptr<sockaddr>& addr, socklen_t len, int fd);
  virtual ~UDPClientTransport();
  void enable();
  void enable(ev::io&);
  void broadcast(const std::string&);
 private:
  void read_cb(ev::io&, int);
  friend ev::io;
  int _fd;
  ev::io _read_watcher;
  /// Clients that are known to exist but we haven't seen yet.
  std::unordered_set<UDPClient> _unknown;
};


#endif // UDPCLIENTTRANSPORT_H_
