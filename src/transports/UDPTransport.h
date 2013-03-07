#ifndef UDPCLIENTTRANSPORT_H_
#define UDPCLIENTTRANSPORT_H_

#include "Transport.h"
#include <ev++.h>

#include <sys/socket.h>

#include <unordered_map>

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
  template<> struct hash<std::pair<UDPClient, bool>> {
    hash<string>::result_type operator()(const std::pair<UDPClient, bool>& client) const {
      return client.first.hash();
    }
  };
}

class UDPTransport : public Transport {
 public:
  UDPTransport();
  void connect(bool, const std::string& host, const std::string& port);
  void connect(bool, const std::string& host, const std::string& port, int fd);
  void connect(bool, const std::shared_ptr<sockaddr>& addr, socklen_t len);
  void connect(bool, const std::shared_ptr<sockaddr>& addr, socklen_t len, int fd);
  virtual ~UDPTransport();
  void enable();
  void enable(ev::io&, int);
  void to_unknown(const std::string&);
  std::size_t nonpersistent() const;
 private:
  void read_cb(ev::io&, int);
  friend ev::io;
  int _fd;
  ev::io _read_watcher;
  /// Clients that are known to exist but we haven't seen yet.
  std::unordered_map<UDPClient, int> _unknown;
};

std::string uid_format(sockaddr*, socklen_t);

#endif // UDPCLIENTTRANSPORT_H_
