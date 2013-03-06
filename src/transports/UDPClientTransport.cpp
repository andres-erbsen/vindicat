#include "UDPClientTransport.h"

#include <cassert>
#include <cerrno>
#include <cstring>

#include <iostream>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>

#define MAX_MISSED_BEACONS 5

std::string uid_format(sockaddr *addr, socklen_t len) {
  std::string ret;
  if(addr->sa_family == AF_INET) {
    sockaddr_in *addr_in = reinterpret_cast<sockaddr_in*>(addr);
    ret.reserve(4+4+2);
    ret  = "IPv4";
    ret += std::string(reinterpret_cast<char*>(&addr_in->sin_addr.s_addr), 4);
    ret += std::string(reinterpret_cast<char*>(&addr_in->sin_port), 2);
  } else if(addr->sa_family == AF_INET6) {
    sockaddr_in6 *addr_in6 = reinterpret_cast<sockaddr_in6*>(addr);
    ret.reserve(4+16+2);
    ret  = "IPv6";
    ret += std::string(reinterpret_cast<char*>(addr_in6->sin6_addr.s6_addr), 16);
    ret += std::string(reinterpret_cast<char*>(&addr_in6->sin6_port), 2);
  } else {
    ret = "DGRAM"+std::string(reinterpret_cast<char*>(addr), len);
  }
  return ret;
}

UDPClient::UDPClient(int fd, const std::string& host, const std::string& port):
    _fd(fd) {
  struct addrinfo hints, *res;
  std::memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_DGRAM;
  int err;
  if((err=getaddrinfo(host.c_str(), port.c_str(), &hints, &res)) != 0) {
    if(err == EAI_SYSTEM)
      std::perror("UDPClientTransport::UDPClientTransport");
    else
      std::cerr << "UDPClientTransport::UDPClientTransport: " << gai_strerror(err) << std::endl;
    std::abort();
  }

  _addr = std::shared_ptr<sockaddr>(
      reinterpret_cast<sockaddr*>(new struct sockaddr_storage));
  std::memcpy(_addr.get(), res->ai_addr, res->ai_addrlen);
  _len = res->ai_addrlen;

  freeaddrinfo(res);
}

UDPClient::UDPClient(int fd, const std::shared_ptr<sockaddr>& addr, socklen_t len):
    _addr(std::move(addr)), _len(len), _fd(fd) {
}

bool UDPClient::send(const std::string &payload) const {
  return sendto(_fd, payload.c_str(), payload.size(), 0, _addr.get(), _len) != -1;
}

std::hash<std::string>::result_type UDPClient::hash() const {
  return std::hash<std::string>()(uid_format(_addr.get(), _len));
}

bool UDPClient::operator==(const UDPClient &client) const {
  return uid_format(_addr.get(), _len) == uid_format(client._addr.get(), client._len);
}

UDPClientTransport::UDPClientTransport():
    _fd(socket(AF_INET6, SOCK_DGRAM | SOCK_NONBLOCK, 0)) {
  if(_fd == -1) {
    std::perror("socket");
    std::abort();
  }
}

void UDPClientTransport::connect(bool persistent, const std::string& host,
                                 const std::string& port) {
  return connect(persistent, host, port, _fd);
}

void UDPClientTransport::connect(bool persistent, const std::string& host,
                                 const std::string& port, int fd) {
  _unknown[UDPClient(fd, host, port)] = persistent? -1: 0;
}

void UDPClientTransport::connect(bool persistent,
                                 const std::shared_ptr<sockaddr>& addr,
                                 socklen_t len) {
  return connect(persistent, std::move(addr), len, _fd);
}

void UDPClientTransport::connect(bool persistent,
                                 const std::shared_ptr<sockaddr>& addr,
				 socklen_t len,
                                 int fd) {
  _unknown[UDPClient(fd, std::move(addr), len)] = persistent? -1: 0;
}

UDPClientTransport::~UDPClientTransport() {
  close(_fd);
}

void UDPClientTransport::enable() {
  enable(_read_watcher, _fd);
}

void UDPClientTransport::enable(ev::io& watcher, int fd) {
  watcher.set<UDPClientTransport, &UDPClientTransport::read_cb>(this);
  watcher.start(fd, ev::READ);
}

void UDPClientTransport::to_unknown(const std::string& payload) {
  for(auto c = _unknown.begin(); c != _unknown.end();) {
    c->first.send(payload);
    if(c->second != -1 && ++(c->second) > MAX_MISSED_BEACONS) {
      c = _unknown.erase(c);
      continue;
    }
    ++c;
  }
}

void UDPClientTransport::read_cb(ev::io& w, int /*revents*/) {
  // Callback for libev loop
  // Reads data and gives it to handler
  char *buf = new char[1500]; // ethernet MTU size
  sockaddr *addr = reinterpret_cast<sockaddr*>(new sockaddr_storage);
  socklen_t len = sizeof(sockaddr_storage);
  ssize_t read = recvfrom(w.fd, buf, 1500, 0, addr, &len);
  if(read == -1 && errno != ECONNREFUSED) {
    std::perror("UDPClientTransport::read_cb: recvfrom:");
    std::abort();
  }

  UDPClient client(w.fd, std::shared_ptr<sockaddr>(addr), len);
  if(_unknown[client] != -1)
    _unknown.erase(client);

  _receive_cb(TransportSocket(std::bind(std::mem_fn(&UDPClient::send),
                                        client, std::placeholders::_1),
                              uid_format(addr, len)),
              std::string(buf, read));

  delete[] buf;
}

std::size_t UDPClientTransport::nonpersistent() const
{
  std::size_t ret = 0;
  for(auto client : _unknown)
    if(!client.second)
      ret++;
  return ret;
}
