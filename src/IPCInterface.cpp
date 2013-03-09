#include "IPCInterface.h"

#include <cstring>
#include <unistd.h>
#include <cstdlib>
#include <netinet/in.h>

#ifndef UNIX_PATH_MAX
#define UNIX_PATH_MAX sizeof(sockaddr_un::sun_path)
#endif

IPCInterface::IPCInterface(const std::string& id)
    : _ip(('\x04'+id).substr(0, 16)) {
  if((_fd = socket(AF_UNIX, SOCK_DGRAM, 0)) == -1) {
    std::perror("socket");
    std::abort();
  }
  std::memset(&_addr, 0, sizeof(_addr));
  _addr.sun_family = AF_UNIX;
  std::snprintf(_addr.sun_path, UNIX_PATH_MAX, "/tmp/vindicat.%d", getpid());
  if(bind(_fd, reinterpret_cast<sockaddr*>(&_addr), sizeof(_addr)) == -1) {
    std::perror("bind");
    std::abort();
  }
}

IPCInterface::~IPCInterface() {
  close(_fd);
  unlink(_addr.sun_path);
}

bool IPCInterface::match(const std::string& from, std::uint8_t next_header,
                         const std::string& data) {
  std::uint16_t port = reinterpret_cast<const std::uint16_t*>(data.data())[1];
  switch(next_header) {
    case IPPROTO_TCP:
      return _tcp.count(from) > 0 && _tcp[from].count(port) > 0;
    case IPPROTO_UDP:
      return _udp.count(from) > 0 && _udp[from].count(port) > 0;
    default:
      if(_clients.count(from) > 0) {
        auto range = _clients.equal_range(from);
	range.second++;
        for(auto i = range.first; i != range.second; i++)
	  if(i->first == from)
	    return true;
      }
      return false;
  }
}

void IPCInterface::send(const std::string& from, std::uint8_t next_header,
                        const std::string& data) {
  sockaddr_un client;
  std::memset(&client, 0, sizeof(client));
  client.sun_family = AF_UNIX;
  std::uint16_t port = reinterpret_cast<const std::uint16_t*>(data.data())[1];
  switch(next_header) {
    case IPPROTO_TCP:
      std::memcpy(client.sun_path, _tcp[from][port].data(),
                  _tcp[from][port].size());
      send(client, from, next_header, data);
      return;
    case IPPROTO_UDP:
      std::memcpy(client.sun_path, _udp[from][port].data(),
                  _udp[from][port].size());
      send(client, from, next_header, data);
      return;
    default:
      auto range = _clients.equal_range(from);
      range.second++;
      for(auto i = range.first; i != range.second; i++) {
        std::memset(client.sun_path, 0, UNIX_PATH_MAX);
        std::memcpy(client.sun_path, i->second.data(),
                    i->second.size());
	send(client, from, next_header, data);
      }
  }
}
void IPCInterface::send(const sockaddr_un &client, const std::string& from,
                        std::uint8_t next_header, const std::string& data) {
  auto res = sendto(_fd, data.data(), data.size(), 0,
                    reinterpret_cast<const sockaddr*>(&client),
                    sizeof(client));
  if(res == -1)
    clear(client);
}

void IPCInterface::clear(const sockaddr_un& client) {
  std::string client_path = std::string(client.sun_path, UNIX_PATH_MAX);
  for(auto proto : {_tcp, _udp})
    for(auto server = proto.begin(); server != proto.end();) {
      for(auto conn = server->second.begin(); conn != server->second.end();) {
        if(conn->second == client_path) {
          conn = server->second.erase(conn);
	  continue;
        }
        conn++;
      }
      if(server->second.empty()) {
        server = proto.erase(server);
	continue;
      }
      server++;
    }
  for(auto conn = _clients.begin(); conn != _clients.end();) {
    if(conn->second == client_path) {
      conn = _clients.erase(conn);
      continue;
    }
    conn++;
  }
}
