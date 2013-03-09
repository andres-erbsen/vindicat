#include "IPCInterface.h"

IPCInterface::IPCInterface(const std::string& id)
    _ip(('\x04'+id).substr(0, 16)) {
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
      return _clients.count(from) > 0;
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
      break;
    case IPPROTO_UDP:
      std::memcpy(client.sun_path, _udp[from][port].data(),
                  _udp[from][port].size());
      break;
    default:
      std::memcpy(client.sun_path, _clients[from].data(),
                  _clients[from].size());
  }
  auto res = sendto(_fd, data.data(), data.size(), 0, &client, sizeof(client));
  if(res == -1)
    clear(client);
}

void clear(const sockaddr_un& client) {
  std::string client_path = std::string(client.sun_path, UNIX_PATH_MAX);
  for(auto proto : {_tcp, _udp})
    for(auto server = proto.begin(); server != proto.end();) {
      for(auto conn = server->begin(); conn != server->end();) {
        if(conn->second == client_path) {
          conn = server->erase(conn);
	  continue;
        }
        conn++;
      }
      if(server->empty()) {
        server = proto->erase(server);
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
