#include "libvindicat.h"
#include "libvindicat.pb.h"

#include <system_error>

#include <cstring>

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

libvindicat::Connection::Connection(const std::string& server_path,
                                    const std::string& client_path) {
  if((_fd = socket(AF_UNIX, SOCK_DGRAM, 0)) == -1)
    throw std::system_error(errno, std::system_category());

  _server = new sockaddr_un;
  std::memset(_server, 0, sizeof(sockaddr_un));
  _server->sun_family = AF_UNIX;
  std::memcpy(_server->sun_path, server_path.data(),
              std::min(server_path.size(), sizeof(sockaddr_un::sun_path)));

  _client = new sockaddr_un;
  std::memset(_client, 0, sizeof(sockaddr_un));
  _client->sun_family = AF_UNIX;
  std::memcpy(_client->sun_path, client_path.data(),
              std::min(server_path.size(), sizeof(sockaddr_un::sun_path)));

  if(bind(_fd, reinterpret_cast<sockaddr*>(_client), sizeof(*_client)) == -1) {
    delete _client;
    delete _server;
    close(_fd);
    throw std::system_error(errno, std::system_category());
  }

  if(connect(_fd, reinterpret_cast<sockaddr*>(_server),
             sizeof(sockaddr_un)) == -1) {
    delete _client;
    delete _server;
    close(_fd);
    unlink(client_path.c_str());
    throw std::system_error(errno, std::system_category());
  }
}

libvindicat::Connection::~Connection() {
  for(auto socket : _sockets)
    delete socket;
  close(_fd);
  unlink(_client->sun_path);
  delete _client;
  delete _server;
}
