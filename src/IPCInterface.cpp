#include "IPCInterface.h"

#include <cstring>
#include <unistd.h>
#include <cstdlib>
#include <netinet/in.h>
#include "libvindicat.pb.h"

#ifndef UNIX_PATH_MAX
#define UNIX_PATH_MAX sizeof(sockaddr_un::sun_path)
#endif

#include <iostream>

IPCInterface::IPCInterface(const std::string& id)
    : _id(id) {
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

  _read_watcher.set<IPCInterface, &IPCInterface::read_cb>(this);
  _read_watcher.start(_fd, ev::READ);
  _ping_timer.set<IPCInterface, &IPCInterface::ping>(this);
  _ping_timer.start(1, 10);
}

IPCInterface::~IPCInterface() {
  close(_fd);
  unlink(_addr.sun_path);
}

bool IPCInterface::match(const std::string&, std::uint8_t next_header,
                         const std::string& data) {
  switch(next_header) {
    case IPPROTO_TCP:
      return data.size() >= 8 &&
        _tcp.count(reinterpret_cast<const std::uint16_t*>(data.data())[1]) > 0;
    case IPPROTO_UDP:
      return data.size() >= 8 &&
        _udp.count(reinterpret_cast<const std::uint16_t*>(data.data())[1]) > 0;
    default:
      return _clients.count(next_header) > 0;
  }
}

void IPCInterface::send(const std::string& from, std::uint8_t next_header,
                        const std::string& data) {
  sockaddr_un client;
  std::memset(&client, 0, sizeof(client));
  client.sun_family = AF_UNIX;
  std::uint16_t port = reinterpret_cast<const std::uint16_t*>(data.data())[1];
  libvindicat::Packet packet;
  packet.set_identifier(from);
  packet.set_next_header(next_header);
  packet.set_payload(data);
  std::string message("\x02", 1);
  packet.AppendToString(&message);

  switch(next_header) {
    case IPPROTO_TCP:
      for(auto i = _tcp.find(port); i != _tcp.end() && i->first == port; i++) {
        std::memset(client.sun_path, 0, UNIX_PATH_MAX);
        std::memcpy(client.sun_path, i->second.data(), i->second.size());
        send(client, message);
      }
      break;
    case IPPROTO_UDP:
      for(auto i = _udp.find(port); i != _udp.end() && i->first == port; i++) {
        std::memset(client.sun_path, 0, UNIX_PATH_MAX);
        std::memcpy(client.sun_path, i->second.data(), i->second.size());
        send(client, message);
      }
      break;
    default:
      for(auto i = _clients.find(next_header);
          i != _clients.end() && i->first == next_header; i++) {
        std::memset(client.sun_path, 0, UNIX_PATH_MAX);
        std::memcpy(client.sun_path, i->second.data(), i->second.size());
        send(client, message);
      }
  }
}

void IPCInterface::send(const sockaddr_un &client, const std::string& msg) {
  auto res = sendto(_fd, msg.data(), msg.size(), 0,
                    reinterpret_cast<const sockaddr*>(&client),
                    sizeof(client));

  if(res == -1)
    clear(client);
}

void IPCInterface::clear(const sockaddr_un& client) {
  std::string client_path = std::string(client.sun_path, UNIX_PATH_MAX);
  for(auto proto : {&_tcp, &_udp})
    for(auto conn = proto->begin(); conn != proto->end();)
      if(conn->second == client_path)
        conn = proto->erase(conn);
      else
        conn++;
  for(auto conn = _clients.begin(); conn != _clients.end();)
    if(conn->second == client_path)
      conn = _clients.erase(conn);
    else
      conn++;
}

void IPCInterface::read_cb(ev::io&, int) {
  sockaddr_un from;
  socklen_t length = sizeof(from);

  char *buf = new char[4096];
  auto buf_len = recvfrom(_fd, buf, 4096, 0,
                          reinterpret_cast<sockaddr*>(&from), &length);
  if(buf_len >= 1) {
    switch(buf[0]) {
      case 0x00: {  // Configuration request
        libvindicat::ConfigurationResponse response;
        response.set_local_identifier(_id);
        std::string message("\x00", 1);
        response.AppendToString(&message);
        send(from, message);
	      break;
      }
      case 0x01: {  // Forwarding request
        std::string client_path(from.sun_path, UNIX_PATH_MAX); 
        libvindicat::ForwardRequest request;
        if(!request.ParseFromArray(buf+1, buf_len-1) ||
            request.next_header() < 0 || request.next_header() > 255) {
          send(from, "\x01");
          break;
        }
        if(request.next_header() == IPPROTO_TCP &&
            (!request.has_tcp() || request.tcp().port() <= 0 ||
               request.tcp().port() > 0xFFFF)) {
          send(from, "\x01");
          break;
        }
        if(request.next_header() == IPPROTO_UDP &&
            (!request.has_udp() || request.udp().port() <= 0 ||
                   request.udp().port() > 0xFFFF )) {
	        send(from, "\x01");
	        break;
	      }
        switch(request.next_header()) {
          case IPPROTO_TCP:
            _tcp.insert(std::make_pair(request.tcp().port(), client_path));
            break;
          case IPPROTO_UDP:
            _udp.insert(std::make_pair(request.udp().port(), client_path));
            break;
          default:
            _clients.insert(std::make_pair(request.next_header(),
                                           client_path));
       	}
	      break;
      }
      case 0x02: {  // Packet
        libvindicat::Packet packet;
        if(packet.ParseFromArray(buf+1, buf_len-1)) {
          _receive_cb(std::string(packet.identifier()),
              static_cast<char>(packet.next_header())+packet.payload());
        }
        break;
      }
      case 0x03:  // Ping
        break;
      case 0x04: {  // Remove forwarding
        std::string client_path(from.sun_path, UNIX_PATH_MAX);
        libvindicat::ForwardRequest request;
        if(!request.ParseFromArray(buf+1, buf_len-1) ||
            request.next_header() > 256 ||
            (request.next_header() == IPPROTO_TCP &&
               !(request.has_tcp() && request.tcp().port() > 0 &&
                    request.tcp().port() <= 0xFFFF)) ||
            (request.next_header() == IPPROTO_UDP &&
               !(request.has_udp() && request.udp().port() > 0 &&
                    request.udp().port() <= 0xFFFF ))) {
          send(from, "\x04");
          break;
      	}
      	switch(request.next_header()) {
      	  case IPPROTO_TCP: {
	          for(auto i = _tcp.find(request.tcp().port());
                i->first == request.tcp().port() && i != _tcp.end(); i++)
      	      if(i->second == client_path) {
	              _tcp.erase(i);
            		break;
      	      }
	          break;
          } 
          case IPPROTO_UDP: {
            for(auto i = _udp.find(request.udp().port());
                i->first == request.udp().port() && i != _udp.end(); i++)
              if(i->second == client_path) {
                _udp.erase(i);
                break;
              }
            break;
          }
          default: {
            for(auto i = _clients.find(request.next_header());
                i->first == request.next_header() && i != _clients.end(); i++)
              if(i->second == client_path) {
                _clients.erase(i);
                break;
              }
          }
        }
        break;
      }
    }
  }

  delete[] buf;
}

void IPCInterface::ping(ev::timer&, int) {
  sockaddr_un client;
  std::memset(&client, 0, sizeof(client));
  client.sun_family = AF_UNIX;

  std::vector<std::string> clients;

  for(auto proto : {_tcp, _udp})
    for(auto conn : proto)
      clients.push_back(conn.second);
  for(auto conn : _clients)
    clients.push_back(conn.second);
  for(auto path : clients) {
    std::memset(client.sun_path, 0, UNIX_PATH_MAX);
    std::memcpy(client.sun_path, path.data(), path.size());
    send(client, "\x03");
  }
}
