#include "LinkLocalDiscovery.h"
#include "NetworkMap.h"
#include "transports/UDPTransport.h"
#include "Log.h"

#include <sys/types.h>
#include <ifaddrs.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>

#include <cstring>

#include <algorithm>
#include <iostream>

LinkLocalDiscovery::LinkLocalDiscovery(UDPTransport* clients,
                                       const NetworkMap &nm):
    _clients(clients), _nm(nm), _fd(socket(AF_INET6, SOCK_DGRAM, 0))
{
  if(_fd == -1)
  {
    ERROR().perror("socket");
    return;
  }

  struct ifaddrs *ifap;
  if(getifaddrs(&ifap) == -1)
  {
    ERROR().perror("getifaddrs");
    close(_fd);
    _fd = -1;
    return;
  }

  {
    int reuse = 1;
    if(setsockopt(_fd, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) != 0)
    {
      ERROR().perror("setsockopt");
      close(_fd);
      _fd = -1;
      return;
    }
  }

  struct sockaddr_in6 local_ipv6;
  std::memset(&local_ipv6, 0, sizeof(struct sockaddr_in6));
  local_ipv6.sin6_family = AF_INET6;
  local_ipv6.sin6_port = htons(30307);
  local_ipv6.sin6_addr = IN6ADDR_ANY_INIT;
  if(bind(_fd, reinterpret_cast<struct sockaddr*>(&local_ipv6),
          sizeof(struct sockaddr_in6)) == -1)
  {
    ERROR().perror("bind");
    close(_fd);
    _fd = -1;
    return;
  }

  struct ipv6_mreq ipv6_request;
  struct ip_mreqn ipv4_request;
  std::memset(&ipv6_request, 0, sizeof(struct ipv6_mreq));
  std::memset(&ipv4_request, 0, sizeof(struct ip_mreqn));
  inet_pton(AF_INET6, "ff02::dc", ipv6_request.ipv6mr_multiaddr.s6_addr);
  ipv4_request.imr_multiaddr.s_addr = inet_addr("224.0.0.254");

  for(struct ifaddrs *i = ifap; i != nullptr; i = i->ifa_next)
  {
    if(i->ifa_flags & IFF_UP && i->ifa_addr)
    {
      switch(i->ifa_addr->sa_family)
      {
        case AF_INET:
    ipv4_request.imr_ifindex = if_nametoindex(i->ifa_name);
    ipv4_request.imr_address = reinterpret_cast<struct sockaddr_in*>(i->ifa_addr)->sin_addr;
    if(setsockopt(_fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, &ipv4_request, sizeof(struct ip_mreqn)) != 0)
      ERROR().perror("setsockopt for IPv4");
    break;
  case AF_INET6:
    ipv6_request.ipv6mr_interface = if_nametoindex(i->ifa_name);
    if(setsockopt(_fd, IPPROTO_IPV6, IPV6_ADD_MEMBERSHIP, &ipv6_request, sizeof(struct ipv6_mreq)) != 0)
      ERROR().perror("setsockopt for IPv6");
    break;
      }
    }
  }

  freeifaddrs(ifap);
}

LinkLocalDiscovery::~LinkLocalDiscovery()
{
  close(_fd);
}

void LinkLocalDiscovery::enable()
{
  _read_watcher.set<LinkLocalDiscovery, &LinkLocalDiscovery::read_cb>(this);
  if(_fd != -1)
    _read_watcher.start(_fd, ev::READ);
}

void LinkLocalDiscovery::read_cb(ev::io& /*w*/, int /*revents*/)
{
  char buf[1500];
  socklen_t srclen = sizeof(struct sockaddr_in6);
  struct sockaddr_in6 *src = new sockaddr_in6;
  std::memset(src, 0, sizeof(sockaddr_in6));
  ssize_t read = recvfrom(_fd, buf, 1500, 0, reinterpret_cast<sockaddr*>(src), &srclen);
  // Set an arbitrary limit on nonexistent servers
  if(read != -1) {
    if( _clients->nonpersistent() < 128) {
      auto device = _nm.device(
          TransportSocket([](const std::string&){return false;},
                          uid_format(reinterpret_cast<sockaddr*>(src), srclen)));
      if(!device) {
        char ip[40];
        INFO() << "Connecting to ["
               << inet_ntop(AF_INET6, src->sin6_addr.s6_addr, ip, 40) << "]:"
               << ntohs(src->sin6_port);
        _clients->connect(false,
            std::shared_ptr<sockaddr>(reinterpret_cast<sockaddr*>(src)), srclen);
        return;
      }
    }
  } else {
    ERROR().perror("recvfrom");
  }
  delete src;
}
