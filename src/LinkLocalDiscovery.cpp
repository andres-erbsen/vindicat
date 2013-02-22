#include "LinkLocalDiscovery.h"
#include "Transport.h"
#include "transports/UDPClientTransport.h"

#include <sys/types.h>
#include <ifaddrs.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>

#include <cstring>

#include <algorithm>
#include <iostream>

LinkLocalDiscovery::LinkLocalDiscovery(std::vector<Transport*> &transports, const PacketHandler &phn):
    _transports(transports), _phn(phn), _fd(-1)
{
  struct ifaddrs *ifap;
  if(getifaddrs(&ifap) == -1)
  {
    std::perror("LinkLocalDiscovery::LinkLocalDiscovery");
    std::abort();
  }

  _fd = socket(AF_INET6, SOCK_DGRAM | SOCK_NONBLOCK, 0);

  {
    int reuse = 1;
    if(setsockopt(_fd, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) != 0)
    {
      std::perror("LinkLocalDiscovery::LinkLocalDiscovery");
      std::abort();
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
    std::perror("LinkLocalDiscovery::LinkLocalDiscovery");
    close(_fd);
    _fd = -1;
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
	    perror("setsockopt IPv4");
	  break;
	case AF_INET6:
	  ipv6_request.ipv6mr_interface = if_nametoindex(i->ifa_name);
	  if(setsockopt(_fd, IPPROTO_IPV6, IPV6_ADD_MEMBERSHIP, &ipv6_request, sizeof(struct ipv6_mreq)) != 0)
	    std::perror("setsockopt IPv6");
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
  struct sockaddr_in6 src;
  std::memset(&src, 0, sizeof(sockaddr_in6));
  ssize_t read = recvfrom(_fd, buf, 1500, 0, reinterpret_cast<struct sockaddr*>(&src), &srclen);
  if(read != -1)
  {
    bool exists =
    std::any_of(_transports.begin(), _transports.end(),
        [&src](const Transport *tr)
	{
	  const UDPClientTransport *client = dynamic_cast<const UDPClientTransport*>(tr);
          return client && client->address()->sa_family == AF_INET6 && std::memcmp(reinterpret_cast<const struct sockaddr_in6*>(client->address())->sin6_addr.s6_addr, src.sin6_addr.s6_addr, 16) == 0 && reinterpret_cast<const struct sockaddr_in6*>(client->address())->sin6_port == src.sin6_port;
        }
    );

    if(!exists)
    {
      char ip[40];
      std::cout << "Connecting to [" << inet_ntop(AF_INET6, src.sin6_addr.s6_addr, ip, 40) << "]:" << ntohs(src.sin6_port) << std::endl;
      UDPClientTransport *client = new UDPClientTransport(reinterpret_cast<struct sockaddr*>(&src), sizeof(struct sockaddr_in6));
      client->onPacket(_phn);
      client->enable();
      _transports.push_back(client);
    }
  }
  else
    std::perror("LinkLocalDiscovery::read_cb");
}
