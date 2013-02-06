#include "TUNInterface.h"
#include "IPv6.h"

#include <sys/socket.h>

#include <cerrno>
#include <cstring>
#include <fcntl.h>
#include <linux/if.h>
#include <linux/if_tun.h>
#include <linux/ipv6.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <system_error>
#include <unistd.h>

TUNInterface::TUNInterface(const std::string &device_hash, const std::string &dev): _fd(-1)
{
  struct ifreq ifr;
  struct in6_ifreq ifr6;
  
  if((_fd = open("/dev/net/tun", O_RDWR)) < 0)
    goto throw_error;

  memset(&ifr, 0, sizeof(ifr));

  ifr.ifr_flags = IFF_TUN | IFF_NO_PI; 
  std::strncpy(ifr.ifr_name, dev.c_str(), IFNAMSIZ);

  if(ioctl(_fd, TUNSETIFF, &ifr) < 0)
    goto device_error;

  int sockfd;
  if((sockfd = socket(AF_INET6, SOCK_DGRAM, 0)) < 0)
    goto device_error;
  if(setsockopt(sockfd, SOL_SOCKET, SO_BINDTODEVICE, ifr.ifr_name, IFNAMSIZ) < 0)
    goto socket_error;

  if(ioctl(sockfd, SIOCGIFFLAGS, &ifr) < 0)
    goto socket_error;
  ifr.ifr_flags |= IFF_UP;
  if(ioctl(sockfd, SIOCSIFFLAGS, &ifr) < 0)
    goto socket_error;

  // Set IPv6 address and netmask
  if(ioctl(sockfd, SIOCGIFINDEX, &ifr) < 0)
    goto socket_error;
  std::memset(&ifr6, 0, sizeof(struct in6_ifreq));
  ifr6.ifr6_ifindex = ifr.ifr_ifindex;
  ifr6.ifr6_addr.s6_addr[0] = 0x04;
  for(int i = 0; i < 15; i++)
    ifr6.ifr6_addr.s6_addr[i+1] = device_hash.at(i);
  ifr6.ifr6_prefixlen = 8;
  if(ioctl(sockfd, SIOCSIFADDR, &ifr6) < 0)
    goto socket_error;  
  
  close(sockfd);
  
  _read_watcher.set<TUNInterface, &TUNInterface::read_cb>(this);
  _read_watcher.start(_fd, ev::READ);
  
  return;

socket_error:
  close(sockfd);
device_error:
  close(_fd);
throw_error:
  throw std::system_error(errno, std::system_category());
}

TUNInterface::~TUNInterface()
{
  close(_fd);
}

void TUNInterface::read_cb(ev::io &w, int revents)
{
  IPv6::Packet packet = IPv6::Packet::read(_fd);
  // Now what?
}

void TUNInterface::send(const IPv6::Packet &packet)
{
  write(_fd, packet.data(), IPv6::Packet::header_length+packet.payload_length());
}
