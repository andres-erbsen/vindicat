#include "TUNInterface.h"
#include "IPv6.h"

#include <sys/socket.h>

#include <cassert>
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

std::unique_ptr<TUNInterface> TUNInterface::open(const std::string &device_hash,
    const std::string &dev)
{
  std::unique_ptr<TUNInterface> ret(new TUNInterface);
  struct ifreq ifr;
  struct in6_ifreq ifr6;
  
  if((ret->_fd = ::open("/dev/net/tun", O_RDWR)) < 0)
    goto error;

  std::memset(&ifr, 0, sizeof(ifr));

  ifr.ifr_flags = IFF_TUN | IFF_NO_PI; 
  std::strncpy(ifr.ifr_name, dev.c_str(), IFNAMSIZ);

  if(ioctl(ret->_fd, TUNSETIFF, &ifr) < 0)
    goto error;

  int sockfd;
  if((sockfd = socket(AF_INET6, SOCK_DGRAM, 0)) < 0)
    goto error;
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
  ret->_address.address[0] = ifr6.ifr6_addr.s6_addr[0] = 0x04;
  for(int i = 0; i < 15; i++)
    ret->_address.address[i+1] = ifr6.ifr6_addr.s6_addr[i+1] = device_hash.at(i);
  ifr6.ifr6_prefixlen = 8;
  if(ioctl(sockfd, SIOCSIFADDR, &ifr6) < 0)
    goto socket_error;  
  
  close(sockfd);
  
  ret->_read_watcher.set<TUNInterface, &TUNInterface::read_cb>(ret.get());
  ret->_read_watcher.start(ret->_fd, ev::READ);
  
  return ret;

socket_error:
  close(sockfd);
error:
  return std::unique_ptr<TUNInterface>();
}

TUNInterface::~TUNInterface()
{
  close(_fd);
}

void TUNInterface::read_cb(ev::io &w, int revents)
{
  IPv6::Packet packet = IPv6::Packet::read(_fd);
  
  // For now ignore foreign traffic
  if(packet.destination().address[0] != 0x04)
    return;
  
  _receive_cb(std::string(reinterpret_cast<char*>(packet.destination().address)+1,
                          15),
	      std::string(1, packet.next_header())+
	          std::string(reinterpret_cast<char*>(packet.payload()),
                              packet.payload_length()));
}

void TUNInterface::send(const IPv6::Packet &packet)
{
  write(_fd, packet.data(), IPv6::Packet::header_length+packet.payload_length());
}

void TUNInterface::send(const std::string& from_id, uint8_t protocol_number, const std::string& payload)
{
  IPv6::Address src;
  src.address[0] = 0x04;
  for(int i = 0; i < 15; i++)
    src.address[i+1] = from_id.at(i);
  send(IPv6::Packet::reassemble(src, _address, protocol_number, payload));
}

