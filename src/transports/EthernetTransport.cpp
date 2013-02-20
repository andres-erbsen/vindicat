#include "EthernetTransport.h"
#include <cstdlib>
#include <pcap/pcap.h>
#include <iostream>
#include <cassert>
#include <cstring>
#include <unistd.h>
#include <sys/ioctl.h>
#include <net/if.h>

EthernetTransport::EthernetTransport(const std::string& d)
{
  char errbuf[PCAP_ERRBUF_SIZE];
  const char *device;
  if(d.empty())
    device = pcap_lookupdev(errbuf);
  else
    device = d.c_str();
  if(!device)
  {
    std::cerr << "pcap_lookupdev: " << errbuf << std::endl;
    std::abort();
  }
  errbuf[0] = 0;
  _pcap = pcap_open_live(device, (1<<16)-1, false, -1, errbuf);
  if(!_pcap)
  {
    std::cerr << "pcap_open_live: " << errbuf << std::endl;
    std::abort();
  }
  if(errbuf[0] != 0)
    std::clog << "pcap_open_live: " << errbuf << std::endl;
  if(pcap_compile(_pcap, &_filter, "ether proto 0xDCDC", false, PCAP_NETMASK_UNKNOWN) == -1)
  {
    pcap_perror(_pcap, "pcap_compile");
    std::abort();
  }
  if(pcap_setfilter(_pcap, &_filter) == -1)
  {
    pcap_perror(_pcap, "pcap_setfilter");
    std::abort();
  }

  ifreq ifr;
  int sock = socket(PF_INET, SOCK_DGRAM, 0);
  if(sock == -1)
  {
    std::perror("socket");
    std::abort();
  }
  std::memset(&ifr, 0, sizeof(ifreq));
  std::strcpy(ifr.ifr_name, device);
  if(ioctl(sock, SIOCGIFHWADDR, &ifr) == -1)
  {
    std::perror("ioctl");
    std::abort();
  }
  close(sock);
  std::memcpy(_mac, ifr.ifr_hwaddr.sa_data, ETH_ALEN);
}

void EthernetTransport::enable()
{
  _read_watcher.set<EthernetTransport, &EthernetTransport::read_cb>(this);
  int fd = pcap_get_selectable_fd(_pcap);
  // pcap_get_selectable_fd() returns, on UNIX, a file descriptor number for a
  // file descriptor on which one can do a select() or poll() to wait for it to
  // be possible to read packets without blocking, if such a descriptor exists,
  // or -1, if no such descriptor exists. pcap_get_selectable_fd() is not
  // available on Windows.
  assert(fd != -1);
  _read_watcher.start(fd, ev::READ);
}

void EthernetTransport::broadcast(const std::string& msg)
{
  send(msg, std::string(ETH_ALEN, 0xff));
}

bool EthernetTransport::send(const std::string& msg, const std::string& mac)
{
  std::uint8_t *buf = new std::uint8_t[msg.size()+sizeof(ether_header)];
  ether_header *header = reinterpret_cast<ether_header*>(buf);
  std::memcpy(header->ether_shost, _mac, ETH_ALEN);
  std::memcpy(header->ether_dhost, mac.c_str(), ETH_ALEN);
  header->ether_type = 0xDCDC;
  std::memcpy(buf+sizeof(ether_header), msg.c_str(), msg.size());
  int res = pcap_sendpacket(_pcap, buf, msg.size()+sizeof(ether_header));
  delete[] buf;
  return res == 0;
}

void EthernetTransport::read_cb(ev::io &watcher, int revents)
{
  const std::uint8_t *packet;
  pcap_pkthdr *header;
  int res = pcap_next_ex(_pcap, &header, &packet);
  if(res == -1)
    pcap_perror(_pcap, "pcap_next_ex");
  else if(res != 1)
    return;
  const ether_header *eth = reinterpret_cast<const ether_header*>(packet);
  _receive_cb(
      TransportSocket(
          std::bind(std::mem_fn(&EthernetTransport::send),
                    this, std::placeholders::_1,
                    std::string(reinterpret_cast<const char*>(eth->ether_shost),
                                ETH_ALEN)),
	  "ETHER"+std::string(reinterpret_cast<const char*>(eth->ether_shost),
                              ETH_ALEN)),
      std::string(reinterpret_cast<const char*>(packet+sizeof(ether_header)),
                  header->caplen-sizeof(ether_header)));
}
