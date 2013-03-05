#include "EthernetTransport.h"
#include <cstdlib>
#include <pcap/pcap.h>
#include <iostream>
#include <cassert>
#include <cstring>
#include <unistd.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <functional>

EthernetTransport::EthernetTransport(const std::string& d): _fd{-1, -1}
{
  char errbuf[PCAP_ERRBUF_SIZE];
  const char *device;
  if(d.empty())
    device = pcap_lookupdev(errbuf);
  else
    device = d.c_str();
  if(device == nullptr)
  {
    std::cerr << "pcap_lookupdev: " << errbuf << std::endl;
    std::abort();
  }
  errbuf[0] = 0;
  _pcap = pcap_open_live(device, (1<<16)-1, false, -1, errbuf);
  if(_pcap == nullptr)
  {
    std::cerr << "pcap_open_live: " << errbuf << std::endl;
    std::abort();
  }
  if(errbuf[0] != 0)
    std::clog << "pcap_open_live: " << errbuf << std::endl;

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

  char buf[100];
  std::sprintf(buf, "ether proto 0xDCDC and (ether host %02x:%02x:%02x:%02x:"
      "%02x:%02x or ether host ff:ff:ff:ff:ff:ff)",
      _mac[0], _mac[1], _mac[2], _mac[3], _mac[4], _mac[5]);

  if(pcap_compile(_pcap, &_filter, buf, false, PCAP_NETMASK_UNKNOWN) == -1)
  {
    pcap_perror(_pcap, "pcap_compile");
    std::abort();
  }
  if(pcap_setfilter(_pcap, &_filter) == -1)
  {
    pcap_perror(_pcap, "pcap_setfilter");
    std::abort();
  }

  _fd[0] = pcap_get_selectable_fd(_pcap);
  if(_fd[0] == -1 && pipe(_fd) == -1)
  {
    std::perror("pipe");
    std::abort();
  }
}

EthernetTransport::~EthernetTransport()
{
  pcap_breakloop(_pcap);
  if(_pcap_loop_thread.joinable())
    _pcap_loop_thread.join();
  pcap_close(_pcap);
  if(_fd[1] != -1)
  {
    close(_fd[0]);
    close(_fd[1]);
  }
}

void EthernetTransport::enable()
{
  _read_watcher.set<EthernetTransport, &EthernetTransport::read_cb>(this);
  _read_watcher.start(_fd[0], ev::READ);
  if(_fd[1] != -1)
    _pcap_loop_thread = std::thread(&pcap_loop, _pcap, -1,
                                    &EthernetTransport::pcap_callback,
                                    reinterpret_cast<std::uint8_t*>(_fd));
}

void EthernetTransport::pcap_callback(std::uint8_t *data,
                                      const pcap_pkthdr* pcap_header,
                                      const std::uint8_t* packet)
{
  int *fd = reinterpret_cast<int*>(data);
  write(fd[1], pcap_header, sizeof(pcap_pkthdr));
  write(fd[1], packet, pcap_header->caplen);
}

void EthernetTransport::to_unknown(const std::string& msg)
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

void EthernetTransport::read_cb(ev::io& /*watcher*/, int /*revents*/)
{
  const std::uint8_t *packet;
  pcap_pkthdr *header;
  if(_fd[1] == -1)
  {
    int res = pcap_next_ex(_pcap, &header, &packet);
    if(res == -1)
      pcap_perror(_pcap, "pcap_next_ex");
    if(res != 1)
      return;
  }
  else
  {
    header = new pcap_pkthdr;
    read(_fd[0], header, sizeof(pcap_pkthdr));
    packet = new std::uint8_t[header->caplen];
    read(_fd[0], const_cast<std::uint8_t*>(packet), header->caplen);
  }

  const ether_header *eth = reinterpret_cast<const ether_header*>(packet);
  const std::string from_mac( reinterpret_cast<const char*>(eth->ether_shost)
                            , ETH_ALEN );
  std::string payload(reinterpret_cast<const char*>(packet+sizeof(ether_header))
                     ,header->caplen-sizeof(ether_header));
  _receive_cb( TransportSocket( std::bind( std::mem_fn(&EthernetTransport::send)
                                         , this
                                         , std::placeholders::_1
                                         , from_mac )
                              , "ETHER"+from_mac )
             , std::move(payload) );

  if(_fd[1] != -1)
  {
    delete header;
    delete[] packet;
  }
}
