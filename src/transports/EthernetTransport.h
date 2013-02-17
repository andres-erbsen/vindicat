#ifndef TRANSPORTS_ETHERNETTRANSPORT_H_
#define TRANSPORTS_ETHERNETTRANSPORT_H_

#include "Transport.h"

#include <netinet/if_ether.h>
#include <ev++.h>
#include <cstdint>
#include <string>
#include <pcap/pcap.h>

class EthernetTransport: public Transport
{
 public:
  EthernetTransport(const std::string& device = std::string());
  virtual ~EthernetTransport();
  void onPacket(packet_callback);
  void enable();
  void broadcast(const std::string&);
  void read_cb(ev::io&, int);
  bool send(const std::string&, const std::string&);
 private:
  pcap_t *_pcap;
  ev::io _read_watcher;
  packet_callback _handler;
  std::uint8_t _mac[ETH_ALEN];
  bpf_program _filter;
  static void pcap_callback(void* data, const pcap_pkthdr* pcap_header, const std::uint8_t* packet);
};

#endif // TRANSPORTS_ETHERNETTRANSPORT_H_
