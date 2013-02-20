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
  virtual ~EthernetTransport() = default;
  void enable();
  void broadcast(const std::string&);
  bool send(const std::string&, const std::string&);
 private:
  pcap_t *_pcap;
  friend ev::io;
  void read_cb(ev::io&, int);
  ev::io _read_watcher;
  std::uint8_t _mac[ETH_ALEN];
  bpf_program _filter;
  static void pcap_callback(void* data, const pcap_pkthdr* pcap_header, const std::uint8_t* packet);
};

#endif // TRANSPORTS_ETHERNETTRANSPORT_H_
