#ifndef TRANSPORTS_ETHERNETTRANSPORT_H_
#define TRANSPORTS_ETHERNETTRANSPORT_H_

#include "Transport.h"

#include <netinet/if_ether.h>
#include <ev++.h>
#include <cstdint>
#include <string>
#include <pcap/pcap.h>
#include <thread>

class EthernetTransport: public Transport
{
 public:
  EthernetTransport(const std::string& device = std::string());
  virtual ~EthernetTransport();
  void enable();
  void broadcast(const std::string&);
  bool send(const std::string&, const std::string&);
 private:
  pcap_t *_pcap;
  // If _fd[1] is -1, then _fd[0] is managed by pcap
  int _fd[2];
  friend ev::io;
  void read_cb(ev::io&, int);
  ev::io _read_watcher;
  std::uint8_t _mac[ETH_ALEN];
  bpf_program _filter;
  void pcap_loop_thread();
  std::thread _pcap_loop_thread;
  static void pcap_callback(std::uint8_t*, const pcap_pkthdr*, const std::uint8_t*);
};

#endif // TRANSPORTS_ETHERNETTRANSPORT_H_
