#include "transports/UDPServerTransport.h"
#include "transports/UDPTransport.h"
#include "transports/EthernetTransport.h"
#include "PacketHandler.h"
#include "InterfaceHandler.h"
#include "TUNInterface.h"
#include "ControlInterface.h"
#include "DummyInterface.h"
#include "IPCInterface.h"
#include "Beacon.h"
#include "LinkLocalDiscovery.h"
#include "ConnectionHandler.h"
#include "Log.h"

#include <ev++.h>

#include <vector>
#include <cassert>

class ExitOnSIGINT {
public:
  ExitOnSIGINT() {
    _handler.set<ExitOnSIGINT, &ExitOnSIGINT::break_loop>(this);
    _handler.set(SIGINT);
  }

  void enable() {
    _handler.start();
  }

private:
  friend ev::sig;
  void break_loop() {
    ev::get_default_loop().break_loop();
  }

  ev::sig _handler;
};

int main (int argc, char** argv) {
  std::vector<Transport*> transports;
  UDPTransport *clients = new UDPTransport;
  for ( int i=1; i<argc; ++i ) { std::string arg(argv[i]);
    if (arg == "-s") {
      transports.push_back( new UDPServerTransport(clients, argv[i+1], argv[i+2]) );
      i += 2;
    } else if (arg == "-c") {
      clients->connect(true, argv[i+1], argv[i+2]);
      i += 2;
    } else if (arg == "-e") {
      transports.push_back( new EthernetTransport(argv[i+1]) );
      i += 1;
    } else assert(0);
  }
  transports.push_back(clients);

  CryptoIdentity ci;
  auto our_device = std::make_shared<Device>();
  our_device->parseFrom( ci.our_businesscard() );
  const std::string our_id = our_device->id();

  NetworkMap nm( std::move(our_device) );
  ConnectionPool cp;

  ConnectionHandler ch;
  InterfaceHandler ihn(ci, nm, cp, ch);
  PacketHandler phn(nm, ci, cp, ch);
  for (Transport* tr : transports) tr->onPacket(phn);
  for (Transport* tr : transports) tr->enable();  

  LinkLocalDiscovery lld(clients, nm);
  lld.enable();

  Beacon bcn(3,ci,transports);
  bcn.enable();

  auto tun = TUNInterface::open(our_id);
  if (tun) {
    auto tunnel = new TunnelInterface(nm);
    void (TUNInterface::*tun_send)(const IPv6::Packet&) = &TUNInterface::send;
    tunnel->set_send(std::bind(tun_send, tun.get(), std::placeholders::_1));
    tunnel->onPacket(ihn);
    tun->set_tunnel(std::bind(&TunnelInterface::tunnel, tunnel,
          std::placeholders::_1));
    ch.addInterface(std::unique_ptr<Interface>(tunnel));
    ci.add_capability("IPv6-Tunnel");
    ci.update_businesscard();
  }

  std::unique_ptr<Interface> ctrl(new ControlInterface(nm, ci));
  ctrl->onPacket(ihn);
  ch.addInterface( std::move(ctrl) );

  std::unique_ptr<Interface> ipc(new IPCInterface(our_id));
  ipc->onPacket(ihn);
  ch.addInterface(std::move(ipc));

  if (tun) {
    tun->onPacket(ihn);
    ch.addInterface(std::unique_ptr<Interface>(tun.release()));
  } else {
    ERROR() << "TUN interface creation failed";
    ch.addInterface( std::unique_ptr<Interface>(new DummyInterface) );
  }

  ExitOnSIGINT sigint_handler;
  sigint_handler.enable();

  ev::get_default_loop().run();

  for (Transport* tr : transports)
    delete tr;

  return 0;
}
