#include "transports/UDPServerTransport.h"
#include "transports/UDPClientTransport.h"
#include "transports/EthernetTransport.h"
#include "PacketHandler.h"
#include "InterfaceHandler.h"
#include "TUNInterface.h"
#include "DummyInterface.h"
#include "Beacon.h"
#include "LinkLocalDiscovery.h"

#include <ev++.h>

#include <vector>
#include <cassert>

int main (int argc, char** argv) {
	std::vector<Transport*> transports;
	UDPClientTransport *clients = new UDPClientTransport;
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

	std::unique_ptr<Interface> iface = TUNInterface::open(our_device->id());

	NetworkMap nm( std::move(our_device) );
	ConnectionPool cp;

	Beacon bcn(3,ci,transports);
	bcn.enable();

	if (iface) {
		InterfaceHandler ihn(ci, nm, cp, *iface);
		iface->onPacket(ihn);
	} else {
		std::cerr << "TUN interface creation failed" << std::endl;
		iface.reset(new DummyInterface);
	}

	PacketHandler phn(nm, ci, cp, iface.get());
	for (Transport* tr : transports) tr->onPacket(phn);
	for (Transport* tr : transports) tr->enable();	
	LinkLocalDiscovery lld(clients);
	lld.enable();

	ev_run (EV_DEFAULT_ 0);	
}
