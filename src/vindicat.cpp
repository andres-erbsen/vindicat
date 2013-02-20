#include "transports/UDPServerTransport.h"
#include "transports/UDPClientTransport.h"
#include "transports/EthernetTransport.h"
#include "PacketHandler.h"
#include "InterfaceHandler.h"
#include "TUNInterface.h"
#include "Beacon.h"
#include "LinkLocalDiscovery.h"

#include <ev++.h>

#include <vector>
#include <cassert>

int main (int argc, char** argv) {
	std::vector<Transport*> transports;
	for ( int i=1; i<argc; ++i ) { std::string arg(argv[i]);
		if (arg == "-s") {
			transports.push_back( new UDPServerTransport(argv[i+1], argv[i+2]) );
			i += 2;
		} else if (arg == "-c") {
			transports.push_back( new UDPClientTransport(argv[i+1], argv[i+2]) );
			i += 2;
		} else if (arg == "-e") {
			transports.push_back( new EthernetTransport(argv[i+1]) );
			i += 1;
		} else assert(0);
	}

	CryptoIdentity ci;
	auto our_bcard = ci.our_businesscard();
	auto our_device = std::make_shared<Device>();
	our_device->parseFrom( std::move(our_bcard) );

	std::unique_ptr<TUNInterface> tun = TUNInterface::open(our_device->id());

	NetworkMap nm( std::move(our_device) );
	ConnectionPool cp;

	PacketHandler phn(nm, ci);
	for (Transport* tr : transports) tr->onPacket(phn);
	for (Transport* tr : transports) tr->enable();	

	Beacon bcn(3,ci,transports);
	bcn.enable();

	LinkLocalDiscovery lld(transports, phn);
	lld.enable();
	
	if (tun) {
		InterfaceHandler ihn(ci, nm, cp, *tun);
		tun->onPacket(ihn);
	} else {
		std::cerr << "TUN interface creation failed" << std::endl;
	}

	ev_run (EV_DEFAULT_ 0);	
}
