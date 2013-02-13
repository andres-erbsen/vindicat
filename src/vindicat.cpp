#include "transports/UDPServerTransport.h"
#include "transports/UDPClientTransport.h"
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
		} else assert(0);
	}

	CryptoIdentity ci;
	auto our_bcard = ci.our_businesscard();
	auto our_device = std::make_shared<Device>();
	our_device->parseFrom( std::move(our_bcard) );

	std::unique_ptr<TUNInterface> tun = TUNInterface::open(our_device->id());
	if(!tun)
		std::cerr << "TUN interface creation failed" << std::endl;
	NetworkMap nm( std::move(our_device) );
	ConnectionPool cp;

	PacketHandler phn(nm);
	for (Transport* tr : transports) tr->onPacket(phn);
	for (Transport* tr : transports) tr->enable();	

	InterfaceHandler ihn(nm, cp);

	Beacon bcn(3,ci,transports);
	bcn.enable();

	LinkLocalDiscovery lld(transports);
	lld.enable();
	
	if(tun)
		tun->onPacket(ihn);

	ev_run (EV_DEFAULT_ 0);	
}
