#include "transports/UDPServerTransport.h"
#include "transports/UDPClientTransport.h"
#include "PacketHandler.h"
#include "Beacon.h"

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

	NetworkMap nm( std::move(our_device) );
	PacketHandler hn(nm);
	for (Transport* tr : transports) tr->onPacket(hn);	
	for (Transport* tr : transports) tr->enable();	

	Beacon bcn(3,ci,transports);
	bcn.enable();

	ev_run (EV_DEFAULT_ 0);	
}
