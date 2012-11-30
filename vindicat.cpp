#include "UDPServerTransport.hpp"
#include "UDPClientTransport.hpp"
#include "NetworkMap.hpp"
#include "PacketHandler.hpp"
#include "Beacon.hpp"
#include "LinkNegotiator.hpp"
#include <stdio.h>

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

	NetworkMap nm;
	CryptoIdentity ci;
	LinkNegotiator lneg(nm, ci);
	PacketHandler hn(nm, ci, transports, lneg);

	for (Transport* tr : transports) tr->onPacket(hn);
	for (Transport* tr : transports) tr->enable();

	Beacon bcn(3,ci,transports);
	bcn.enable();

	ev_run (EV_DEFAULT_ 0);	
}
