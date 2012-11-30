#ifndef _LINKNEGOTIATOR_HPP
#define _LINKNEGOTIATOR_HPP

#include "NetworkMap.hpp"
#include "CryptoIdentity.hpp"

class LinkNegotiator {
public:
	LinkNegotiator(NetworkMap&, CryptoIdentity&);
	void beacon(TransportSocket*);
private:
	NetworkMap& _nm;
	CryptoIdentity& _ci;
};

#endif // _LINKNEGOTIATOR_HPP
