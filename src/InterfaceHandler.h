#ifndef INTERFACEHANDLER_H_
#define INTERFACEHANDLER_H_

#include "CryptoIdentity.h"
#include "NetworkMap.h"
#include "ConnectionPool.h"
#include "Interface.h"

class InterfaceHandler {
public:
	InterfaceHandler(CryptoIdentity&, NetworkMap&, ConnectionPool&, Interface&);
    void operator()(std::string&& from, std::string&& packet);
private:
	CryptoIdentity& _ci;
	NetworkMap& _nm;
	ConnectionPool& _cp;
	Interface& _if;
};

#endif // INTERFACEHANDLER_H_
