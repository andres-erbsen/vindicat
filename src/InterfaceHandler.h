#ifndef INTERFACEHANDLER_H_
#define INTERFACEHANDLER_H_

#include "Interface.h"
#include "CryptoIdentity.h"
#include "NetworkMap.h"
#include "ConnectionPool.h"

class InterfaceHandler {
public:
	InterfaceHandler(CryptoIdentity&, NetworkMap&, ConnectionPool&);
    void operator()(std::string&& from, std::string&& packet);
private:
	CryptoIdentity& _ci;
	NetworkMap& _nm;
	ConnectionPool& _cp;
};

#endif // INTERFACEHANDLER_H_
