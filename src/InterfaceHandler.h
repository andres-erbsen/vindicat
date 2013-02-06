#ifndef INTERFACEHANDLER_H_
#define INTERFACEHANDLER_H_

#include "Interface.h"
#include "ConnectionPool.h"
#include "NetworkMap.h"

class InterfaceHandler {
public:
	InterfaceHandler(NetworkMap& nm, ConnectionPool& cp);
    void operator()(std::string&& from, std::string&& packet);
private:
	NetworkMap& _nm;
	ConnectionPool& _cp;
};

#endif // INTERFACEHANDLER_H_
