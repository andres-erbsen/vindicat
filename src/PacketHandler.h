#ifndef PACKETHANDLER_H_
#define PACKETHANDLER_H_

#include "Transport.h"
#include "NetworkMap.h"
#include "CryptoIdentity.h"
#include "ConnectionPool.h"
#include "ConnectionHandler.h"

#include <memory>

class PacketHandler {
public:
	PacketHandler(NetworkMap& nm, CryptoIdentity& ci, ConnectionPool& cp, ConnectionHandler&);
    void operator()(TransportSocket&& trs, std::string&& packet);
private:
	NetworkMap& _nm;
	CryptoIdentity& _ci;
	ConnectionPool& _cp;
	ConnectionHandler& _ch;
};

#endif // PACKETHANDLER_H_
