#ifndef PACKETHANDLER_H_
#define PACKETHANDLER_H_

#include "Transport.h"
#include "NetworkMap.h"
#include "CryptoIdentity.h"

#include <memory>

class PacketHandler {
public:
	PacketHandler(NetworkMap& nm, CryptoIdentity& ci);
    void operator()(const TransportSocket &trs, const std::string& packet);
private:
	NetworkMap& _nm;
	CryptoIdentity& _ci;
};

#endif // PACKETHANDLER_H_
