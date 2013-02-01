#ifndef PACKETHANDLER_H_
#define PACKETHANDLER_H_

#include "Transport.h"
#include "NetworkMap.h"

#include <memory>

class PacketHandler {
public:
	PacketHandler(NetworkMap& nm);
    void operator()(std::shared_ptr<TransportSocket> trs, const std::string& packet);
private:
	NetworkMap& _nm;
};

#endif // PACKETHANDLER_H_
