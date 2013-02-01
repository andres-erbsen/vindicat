#ifndef PACKETHANDLER_H_
#define PACKETHANDLER_H_

#include "Transport.h"

#include <memory>

class PacketHandler {
public:
    void operator()(std::shared_ptr<TransportSocket> trs, const std::string& packet);
private:
};

#endif // PACKETHANDLER_H_
