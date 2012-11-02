#include "NetworkMap.hpp"
#include <stdint.h>


class PacketHandler {
public:
    void handlePacket(TransportSocket* ts, std::string p);
private:
    NetworkMap& _nm;
    // LinkProposal _createLinkProposal(DeviceBusinesscard,LinkStatus,LinkTransport);
    // LinkPromise _signLink(LinkProposal);
};
