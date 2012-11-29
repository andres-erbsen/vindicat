#include "NetworkMap.hpp"
#include "CryptoIdentity.hpp"
#include <stdint.h>
#include <vector>


class PacketHandler {
public:
	PacketHandler(NetworkMap&, CryptoIdentity&, std::vector<Transport*>&);
    void operator()(TransportSocket* ts, std::string p);
private:
    NetworkMap& _nm;
    CryptoIdentity& _crypto_identity;
	std::vector<Transport*>& _transports;
	void broadcast(const std::string&) const;
    // LinkProposal _createLinkProposal(DeviceBusinesscard,LinkStatus,LinkTransport);
    // LinkPromise _signLink(LinkProposal);
};


bool understandEverything(const LinkInfo&);

