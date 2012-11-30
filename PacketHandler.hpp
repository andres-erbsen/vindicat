#include "NetworkMap.hpp"
#include "CryptoIdentity.hpp"
#include "LinkNegotiator.hpp"
#include <stdint.h>
#include <vector>


class PacketHandler {
public:
	PacketHandler( NetworkMap&
	             , CryptoIdentity&
	             , std::vector<Transport*>&
	             , LinkNegotiator& );
    void operator()(TransportSocket* ts, std::string p);
private:
    NetworkMap& _nm;
    CryptoIdentity& _crypto_identity;
	std::vector<Transport*>& _transports;
	LinkNegotiator& _lneg;
	void broadcast(const std::string&) const;
    // LinkProposal _createLinkProposal(DeviceBusinesscard,LinkStatus,LinkTransport);
    // LinkPromise _signLink(LinkProposal);
};


bool understandEverything(const LinkInfo&);

