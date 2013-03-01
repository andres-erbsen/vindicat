#ifndef CONNECTION_H_
#define CONNECTION_H_

#include "Forwarding.h"
#include "Interface.h"
#include "ConnectionPool.h"
#include "NetworkMap.h"
#include "CryptoIdentity.h"
#include "nacl25519_nm.h"
#include "Util.h"
#include "NonceGen64.h"

#include <memory>
#include <vector>

class Connection : public Forwarding {
public:
	static void handle_request(const CryptoIdentity&, const RoutingRequest&,
			const Hop&, const std::string&, TransportSocket);
	static void handle_auth(const CryptoIdentity& ci, Interface& iface,
			const std::string& packet, TransportSocket ts, ConnectionPool& cp,
			NetworkMap& nm);

	// initiate a connection
	Connection(CryptoIdentity&, Path, ConnectionPool&, Interface&);
	// incoming connection
	Connection(nacl25519_nm&&, const std::string&, const std::string&, ConnectionPool&, Interface&);

    void detatch() override;
    bool forward_out(const std::string&) override;
    bool forward(const std::string&) override;

	void request();
private:
	void _auth(const std::string&);
	void _outgoing(const std::string&);
	void _incoming(const std::string&);

	CryptoIdentity* _ci; // required while negotiating connection
	ConnectionPool& _cp;
	Interface& _if;

	nacl25519_nm _naclsession; // reused
	std::string _their_id;
	std::string _route_id;

	bool _authenticated;
	// present until authenticated:
	std::unique_ptr< std::string > _request_packet;
	std::unique_ptr< std::vector<std::string> > _packet_queue;
	// present after authenticated:
	std::unique_ptr< NonceGen64 > _noncegen;
};

#endif // CONNECTION_H_
