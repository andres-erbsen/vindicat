#ifndef CONNECTION_H_
#define CONNECTION_H_

#include "Forwarding.h"
#include "Interface.h"
#include "ConnectionPool.h"
#include "NetworkMap.h"
#include "CryptoIdentity.h"
#include "nacl25519_nm.h"
#include "Util.h"

#include <memory>
#include <deque>

class Connection : public Forwarding {
public:
	static void handle_request(const CryptoIdentity&, const RoutingRequest&,
			const Hop&, const std::string&, TransportSocket);
	static void handle_auth(const CryptoIdentity& ci,
			const std::string& packet, TransportSocket ts, ConnectionPool& cp, Interface& iface);

	// initiate a connection
	Connection(CryptoIdentity&, Path, ConnectionPool&, Interface&);
	// incoming connection
	Connection(nacl25519_nm&&, const std::string&, const std::string&, ConnectionPool&, Interface&);

    void detatch() override;
    bool forward_out(const std::string&) override;
    bool forward(const std::string&) override;

	void request();
private:
	void _auth(const std::string& cookie_packet);
	void _incoming(const std::string& cookie_packet);

	CryptoIdentity* _ci; // required while negotiating connection
	ConnectionPool& _cp;
	Interface& _if;

	nacl25519_nm _naclsession; // reused
	std::string _their_id;
	std::string _route_id;

	bool _authenticated;
	// present until authenticatd:
	std::unique_ptr< std::string > _request_packet;
	std::unique_ptr< std::deque<std::string> > _packet_queue;
};

#endif // CONNECTION_H_
