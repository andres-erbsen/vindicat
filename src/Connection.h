#ifndef CONNECTION_H_
#define CONNECTION_H_

#include "Forwarding.h"
#include "ConnectionPool.h"
#include "NetworkMap.h"
#include "CryptoIdentity.h"
#include "nacl25519_nm.h"
#include "randomstring.h"

#include <deque>

class Connection : public Forwarding {
public:
	Connection(CryptoIdentity&, ConnectionPool&, Path);
    void detatch() override;
    bool forward_out(const std::string&) override;
    bool forward(const std::string&) override;

	// packet generation
	void hello();
	void auth(std::string&& cookie_packet);
private:
	CryptoIdentity& _ci;
	ConnectionPool& _cp;
	Path _path;
	nacl25519_nm _naclsession;
	std::string _dst_id;
	std::string _route_id;

	void _gen_hello_packet();
	std::string _hello_packet;

	std::deque<std::string> _packet_queue;
};

#endif // CONNECTION_H_
