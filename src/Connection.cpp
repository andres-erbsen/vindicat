#include "Connection.h"
#include "randomstring.h"

uint64_t randint64() {
	unsigned char b[8];
	randombytes(b, 8);
	return *((uint64_t*) b);
}

std::string bytes(uint64_t n) {
	char* p = reinterpret_cast<char*> (&n);
	return std::string(p,8);
}

Connection::Connection(CryptoIdentity& ci, ConnectionPool& cp, Path path)
	: Forwarding(randint64())
	, _ci(ci)
	, _cp(cp)
	, _path(path)
	, _naclsession( std::get<1>(path.at(path.size()-1)) .lock()->enc_key() )
	, _dst_id(      std::get<1>(path.at(path.size()-1)) .lock()->id() )
	, _route_id( bytes( id() ) )
	, _authenticated(false)
	{}

void Connection::detatch() {
	_cp.erase(_dst_id);
}

bool Connection::forward(const std::string& packet) {
	_packet_queue.push_back(packet);
	return 1;
}

void Connection::hello() {
	if (_hello_packet.empty()) _gen_hello_packet();
	_pair_other.lock()->forward_out(_hello_packet);
}

bool Connection::forward_out(const std::string& packet) {
	if (!_authenticated) _auth(packet);
	else _incoming(packet);
	return 1;
}

void Connection::_gen_hello_packet() {
	_hello_packet.clear();
	_hello_packet.push_back('\1');

	std::string nonce = _route_id + randomstring(8);
	_hello_packet.append(nonce);
	nonce.resize(24,'\0');

	RoutingRequest rq;
	rq.set_enc_algo(PkencAlgo::CURVE25519XSALSA20POLY1305);
	rq.set_sender_pubkey(_naclsession.our_pk());
	{
		Hop hop;
		hop.set_type(Hop::UP);
		rq.set_details(_naclsession.encrypt(hop.SerializeAsString(), nonce));
	}

	for (int i=_path.size()-2; i>=0; --i) {
		assert(0); // forwarding requests not supported yet
	}

	rq.AppendToString(&_hello_packet);
}


void Connection::_auth(const std::string& cookie_packet) {
	// cookie packet: ppttype, nonce (24 bytes), [B',ConnectionAccept](B<>A')
	std::string m;
	if ( ! _naclsession.decrypt( cookie_packet.substr(1+24)
	                           , cookie_packet.substr(1,24)
                               , m ) ) return;
	std::string route_id = cookie_packet.substr(1,8);
	assert(route_id == _route_id);

	_naclsession.pk(m.substr(0,32));
	std::string cookie;
	{
		ConnectionAccept ack;
		if (ack.auth() != ConnectionAccept::AUTHENC_BCARD) return;
		ack.ParseFromString(m.substr(32));
		cookie = ack.cookie();
	}

	// client_auth packet: pkttype,rid,pktid,cookie,[[A'](A<>B'),bcard_A](A'<>B')
	std::string auth_packet;
	auth_packet.push_back('\0');
	auth_packet.append(route_id);
	std::string packet_id = randomstring(8); // FIXME: generate deterministically
	auth_packet.append(packet_id);
	auth_packet.append(cookie); // a server should know how long its cookies are
	{
		std::string message;
		std::string nonce = route_id+packet_id;
		nonce.resize(24,'\0');
		{
			std::string vouch;
			_ci.encrypt(_naclsession.our_pk(), nonce, PkencAlgo::CURVE25519XSALSA20POLY1305, _naclsession.pk(), vouch);
			message.append(vouch); // [A'](A<>B')
		}
		_ci.our_businesscard()->AppendToString(&message);
		auth_packet.append( _naclsession.encrypt(message, nonce) );
	}
	_pair_other.lock()->forward_out(auth_packet);
}

void Connection::_incoming(const std::string& packet) {
	// data packet: '\0',rid,pktid, [data type, data](A'<>B')
	std::string m;
	if ( ! _naclsession.decrypt( packet.substr(1+16)
	                           , packet.substr(1,16)
                               , m ) ) return;
}
