#include <crypto_box.h>
#include "Connection.h"
#include "Util.h"

const static unsigned int COOKIE_SIZE = 96;
const static unsigned int crypto_box_AUTHBYTES = crypto_box_ZEROBYTES - crypto_box_BOXZEROBYTES;

Connection::Connection(CryptoIdentity& ci, Path path, ConnectionPool& cp, Interface& iface)
	: Forwarding(randint64())
	, _ci(&ci)
	, _cp(cp)
	, _if(iface)
	, _naclsession( std::get<1>(path.at(path.size()-1)) .lock()->enc_key() )
	, _their_id(      std::get<1>(path.at(path.size()-1)) .lock()->id() )
	, _route_id( bytes( id() ) )
	, _authenticated(false)
	, _request_packet(new std::string)
	, _packet_queue(new std::deque<std::string>)
	{
	_request_packet->push_back('\1');

	std::string nonce = _route_id + randomstring(8);
	_request_packet->append(nonce);
	nonce.resize(crypto_box_NONCEBYTES,'\0');

	RoutingRequest rq;
	rq.set_enc_algo(enumval(PkencAlgo::CURVE25519XSALSA20POLY1305));
	rq.set_sender_pubkey(_naclsession.our_pk());
	{
		Hop hop;
		hop.set_type(Hop::UP);
		rq.set_details(_naclsession.encrypt(hop.SerializeAsString(), nonce));
	}

	for (signed int i=path.size()-2; i>=0; --i) {
		assert(0); // forwarding requests not supported yet
	}

	rq.AppendToString(_request_packet.get());
}

void Connection::request() {
	_pair_other.lock()->forward_out(*_request_packet);
}

void Connection::handle_request(const CryptoIdentity& ci, const RoutingRequest& rq, const Hop& hop, const std::string& route_id, TransportSocket ts) {
	assert( hop.type() == Hop::UP );
	// response is a "cookie packet":
	// pkt type, nonce (24 bytes), [B',ConnectionAccept](B<>A')
	std::string cookie_packet;
	cookie_packet.push_back('\0');
	std::string nonce = route_id + randomstring(16);
	cookie_packet.append(nonce);
	ConnectionAccept ack;
	ack.set_auth( ConnectionAccept::AUTHENC_BCARD );
	// generate our keys for this connection
	std::string connection_sk;
	std::string connection_pk = crypto_box_keypair(&connection_sk);
	// ...and let the other party store them for us, in encrypted form ofc
	ack.set_cookie( ci.cookies.cookie( rq.sender_pubkey() + connection_sk ) );
	assert(ack.cookie().size() == COOKIE_SIZE);
	std::string encpart;
	ci.encrypt( connection_pk+ack.SerializeAsString(), nonce
			  , static_cast<PkencAlgo>(rq.enc_algo()), rq.sender_pubkey()
			  , encpart);
	cookie_packet.append(encpart);
	ts.send(cookie_packet);

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
		nonce.resize(crypto_box_NONCEBYTES,'\0');
		{
			std::string vouch;
			_ci->encrypt(_naclsession.our_pk(), nonce, PkencAlgo::CURVE25519XSALSA20POLY1305, _naclsession.pk(), vouch);
			message.append(vouch); // [A'](A<>B')
		}
		_ci->our_businesscard()->AppendToString(&message);
		auth_packet.append( _naclsession.encrypt(message, nonce) );
	}
	_pair_other.lock()->forward_out(auth_packet);
}

void Connection::handle_auth(const CryptoIdentity& ci, const std::string& packet, TransportSocket ts, ConnectionPool& cp, Interface& iface) {
	// This may be the auth packet to start a connection, with contents:
	// pkttype, routeid, pktid, cookie, [[A'](A<>B'),bcard_A](A'<>B')
	if (packet.size() < 1+8+8+COOKIE_SIZE) return;
	std::string nonce = packet.substr(1,8+8);
	nonce.resize(crypto_box_NONCEBYTES,'\0');
	std::string their_connection_pk;
	std::string connection_sk;
	{ // open the cookie we gave them before
		std::string c;
		std::string cookie = packet.substr(1+8+8, COOKIE_SIZE);
		if ( ! ci.cookies.open(cookie, c) ) return;
		their_connection_pk = c.substr(0,crypto_box_PUBLICKEYBYTES);
		// FIXME: don't accept connections from connection enc keys that may
		// have been used with the current cookies to avoid session replays
		connection_sk = c.substr(crypto_box_PUBLICKEYBYTES);
		assert(connection_sk.size() == crypto_box_SECRETKEYBYTES);
	}
	std::string remaining = packet.substr(1+8+8+COOKIE_SIZE);

	// decrypt the message body
	std::string message;
	nacl25519_nm naclsession(their_connection_pk, connection_sk);
	if ( ! naclsession.decrypt(remaining, nonce, message) ) return;

	// their main enc key should vouch for the connection enc key
	auto vouchlen = crypto_box_PUBLICKEYBYTES + crypto_box_AUTHBYTES;
	if (message.size() < vouchlen) return;
	std::string their_main_pk;
	Device dev;
	if ( ! dev.parseFrom( message.substr(vouchlen) ) ) return;

	// verify the vouching
	std::string vouch = message.substr(0, vouchlen);
	std::string vkey; // FIXME: get from device
	if ( ! dev.open( vouch, nonce, connection_sk
	               , PkencAlgo::CURVE25519XSALSA20POLY1305, vkey )
	  || vkey != their_connection_pk) return;

	// all ok, save the connection
	std::string their_id = dev.id();
	std::string route_id = packet.substr(1,8);
	auto conn = std::make_shared<Connection>(std::move(naclsession), their_id, route_id, cp, iface);
}	

Connection::Connection(nacl25519_nm&& ns, const std::string& their_id
		, const std::string& route_id, ConnectionPool& cp, Interface& iface)
	: Forwarding( *reinterpret_cast<const uint64_t*>(route_id.data()) )
	, _ci(nullptr)
	, _cp(cp)
	, _if(iface)
	, _naclsession(std::move(ns))
	, _their_id(their_id)
	, _route_id(route_id)
	, _authenticated(true)
	, _request_packet(nullptr)
	, _packet_queue(nullptr)
	{}

bool Connection::forward(const std::string& packet) {
	_packet_queue->push_back(packet);
	return 1;
}

void Connection::_incoming(const std::string& packet) {
	// data packet: '\0',rid,pktid, [data type, data](A'<>B')
	std::string m;
	if ( ! _naclsession.decrypt( packet.substr(1+16)
	                           , packet.substr(1,16)
                               , m ) ) return;
	_if.send(_their_id,m[0],m.substr(0));
}

bool Connection::forward_out(const std::string& packet) {
	if (!_authenticated) _auth(packet);
	else _incoming(packet);
	return 1;
}

void Connection::detatch() {
	_cp.erase(_their_id);
}
