#include <sodium/crypto_box.h>
#include "crypto_box-wrapper.h"
#include "Connection.h"
#include "Util.h"

const static unsigned int COOKIE_SIZE = 96;

Connection::Connection(CryptoIdentity& ci, Path path, ConnectionPool& cp, ConnectionHandler& ch)
  : Forwarding(randint64())
  , _ci(&ci)
  , _cp(cp)
  , _ch(ch)
  , _naclsession( std::get<1>(path.at(path.size()-1)) .lock()->enc_key() )
  , _their_id(      std::get<1>(path.at(path.size()-1)) .lock()->id() )
  , _route_id( bytes( id() ) )
  , _authenticated(false)
  , _request_packet(new std::string)
  , _packet_queue(new std::vector<std::string>)
  , _nonces(nullptr)
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
    hop.set_nonce_algo(Hop::XTEA32);
    rq.set_details(_naclsession.encrypt(hop.SerializeAsString(), nonce));
  }

  for (signed int i=path.size()-2; i>=0; --i) {
    assert(0); /// \TODO forwarding requests not supported yet
  }

  rq.AppendToString(_request_packet.get());
}

void Connection::request() {
  _pair_other.lock()->forward_out(*_request_packet);
}

void Connection::handle_request(const CryptoIdentity& ci, const RoutingRequest& rq, const Hop& hop, const std::string& route_id, TransportSocket ts) {
  if ( hop.type() != Hop::UP ) return;
  if ( hop.nonce_algo() != Hop::XTEA32 ) return;
  // response is a "cookie packet":
  // tag, route id, nonce (24 bytes), [ConnectionAccept](B<>A')
  std::string cookie_packet;
  cookie_packet.push_back('\0');
  cookie_packet.append(route_id);
  std::string nonce = randomstring(24);
  cookie_packet.append(nonce);
  ConnectionAccept ack;
  ack.set_auth( ConnectionAccept::AUTHENC_BCARD );
  // generate our keys for this connection
  std::string connection_sk;
  std::string connection_pk = crypto_box_keypair(&connection_sk);
  ack.set_sender_pubkey(connection_pk);
  // ...and let the other party store them for us, in encrypted form ofc
  ack.set_cookie( ci.cookies.cookie( rq.sender_pubkey() + connection_sk ) );
  assert(ack.cookie().size() == COOKIE_SIZE);
  std::string encpart;
  ci.encrypt( ack.SerializeAsString()
            , nonce
            , static_cast<PkencAlgo>(rq.enc_algo())
            , rq.sender_pubkey()
            , encpart);
  cookie_packet.append(encpart);
  ts.send(cookie_packet);

}

void Connection::_auth(const std::string& cookie_packet) {
  // cookie packet: tag, route id, nonce (24 bytes), [ConnectionAccept](B<>A')
  if (cookie_packet.size() < 1+8+24) return;
  std::string route_id = cookie_packet.substr(1,8);
  assert(route_id == _route_id);

  std::string m;
  if ( ! _naclsession.decrypt( cookie_packet.substr(1+8+24)
                             , cookie_packet.substr(1+8,24)
                               , m ) ) return;

  std::string cookie;
  {
    ConnectionAccept ack;
    if (ack.auth() != ConnectionAccept::AUTHENC_BCARD) return;
    ack.ParseFromString(m);
    cookie = ack.cookie();
    _naclsession.pk(ack.sender_pubkey());
  }

  // client_auth packet: pkttype,rid,pktid,cookie,[[A'](A<>B'),bcard_A](A'<>B')
  std::string auth_packet;
  auth_packet.push_back('\0');
  auth_packet.append(route_id);

  _nonces.reset(new NonceSession( _naclsession.encrypt("", std::string(24,'\0'))
                                , _naclsession.nonce_bit() ));
  std::string packet_id = _nonces->next();
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
  _authenticated = true;

  // also send all the queued packets
  for ( const std::string& packet : *_packet_queue ) _outgoing(packet);
  _packet_queue.reset(nullptr);
  _request_packet.reset(nullptr);
}

void Connection::handle_auth(CryptoIdentity& ci, ConnectionHandler& ch, const std::string& packet, TransportSocket ts, ConnectionPool& cp, NetworkMap& nm) {
  // This may be the auth packet to start a connection, with contents:
  // pkttype, routeid, pktid, cookie, [[A'](A<>B'),bcard_A](A'<>B')
  if (packet.size() < 1+8+8+COOKIE_SIZE) return;
  std::cout << "auth size ok" << std::endl;
  std::string nonce = packet.substr(1,8+8);
  nonce.resize(crypto_box_NONCEBYTES,'\0');
  std::string their_connection_pk;
  std::string connection_sk;
  { // open the cookie we gave them before
    std::string c;
    std::string cookie = packet.substr(1+8+8, COOKIE_SIZE);
    if ( ! ci.cookies.open(cookie, c) ) return;
    std::cout << "auth cookie ok" << std::endl;
    their_connection_pk = c.substr(0,crypto_box_PUBLICKEYBYTES);
    if ( ! ci.cookies.allowed(their_connection_pk)) return;
    std::cout << "auth cookie allowed" << std::endl;
    ci.cookies.blacklist(their_connection_pk);
    /// \FIXME don't accept connections from connection enc keys that may
    /// have been used with the current cookies to avoid session replays
    connection_sk = c.substr(crypto_box_PUBLICKEYBYTES);
    assert(connection_sk.size() == crypto_box_SECRETKEYBYTES);
  }
  std::string remaining = packet.substr(1+8+8+COOKIE_SIZE);

  // decrypt the message body
  std::string message;
  nacl25519_nm naclsession(their_connection_pk, connection_sk);
  if ( ! naclsession.decrypt(remaining, nonce, message) ) return;
  std::cout << "auth message ok" << std::endl;

  // their main enc key should vouch for the connection enc key
  auto vouchlen = crypto_box_PUBLICKEYBYTES + crypto_box_MACBYTES;
  if (message.size() < vouchlen) return;
  std::string their_main_pk;
  Device dev;
  if ( ! dev.parseFrom( message.substr(vouchlen) ) ) return;
  std::cout << "auth device ok" << std::endl;

  // verify the vouching
  std::string vouch = message.substr(0, vouchlen);
  std::string vkey; /// \FIXME get from device
  if ( ! dev.open( vouch, nonce, connection_sk
                 , PkencAlgo::CURVE25519XSALSA20POLY1305, vkey )
    || vkey != their_connection_pk) return;
  std::cout << "auth vouch ok" << std::endl;

  // all ok, save the connection
  std::string their_id = dev.id();
  std::string route_id = packet.substr(1,8);

  auto conn = std::make_shared
    <Connection>(std::move(naclsession), their_id, route_id, cp, ch);
  auto rfwd = std::make_shared
    <SimpleForwarding>(nm, conn->id());  
  Forwarding::pair(conn, rfwd);  

  cp.insert( std::make_pair(their_id, conn) );
  nm.device(ts)->addForwarding(rfwd);
}  

Connection::Connection(nacl25519_nm&& ns, const std::string& their_id
    , const std::string& route_id, ConnectionPool& cp, ConnectionHandler& ch)
  : Forwarding( *reinterpret_cast<const uint64_t*>(route_id.data()) )
  , _ci(nullptr)
  , _cp(cp)
  , _ch(ch)
  , _naclsession(std::move(ns))
  , _their_id(their_id)
  , _route_id(route_id)
  , _authenticated(true)
  , _request_packet(nullptr)
  , _packet_queue(nullptr)
  , _nonces( new NonceSession( _naclsession.encrypt("", std::string(24, '\0'))
                             , _naclsession.nonce_bit()) )
  {}

bool Connection::forward(const std::string& packet) {
  if ( ! _authenticated ) _packet_queue->push_back(packet);
  else _outgoing(packet);
  return 1;
}

void Connection::_outgoing(const std::string& packet) {
  assert(packet.size() >= 1);
  if (packet[0] == '\xDC') return; // would collide with embedded message
  assert(_nonces);
  std::string packet_id = _nonces->next();
  assert(packet_id.size() == 8);
  std::string nonce = _route_id + packet_id;
  assert(nonce.size() == 16);
  nonce.resize(crypto_box_NONCEBYTES,'\0');
  std::string nul(1,'\0');
  _pair_other.lock()->forward_out( 
      nul + _route_id + packet_id + _naclsession.encrypt(packet, nonce)
  );
}

void Connection::_incoming(const std::string& packet) {
  // data packet: '\0',rid,pktid, [data type, data](A'<>B')
  std::string nonce(packet.substr(1,8+8));
  std::string packet_id(packet.substr(1+8,8));
  assert(_nonces);
  if ( ! _nonces->open(packet_id) ) return; // out of order / replay / ours
  nonce.resize(crypto_box_NONCEBYTES,'\0');

  std::string m;
  if ( ! _naclsession.decrypt( packet.substr(1+8+8)
                             , nonce
                               , m ) ) return;
  _ch(_their_id, m[0], m.substr(1));
}

bool Connection::forward_out(const std::string& packet) {
  if ( ! _authenticated) _auth(packet);
  else _incoming(packet);
  return 1;
}

void Connection::detatch() {
  _cp.erase(_their_id);
}
