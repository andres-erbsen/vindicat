#include "PacketHandler.h"
#include "Forwarding.h"
#include "Link.h"
#include "vindicat.pb.h"
#include "randomstring.h"
#include "Util.h"
#include "nacl25519_nm.h"

#include <iostream>
#include <cstdint>
#include <crypto_box.h>

const static unsigned int COOKIE_SIZE = 96;
const static unsigned int crypto_box_AUTHBYTES = crypto_box_ZEROBYTES - crypto_box_BOXZEROBYTES;

PacketHandler::PacketHandler(NetworkMap& nm, CryptoIdentity& ci)
	: _nm(nm)
	, _ci(ci)
	{}


std::shared_ptr<Link> make_link( std::shared_ptr<LinkPromise>&& promise
                               , const NetworkMap& nm) {
	std::unique_ptr<LinkInfo> info(new LinkInfo());
	if ( ! promise->has_link_info_msg()
	  || ! promise->left_sig_algos_size() 
	  || ! promise->right_sig_algos_size() 
	  || ! promise->left_sigs_size() 
	  || ! promise->right_sigs_size() 
	  || ! info->ParseFromString(promise->link_info_msg()) ) {
		return std::shared_ptr<Link>();
	}
	
	// Look up the relevant devices from the network map
	std::shared_ptr<Device> left_device  = nm.device(info->left() );
	std::shared_ptr<Device> right_device = nm.device(info->right());
	if ( ! left_device || ! right_device ) return std::shared_ptr<Link>();

	// verify the signatures
	const auto& msg = promise->link_info_msg();
	int n = std::min(promise->left_sigs_size(), promise->left_sig_algos_size());
	bool got_left = 0;
	for (int i=0; i<n; ++i) {
		const auto& sig = promise->left_sigs(i);
		const auto algo = promise->left_sig_algos(i);
		if ( left_device->verifySignature(msg, sig, algo) ) {
			got_left = 1;
		}
	}

	int m = std::min(promise->right_sigs_size(), promise->right_sig_algos_size());
	bool got_right = 0;
	for (int i=0; i<m; ++i) {
		const auto& sig = promise->right_sigs(i);
		const auto algo = promise->right_sig_algos(i);
		if ( right_device->verifySignature(msg, sig, algo) ) {
			got_right = 1;
		}
	}

	if ( !got_left || !got_right ) return std::shared_ptr<Link>();

	if (info->status() == LinkInfo::PUBLIC) {
		return std::make_shared<PublicLink>( info->left()
                                      , info->right()
                                      , info->time()
                                      , std::move(promise) );
	} else if (info->status() == LinkInfo::DEAD) {
		return std::make_shared<DeadLink>( info->left()
                                    , info->right()
                                    , info->time()
                                    , std::move(promise) );
	} else return std::shared_ptr<Link>();
}


static std::string ipv6ify(const std::string& input) {
    static const char* const lut = "0123456789abcdef";
    size_t len = input.length();
	if (len > 15) len = 15;

    std::string output = "4";
    output.reserve(1+3 * len);
    for (size_t i = 0; i < len; ++i) {
        const unsigned char c = input[i];
        output.push_back(lut[c >> 4]);
        output.push_back(lut[c & 15]);
		if (i%2==0 && i != len-1) output.push_back(':');
    }
    return output;
}


void PacketHandler::operator()(TransportSocket&& ts, std::string&& packet) {
	if (packet.size() == 0) {
		return;
	}
	uint8_t tag = packet[0];

	if ( tag == 0 ) { // "please forward this packet"
		if (packet.size() < 1+8+8) return;
		uint64_t route_id = *( reinterpret_cast<const uint64_t*>(packet.data()+1) );
		auto dev =_nm.device(ts);
		if (!dev) return;
		auto fwd = dev->getForwarding(route_id);
		if (fwd) {
			fwd->forward(packet);
			return;
		}

		// This may be the auth packet to start a connection, with contents:
		// pkttype, routeid, pktid, cookie, [[A'](A<>B'),bcard_A](A'<>B')
		if (packet.size() < 1+8+8+COOKIE_SIZE) return;
		std::string nonce = packet.substr(1,8+8);
		std::string their_connection_pk;
		std::string connection_sk;
		{ // open the cookie we gave them before
			std::string c;
			std::string cookie = packet.substr(1+8+8, COOKIE_SIZE);
			if ( ! _ci.cookies.open(cookie, c) ) return;
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
		{ // find the main enc key from the authenticated DeviceBusinesscard
			DeviceBusinesscard dev_card;
			if ( ! dev_card.ParseFromArray( message.data()+vouchlen
										  , message.size()-vouchlen ) ) return;
			DeviceInfo dev_info;
			if ( ! dev_card.has_device_info_msg() 
			  || ! dev_info.ParseFromString( dev_card.device_info_msg() ) ) {
				return;
			}
			int n = std::min(dev_info.enc_keys_size(), dev_info.enc_algos_size());
			for (int i=0; i<n; i++) {
				const PkencAlgo& algo = dev_info.enc_algos(i);
				if ( algo == PkencAlgo::CURVE25519XSALSA20POLY1305 ) {
					their_main_pk = dev_info.enc_keys(i);
					break;
				}
			}
		}

		// verify the vouching
		std::string vouch = message.substr(0, vouchlen);
		std::string vkey;
		if ( ! nacl25519_nm(their_main_pk, connection_sk)
				.decrypt(vouch, nonce, vkey) ) return;
		if (vkey != their_connection_pk) return;

		// FIXME: blacklist their connection enc key until the cookies expire
		// We've got a valid incoming connection!
	
	} else if (tag == 1) { // "please forward packets with this route id that way"
		if (packet.size() < 1+8+8) return;

		std::string hop_info;
		std::string their_connection_pk;
		PkencAlgo enc_algo;
		{ // Parse the public part of the request, decrypt the part meant for us
			std::string nonce = packet.substr(1,8+8);
			nonce.resize(24, '\0');
			RoutingRequest rq;
			if ( ! rq.ParseFromArray( packet.data()+1+8+8
			                        , packet.size()-1-8-8) ) return;
			their_connection_pk = rq.sender_pubkey();
			if ( ! _ci.open(rq.details(), nonce, their_connection_pk
						, enc_algo = rq.enc_algo(), hop_info) ) return;
		}
		Hop hop;
		if ( ! hop.ParseFromString(hop_info) ) return;

		if (hop.type() == Hop::UP) { // Connection to us, not through us
			// response is a "cookie packet":
			// pkt type, nonce (24 bytes), [B',ConnectionAccept](B<>A')
			std::string cookie_packet;
			cookie_packet.push_back('\0');
			std::string nonce = packet.substr(1,8) + randomstring(16);
			cookie_packet.append(nonce);
			// require authentication by public key authenticated encryption
			ConnectionAccept ack;
			ack.set_auth( ConnectionAccept::AUTHENC_BCARD );
			// generate our keys for this connection
			std::string connection_sk;
			std::string connection_pk = crypto_box_keypair(&connection_sk);
			// ..and let them store it for us, in encrypted form ofc
			ack.set_cookie( _ci.cookies.cookie(
						their_connection_pk + connection_sk ) );
			assert(ack.cookie().size() == COOKIE_SIZE);
			std::string encpart;
			_ci.encrypt( connection_pk+ack.SerializeAsString(), nonce
					   , enc_algo, their_connection_pk, encpart);
			cookie_packet.append(encpart);
			ts.send(cookie_packet);
		}


	} else if (tag == 4) { // "Hey, it's /me/ here"
		auto card = std::make_shared<DeviceBusinesscard>();
		if ( ! card->ParseFromArray( packet.data()+1
		                           , packet.size()-1 ) ) return;
		auto dev = std::make_shared<Device>();
		if ( ! dev->parseFrom( std::move(card) ) ) return;
		std::cout << "Beacon from " << ipv6ify(dev->id()) << std::endl;

		bool relevant = 0;
		{
			auto prev_dev = _nm.device(ts);
			if (!prev_dev) relevant = 1; // first valid card from this socket
			else { // tsocket already taken
				// the new device description replaces the old one if and only if
				// the latter was signed using at least one of the keys in the former
				for ( const auto& old_id : prev_dev->ids() ) {
					if ( contains(dev->ids(), old_id) ) {
						relevant = 1;
						break;
					}
				}
			}
		}
		if (!relevant) return;

		auto link = std::make_shared
			<DirectLink>(_nm.our_device().id(),std::move(ts),dev->id());
		assert(link);
		_nm.add( std::move(dev)  );
		_nm.add( std::move(link) );
	} else {
		std::cerr << "Packet with unkown tag " << tag << std::endl;
	}
}
