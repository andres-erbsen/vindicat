#include "PacketHandler.h"
#include "Forwarding.h"
#include "Connection.h"
#include "Link.h"
#include "vindicat.pb.h"
#include "Constants.h"
#include "Util.h"
#include "nacl25519_nm.h"

#include <iostream>
#include <cstdint>
#include <crypto_box.h>

const static unsigned int COOKIE_SIZE = 96;
const static unsigned int crypto_box_AUTHBYTES = crypto_box_ZEROBYTES - crypto_box_BOXZEROBYTES;

PacketHandler::
PacketHandler(NetworkMap& nm, CryptoIdentity& ci, ConnectionPool& cp, ConnectionHandler& ch)
	: _nm(nm)
	, _ci(ci)
	, _cp(cp)
	, _ch(ch)
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
		const auto algo = static_cast<SigAlgo>( promise->left_sig_algos(i) );
		if ( left_device->verifySignature(msg, sig, algo) ) {
			got_left = 1;
		}
	}

	int m = std::min(promise->right_sigs_size(), promise->right_sig_algos_size());
	bool got_right = 0;
	for (int i=0; i<m; ++i) {
		const auto& sig = promise->right_sigs(i);
		const auto algo = static_cast<SigAlgo>( promise->right_sig_algos(i) );
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
		} else {
			Connection::handle_auth(_ci, _ch, packet, ts, _cp, _nm);
		}

	} else if (tag == 1) { // "please forward packets with this route id that way"
		if (packet.size() < 1+8+8) return;
		std::string route_id = packet.substr(1,8);
		RoutingRequest rq;
		std::string hop_info;

		{ // Parse the public part of the request, decrypt the part meant for us
			std::string nonce = packet.substr(1,8+8);
			nonce.resize(24, '\0');
			if ( ! rq.ParseFromArray( packet.data()+1+8+8
			                        , packet.size()-1-8-8) ) return;
			if ( ! _ci.open(rq.details(), nonce, rq.sender_pubkey()
					, static_cast<PkencAlgo>( rq.enc_algo() ), hop_info) ) {
				return;
			}
		}

		Hop hop;
		if ( ! hop.ParseFromString(hop_info) ) return;

		if (hop.type() == Hop::UP) { // Connection to us, not through us
			Connection::handle_request(_ci, rq, hop, route_id, ts);
		}


	} else if (tag == 4) { // "Hey, it's /me/ here"
		auto dev = std::make_shared<Device>();
		if ( ! dev->parseFrom( packet.substr(2) ) ) return;
		std::cout << "Beacon from " << ipv6ify(dev->id()) << std::endl;

		if(packet[1] == '\x01') {
			std::string response("\x04\x00", 2);
			_ci.our_businesscard()->AppendToString(&response);
			ts.send(response);
		}

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
