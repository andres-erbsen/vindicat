// vim: set ts=4 sw=4 :
#include "PacketHandler.hpp"
#include "vindicat.pb.h"
#include <ctime>

PacketHandler::PacketHandler( NetworkMap& nm
                            , CryptoIdentity& ci
                            , std::vector<Transport*>& transports
	                        , LinkNegotiator& lneg)
    : _nm(nm)
	, _crypto_identity(ci)
	, _transports(transports)
	, _lneg(lneg)
	{}

void PacketHandler::operator()(TransportSocket* trs, std::string packet) {
	if (packet.size() == 0) {
		_nm.addSocket(trs); // TODO: do we need to check whether the sock was new?
		return;
	}
	uint8_t tag = packet[0];
	if (tag == 0) { // "Forward me as agreed before"
		// the most popular packet, the one that needs forwarding
		// This packet is NOT a protobuf message, for space and parsing efficiency.
		// After the tag there is a 32-bit route identifier.
		// Note that the endianness does not matter here, just uniqueness.
		// Packet structure: 32 bits route id; packet id, data... (opaque)
		if (packet.size() < 1+4) return;
		uint32_t route_id = *((uint32_t*) packet.data() + 1 );
		_nm.forward(trs, route_id, packet);
	} else if (tag == 1) { // "Please forward packets with this id to them"
		// This packet is NOT entirely a protobuf message
		// The static header is however followed by protobuf-encoded routing details
		// Packet structure: route id, packet id, more headers as protobuf
		if (packet.size() < 1+4+4) return;
		uint32_t route_id  = *( (uint32_t*) packet.data() + 1 ); // 4 bytes
		uint32_t packet_id = *( (uint32_t*) packet.data() + 1+4 ); // 4 bytes
		// protobuf headers
		RoutingRequest rrq;
		if ( rrq.ParseFromArray( packet.data()+1+4+4
		                       , packet.size()-1-4-4 ) == 0 ) return;

		// the encrypted cons list of forwardings
		EncEnvelope* enc = rrq.mutable_tail();
		std::string tail_bytes;
		// some encryption details may be specified on request or per-node
		// the combination of route_id and packet_id serves as a nonce
		if ( ! enc->has_nonce() ) enc->set_nonce(packet.data()+1, 4+4);
		if ( rrq.has_sender_pubkey() && ! enc->has_enckey() ) {
			enc->set_enckey( rrq.sender_pubkey().key() );
		}
		if ( _crypto_identity.open(*enc, tail_bytes) == 0 ) return;
		Hop hop;
		if ( hop.ParseFromString(tail_bytes) == 0 ) return;

		Forwarding* fwd;
		if ( hop.type() == Hop::SIMPLE_ONEWAY ) {
			fwd = new OnewayForwarding(route_id, hop.next());
		} else if ( hop.type() == Hop::SIMPLE_TWOWAY ) {
			fwd = new TwowayForwarding(route_id, hop.next());
		} else return;
		if ( _nm.addForwarding(trs, fwd) == 0 ) return;

		*enc = hop.tail();
		packet.resize(1+4+4);
		if ( rrq.AppendToString(&packet) == 0 ) return;
		TransportSocket* trs_out = _nm.dev_socket( hop.next() );
		assert(trs_out != NULL);
		trs_out->send(packet);
	} else if (tag == 2) { // Subgraph
		// Must be given to NetworkMap _nm
		Subgraph sg;
		if ( sg.ParseFromArray( packet.data()+1
		                      , packet.size()-1 ) == 0 ) return;
		if ( _nm.mergeGraph(sg) ) { // was interesting enough for us to spread it
			broadcast(packet);
		}
	} else if (tag == 3) {
		// LinkProposal
		LinkProposal l_prop;
		if ( l_prop.ParseFromArray( packet.data()+1
		                          , packet.size()-1 ) == 0 ) return;
		
		// Get DeviceInfo of left device according to TransportSocket
		DeviceBusinesscard left_card;
		if ( ! _nm.dev_bcard(trs, left_card) ) return;
		
		{
			LinkInfo link_unchecked;
			if ( ! link_unchecked.ParseFromString(l_prop.link_info_msg()) ) return;
			// Make sure there are no unknown fields in l_prop
			if ( ! understandEverything(link_unchecked) ) return;
			// Test if agree with everything in the proposed link
			if ( ! agreeWithFields(link_unchecked) ) return;
		}
		
		// Verify signatures
		DeviceInfo ldev;
		if ( ! verify(left_card, ldev) ) return;
		LinkInfo link;
		if ( ! verify(l_prop, ldev, link) ) return;
		
		// IMPORTANT
		// Before continuing beyond this point,
		// it's wise to determine if all the fields in l_prop are ok by us
		// Right now, everything has been checked
		
		// Sign
		Signature sig;
		if ( _crypto_identity.sign(l_prop.link_info_msg(), sig) == 0 ) return;
		
		// Create LinkPromise
		LinkPromise l_prom;
		l_prom.set_link_info_msg(l_prop.link_info_msg());
		l_prop.mutable_left_sigs()->Swap(l_prom.mutable_left_sigs());
		// WARNING: left sigs of l_prop are now NOT set
		*l_prom.add_right_sigs() = sig;
		
		// Create Subgraph
		Subgraph sg;
		*sg.add_devices() = left_card;
		_crypto_identity.our_businesscard(*sg.add_devices());
		*sg.add_links() = l_prom;
		
		// Add to map
		_nm.mergeGraph(sg);
		
		// Broadcast
		std::string sg_packet = "\2";
		if ( ! sg.AppendToString(&sg_packet) ) assert(0);
		broadcast(sg_packet);
		
	} else if (tag == 4) { // Beacon packet
		// optimize: cache beacon packets, do not parse+verify again if known
		DeviceBusinesscard card;
		if ( ! card.ParseFromArray( packet.data()+1, packet.size()-1 ) ) return;
		_nm.beacon(trs, card);
		if (! _nm.has_link(trs) ) _lneg.beacon(trs);
	}
}

void PacketHandler::broadcast(const std::string& packet) const {
	for (auto transport : _transports) {
		transport->broadcast(packet);
	}
}

bool PacketHandler::agreeWithFields(const LinkInfo& link) {
    // Tests if we agree with every field in proposed link
    // If necessary, add your own tests here
    
    // Status is public
    if ( link.status() != LinkInfo::PUBLIC ) return 0;
    // Left device is the one sending data - will be tested when verifying signatures
    // Right device is us
    if ( ! std::count(_crypto_identity.our_identifiers().begin(), _crypto_identity.our_identifiers().end(), link.right()) ) return 0;
    // Time is approximately now
    // Adequate values chosen to be between 20s in the past and 5s in the future
    if ( (std::time(NULL) - link.time()) > 20 ) return 0;
    if ( (link.time() - std::time(NULL)) >  5 ) return 0;
    
    return 1;
}

bool understandEverything(const LinkInfo& original) {
	// Makes sure there are no fields unknown to us in the LinkProposal
	// Copy all the fields we know,
	// then test if the original and the copy are the same.
	LinkInfo replica;
	replica.set_status (original.status());
	replica.set_left   (original.left()  );
	replica.set_right  (original.right() );
	if ( original.has_time() ) {
	  replica.set_time(original.time());
	}
	return ( original.SerializeAsString() == replica.SerializeAsString() );
}
