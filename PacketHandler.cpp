// vim: set ts=4 sw=4 :
#include "PacketHandler.hpp"
#include "vindicat.pb.h"

PacketHandler::PacketHandler(NetworkMap& nm, CryptoIdentity& ci) 
	: _nm(nm)
	, _crypto_identity(ci)
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
		TransportSocket* trs_out = _nm.socketTo( hop.next() );
		trs_out->send(packet);
	} else if (tag == 2) { // Subgraph
		// Must be given to NetworkMap _nm
		Subgraph sg;
		if ( sg.ParseFromArray( packet.data()+1
							  , packet.size()-1 ) == 0 ) return;
		_nm.mergeGraph(sg);
	} else if (tag == 4) { // Beacon packet
		// optimize: cache beacon packets, do not parse+verify again if known
		DeviceBusinesscard card;
		if ( ! card.ParseFromArray( packet.data()+1, packet.size()-1 ) ) return;
		_nm.beacon(trs, card);
	}
}

bool understandEverything(const LinkProposal original) {
    // Makes sure there are no fields unknown to us in the LinkProposal
	// Copy all the fields we know,
	// then test if the original and the copy are the same.
	LinkProposal replica;
	replica.mutable_left_sigs()->CopyFrom(original.left_sigs());
    if ( original.has_link_info_msg() ) {
      replica.set_link_info_msg(original.link_info_msg());
	}
	return ( original.SerializeAsString() == replica.SerializeAsString() );
}
