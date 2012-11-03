// vim: set ts=4 sw=4 :
#include "PacketHandler.hpp"
#include "vindicat.pb.h"

void PacketHandler::handlePacket(TransportSocket* trs, std::string packet) {
	if (packet.size() == 0) {
		// TODO: handle new tranportsockets
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
	}
    else if (tag == 1) { // "Please forward packets with this id to them"
		// This packet is NOT entirely a protobuf message
		// The static header is however followed by protobuf-encoded routing details
		// Packet structure: route id, packet id, details as encrypted protobuf
		if (packet.size() < 1+4+4) return;
		uint32_t route_id  = *( (uint32_t*) packet.data() + 1 ); // 4 bytes
		uint32_t packet_id = *( (uint32_t*) packet.data() + 1+4 ); // 4 bytes
		RoutingRequest rrq;
		if ( rrq.ParseFromArray( packet.data()+1+4+4
                               , packet.size()-1-4-4 ) == 0 ) return;
		EncEnvelope* enc = rrq.mutable_tail();
		if ( ! enc->has_nonce() ) enc->set_nonce(packet.data()+1, 4+4);
		if ( rrq.has_sender_pubkey() && ! enc->has_sender_pubkey() ) {
			*(enc->mutable_sender_pubkey()) = rrq.sender_pubkey();
		}
		std::string tail_bytes;
		if ( _crypto_identity.open(*enc, tail_bytes) == 0 ) return;
		Hop hop;
		if ( hop.ParseFromString ( tail_bytes ) == 0 ) return;
		if ( hop.type() == Hop::SIMPLE_ONEWAY ) {
			_nm.addForwarding( trs, SimpleForwarding(route_id, hop.next) );
		} else return;
	}
	/* TODO...
    else if (tag == 2) {
        // Subgraph
        // Must be given to NetworkMap _nm
        _nm.mergeGraph(Subgraph(packet)); // TODO - or should sth else do it?
        // Also TODO: confidence in given subgraph?
    }
    else if (tag == 3) {
        // LinkProposal
        // Next step: signing, adding to network map, broadcasting
        // TODO: verifying data accuracy
        LinkPromise lp = _signLink(LinkProposal(packet));
        // TODO: Add to map
        // TODO: Broadcast
    }
    else if (tag == 4) {
        // DeviceBusinesscard
        // Now LinkProposal has to be created and sent back
        LinkProposal lp = _createLinkProposal(DeviceBusinesscard(packet),2,1);
		// TODO: remove hardcoded link params?
        // TODO: Forward packet
    }
	*/
}
