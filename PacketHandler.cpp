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
		if ( hop.ParseFromString ( tail_bytes ) == 0 ) return;

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
