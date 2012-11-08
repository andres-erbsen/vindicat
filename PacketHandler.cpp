// vim: set ts=4 sw=4 :
#include "PacketHandler.hpp"
#include "vindicat.pb.h"

PacketHandler::PacketHandler(NetworkMap& nm, CryptoIdentity& ci) 
	: _nm(nm)
	, _crypto_identity(ci)
	{}

void PacketHandler::operator()(TransportSocket* trs, std::string packet) {
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
	else if (tag == 2) { // Subgraph
		// Must be given to NetworkMap _nm
		Subgraph sg;
		if ( sg.ParseFromArray( packet.data()+1
							  , packet.size()-1 ) == 0 ) return;
		_nm.mergeGraph(sg);
	}
	else if (tag == 3) { // LinkProposal
		LinkProposal l_prop;
		if ( l_prop.ParseFromArray( packet.data()+1
								  , packet.size()-1 ) == 0 ) return;
		
		// Signing
		Signature sig;
		if ( _crypto_identity.sign(l_prop.link_info_msg(), sig) == 0) return;
		
		// Creating LinkPromise
		LinkPromise l_prom;
		l_prom.set_link_info_msg(l_prop.link_info_msg());
		for (int i = 0; i < l_prop.left_sigs_size(); i++) {
			*l_prom.add_left_sigs() = l_prop.left_sigs(i);
		}
		*l_prom.add_right_sigs() = sig;
		
		// Adding to map
		LinkInfo l_info;
		if ( l_info.ParseFromString( l_prop.link_info_msg() ) == 0 ) return;
		DeviceBusinesscard left_dbc;
		if ( left_dbc.ParseFromString( l_info.left() ) == 0 ) return;
		DeviceBusinesscard right_dbc;
		if ( right_dbc.ParseFromString( l_info.right() ) == 0 ) return;
		Subgraph sg;
		*sg.add_devices() = left_dbc;
		*sg.add_devices() = right_dbc;
		*sg.add_links() = l_prom;
		_nm.mergeGraph(sg);
	}
}
