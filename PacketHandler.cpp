// vim: set ts=4 sw=4 :
#include "PacketHandler.hpp"
#include "vindicat.pb.h"

PacketHandler::PacketHandler( NetworkMap& nm
	                        , CryptoIdentity& ci
	                        , std::vector<Transport*>& transports) 
	: _nm(nm)
	, _crypto_identity(ci)
	, _transports(transports)
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
		if ( _nm.mergeGraph(sg) ) { // was interesting enough for us to spread it
			broadcast(packet);
		}
	} else if (tag == 3) {
		// LinkProposal
		LinkProposal l_prop;
		if ( l_prop.ParseFromArray( packet.data()+1
								  , packet.size()-1 ) == 0 ) return;
		
        // Make sure there are no unknown fields in l_prop
        if ( ! understandEverything(l_prop) ) return;
        
        DeviceBusinesscard left_card;
		{ // Get DeviceBusinesscard of left device
        	LinkInfo link_unchecked;
        	if ( ! link_unchecked.ParseFromString(l_prop.link_info_msg()) ) return;
        	if ( ! _nm.getDeviceBusinesscard(link_unchecked.left(), left_card) ) return;
		}
        
        // Verify signatures
        DeviceInfo ldev;
        LinkInfo link;
        if ( ! verify(left_card, ldev) ) return;
        if ( ! _crypto_identity.verifyProposal(l_prop, ldev, link) ) return;
        
        // IMPORTANT
        // Before continuing beyond this point,
        // it's wise to determine if all the fields in l_prop are ok by us.
        // Right now, there's nothing to check.
        
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
        _crypto_identity.our_businesscard(*sg.add_devices()); // our businesscard
        *sg.add_links() = l_prom;
        
        // Add to map
        _nm.mergeGraph(sg);
        
        // Broadcast
        std::string sg_packet = "\0x02";
        if ( ! sg.AppendToString(&sg_packet) ) assert(0);
        broadcast(sg_packet);
        
    } else if (tag == 4) { // Beacon packet
		// optimize: cache beacon packets, do not parse+verify again if known
		DeviceBusinesscard card;
		if ( ! card.ParseFromArray( packet.data()+1, packet.size()-1 ) ) return;
		_nm.beacon(trs, card);
	}
}

void PacketHandler::broadcast(const std::string& packet) const {
	for (auto transport : _transports) {
		transport->broadcast(packet);
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
