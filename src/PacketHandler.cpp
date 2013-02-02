#include "PacketHandler.h"
#include "Link.h"
#include "vindicat.pb.h"

#include <iostream>
#include <cstdint>

PacketHandler::PacketHandler(NetworkMap& nm)
	: _nm(nm)
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
	std::shared_ptr<Device> left_device  = nm.device(info->left() ).lock();
	std::shared_ptr<Device> right_device = nm.device(info->right()).lock();
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
	} else std::shared_ptr<Link>();
}


void PacketHandler::operator()(std::shared_ptr<TransportSocket> ts, const std::string& packet) {
	if (packet.size() == 0) {
		return;
	}
	uint8_t tag = packet[0];
	if (tag == 0) {
	} else if (tag == 4) { // Beacon packet
		auto card = std::make_shared<DeviceBusinesscard>();
		if ( ! card->ParseFromArray( packet.data()+1
		                           , packet.size()-1 ) ) return;
		auto dev = std::make_shared<Device>();
		if ( ! dev->parseFrom( std::move(card) ) ) return;
		std::cout << "received and parsed a card :)" << std::endl;

		_nm.add( std::move(dev) );
	}
}
