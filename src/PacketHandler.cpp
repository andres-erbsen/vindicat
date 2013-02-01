#include "PacketHandler.h"

#include "vindicat.pb.h"
#include "Device.h"

#include <iostream>

#include <cstdint>

void PacketHandler::operator()(std::shared_ptr<TransportSocket> trs, const std::string& packet) {
	if (packet.size() == 0) {
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
	}
}
