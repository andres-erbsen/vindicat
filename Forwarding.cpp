#include "Forwarding.hpp"


Forwarding::Forwarding(uint32_t d, std::string tdev)
	: id(id), to_device_id(tdev) {}


bool SimpleForwarding::forward(TransportSocket* trs, std::string packet) {
	trs->send(packet);
	return 1;
}


TwowayForwarding::TwowayForwarding(uint32_t d, std::string tdev)
	: id(id), to_device_id(tdev) {}

const lemon::ListGraph::Node&
TwowayForwarding::dst(const lemon::ListGraph::Node& src) const {
	if (src == rq_owner) return other_owner;
	if (src == other_owner) return rq_owner;
	return src;
}



OnewayForwarding::OnewayForwarding(uint32_t d, std::string tdev)
	: id(id), to_device_id(tdev) {}

const lemon::ListGraph::Node&
OnewayForwarding::dst(const lemon::ListGraph::Node& src) const {
	if (src == rq_owner) return other_owner;
	return src;
}

