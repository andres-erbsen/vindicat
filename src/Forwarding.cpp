#include "Forwarding.h"

Forwarding::~Forwarding() {
	// There is no need to detatch this Forwarding, because it is destructed
	// exactly when there are no shared pointers to it anymore.
	// Let's remove the other one in this pair, if neccessary.
	if (auto other = _pair_other.lock() ) {
		other->detatch();
		other->_pair_other.reset();
	}
	// The other Forwarding in this pair should be destructed by now
	// It did not call this Forwarding's destructor again because we
	// reset the pointer and the if(.lock()) failed
}

uint32_t Forwarding::id() {
	return _id;
}



void ForeignForwarding::detatch() {
	auto owner = _owner.lock();
	assert(owner);
	owner->removeForwarding(id());
}

bool ForeignForwarding::forward_out(const std::string& packet) {
	auto owner = _owner.lock();
	assert(owner);
	return owner->send(packet);
}
