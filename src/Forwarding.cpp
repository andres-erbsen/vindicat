#include "Forwarding.h"

Forwarding::~Forwarding() {
	// There is no need to detatch this Forwarding, because it is destructed
	// exactly when there are no shared pointers to it anymore.
	// Let's remove the other one in this pair, if neccessary.
	if (auto other = _pair_other.lock() ) {
		other->detatch();
		other->_pair_other.reset();
		// shared_ptr other leaves scope
	}
	// The other Forwarding in this pair should be destructed by now
	// It did not call this Forwarding's destructor again because we
	// reset the pointer and the if(.lock()) failed
}

uint32_t Forwarding::id() {
	return _id;
}

bool Forwarding::forward(const std::string& packet) {
    auto pair_other = _pair_other.lock();
    assert(pair_other);
    return pair_other->forward_out(packet);
}


void ForeignForwarding::detatch() {
	auto owner = _owner.lock();
	assert(owner);
	owner->removeForwarding(id());
}


SimpleForwarding::SimpleForwarding(NetworkMap& nm)
    : _nm(nm)
    {}

bool SimpleForwarding::forward_out(const std::string& packet) {
    auto owner = _owner.lock();
    assert(owner);
    // Get pointer to transportsocket to owner from NetworkMap
    auto tsock = _nm.tsock_to(owner->id()).lock();
    // Return false if no pointer to transportsocket
    if (!tsock) return false;
    // Send to tsock
    tsock->send(packet);
    return true;
}


bool NoForwarding::forward_out(const std::string& packet) {
    (void)packet;
    return false;
}


void UpForwarding::detach() {
    assert(0); // TODO
}

bool UpForwarding::forward_out(const std::string& packet) {
    assert(0); // TODO
}
