#include "Forwarding.h"

void Forwarding::pair( std::shared_ptr<Forwarding> l
                     , std::shared_ptr<Forwarding> r ) {
	l->_pair_other = r;
	r->_pair_other = l;
}

Forwarding::Forwarding(uint64_t id) : _id(id) {}

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

uint64_t Forwarding::id() {
	return _id;
}

ForeignForwarding::
ForeignForwarding( NetworkMap& nm
                 , uint64_t id)
	: Forwarding(id)
    , _nm(nm)
    {}

bool Forwarding::forward(const std::string& packet) {
    auto pair_other = _pair_other.lock();
    assert(pair_other);
    return pair_other->forward_out(packet);
}

void ForeignForwarding::owner(std::weak_ptr<Device>&& owner) {
	assert(!_owner.lock());
	_owner = owner;
}


void ForeignForwarding::detatch() {
	auto owner = _owner.lock();
	assert(owner);
	owner->removeForwarding(id());
}


SimpleForwarding::
SimpleForwarding( NetworkMap& nm
                , uint64_t id)
    : ForeignForwarding(nm, id)
    {}

bool SimpleForwarding::forward_out(const std::string& packet) {
    auto owner = _owner.lock();
    assert(owner);
    // Get transportsocket to owner from NetworkMap
    auto tsock = _nm.tsock_to(owner->id());
    // Send to tsock
    return tsock(packet);
}


bool NoForwarding::forward_out(const std::string& packet) {
    (void)packet;
    return false;
}

