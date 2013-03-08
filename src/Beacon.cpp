#include "Beacon.h"

Beacon::Beacon( int interval
              , const CryptoIdentity& ci
              , const std::vector<Transport*>& v_tr)
              : _msg("\4\1")
              , _transports(v_tr)
{
	ci.our_businesscard()->AppendToString(&_msg);
	_w.set(0.00001,interval);
	_w.set(this);
}

void Beacon::enable() {
	_w.start();
}

void Beacon::operator() (ev::timer& /*w*/, int /*revents*/) {
	for (auto tr : _transports) tr->to_unknown(_msg);
}
