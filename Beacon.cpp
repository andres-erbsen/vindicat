#include "Beacon.hpp"

Beacon::Beacon( int interval
              , const CryptoIdentity& ci
              , const std::vector<Transport*>& v_tr)
              : _transports(v_tr)
              , _msg("\4")
{
	DeviceBusinesscard card;
	ci.our_businesscard(card);
	card.AppendToString(&_msg);
	_w.set(0.00001,interval);
	_w.set(this);
}

void Beacon::enable() {
	_w.start();
}

void Beacon::operator() (ev::timer &w, int revents) {
	for (auto tr : _transports) tr->broadcast(_msg);
}
