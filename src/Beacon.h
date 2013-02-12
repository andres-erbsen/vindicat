#ifndef BEACON_H_
#define BEACON_H_

#include "CryptoIdentity.h"
#include "Transport.h"

#include <ev++.h>

#include <string>
#include <vector>

class Beacon {
public:
	Beacon(int, const CryptoIdentity&, const std::vector<Transport*>&);
	void enable();
	void operator()(ev::timer &w, int revents);
private:
	std::string _msg;
	const std::vector<Transport*> &_transports;
	ev::timer _w;
};


#endif // BEACON_H_
