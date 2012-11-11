#include "CryptoIdentity.hpp"
#include "Transport.hpp"

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
	std::vector<Transport*> _transports;
	ev::timer _w;
};
