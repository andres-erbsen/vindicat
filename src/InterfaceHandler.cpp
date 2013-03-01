#include "InterfaceHandler.h"
#include "Connection.h"

#include <iostream>

InterfaceHandler::InterfaceHandler
	(CryptoIdentity& ci, NetworkMap& nm, ConnectionPool& cp, Interface& iface)
	: _ci(ci)
	, _nm(nm)
	, _cp(cp)
	, _if(iface)
	{}

static std::string hex(const std::string& input) {
    static const char* const lut = "0123456789abcdef";
    size_t len = input.length();

    std::string output;
    output.reserve(2 * len);
    for (size_t i = 0; i < len; ++i)
    {
        const unsigned char c = input[i];
        output.push_back(lut[c >> 4]);
        output.push_back(lut[c & 15]);
    }
    return output;
}

void InterfaceHandler::operator()(std::string&& to, std::string&& packet) {
	std::cerr << "To: " << hex(to) << " (" << packet.size() << " bytes)\n"
		<< packet << "\n" << std::endl;
	auto it = _cp.find(to);
	if (it != _cp.end() ) {
		it->second->forward(packet);
	} else {
		Path path;
		{
			auto dst_dev = _nm.device(to);
			if (!dst_dev) return;
			path = _nm.path_to(*dst_dev);
		}

		auto conn = std::make_shared<Connection>(_ci, path, _cp, _if);
		
		auto rfwd = std::make_shared<SimpleForwarding>(_nm, conn->id());
		auto next_dev = std::get<1>(path.at(0)).lock();
		next_dev->addForwarding(rfwd);

		Forwarding::pair(conn, rfwd);

		_cp.insert( std::make_pair(to, conn) );
		conn->request();
	}
}
