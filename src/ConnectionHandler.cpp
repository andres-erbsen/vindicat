#include "ConnectionHandler.h"

void ConnectionHandler::addInterface(std::unique_ptr<Interface>&& iface) {
	_ifaces.push_back( std::move(iface) );
}

void ConnectionHandler::operator() (const std::string& from, std::uint8_t protocol, const std::string& payload) {
	for ( const auto& iface : _ifaces) {
		if ( iface->match(from, protocol, payload) ) {
			iface->send(from, protocol, payload);
		}
	}
}
