// vim: set ts=4 sw=4 :
#include "UDPServerTransport.h"
#include <cassert>

UDPServerTransport::
UDPServerTransport( const std::string& host
                  , const std::string& port)
                  : _host(host)
                  , _port(port)
                  {}

void UDPServerTransport::onPacket(packet_callback handler) {
	_handler = handler;
}

bool UDPServerTransport::send(const std::string& buf, const std::string& host, const std::string& port)
{
	_sock->sndto(buf.c_str(), buf.size(), host, port);
	return true;
}

void UDPServerTransport::incoming() {
	// optmiize: with lower-level programming something lighter than strings
	// could be used to identify connections.
	std::string addr, port, buf(1500,'\0'); // ethernet MTU size
	_sock->rcvfrom(buf,addr,port);
	_who.insert(std::make_pair(addr, port));

	_handler(std::bind(std::mem_fn(&UDPServerTransport::send), this, std::placeholders::_1, addr, port), buf);
}

void UDPServerTransport::enable() {
	_sock.reset( new libsocket::inet_dgram_server(_host,_port,LIBSOCKET_BOTH,SOCK_NONBLOCK) );
	// setup libev for this transport here with the incoming() from above
    _read_watcher.set <UDPServerTransport, &UDPServerTransport::read_cb> (this);
	_read_watcher.start (_sock->getfd(), ev::READ);
}

void UDPServerTransport::read_cb(ev::io &w, int revents) {
	// Callback for libev loop
	// Calls incoming()
	UDPServerTransport::incoming();
}

void UDPServerTransport::broadcast(const std::string& buf) {
	for (auto& kv : _who)
		send(buf, kv.first, kv.second);
}
