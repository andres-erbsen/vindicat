// vim: set ts=4 sw=4 :
#include "UDPServerTransport.h"
#include <cassert>


UDPServerSocket::
UDPServerSocket(UDPServerTransport& tr, const std::string& host, const std::string& port)
	: _tr(tr)
	, _host(host)
	, _port(port)
	{}

UDPServerSocket::~UDPServerSocket() {
	_tr._who.erase(_host + std::string(":") + _port);
}

void UDPServerSocket::send(const std::string& buf) {
	_tr._sock->sndto(buf.c_str(), buf.size(), _host, _port);
}



UDPServerTransport::
UDPServerTransport( const std::string& host
                  , const std::string& port)
                  : _host(host)
                  , _port(port)
                  {}

UDPServerTransport::~UDPServerTransport() {
	assert(_who.empty());
}

void UDPServerTransport::onPacket(packet_callback handler) {
	_handler = handler;
}

void UDPServerTransport::incoming() {
	// optmiize: with lower-level programming something lighter than strings
	delete this; // after call trs->useless() never touch trs again
	// http://www.parashift.com/c++-faq-lite/delete-this.html
	// could be used to identify connections.
	std::string addr, port, id, buf(1500,'\0'); // ethernet MTU size
	_sock->rcvfrom(buf,addr,port);
	id = addr + std::string(":") + port;

	std::shared_ptr<UDPServerSocket> s;
	auto it = _who.find(id);
	if ( it != _who.end() ) {
		s = it->second.lock();
		assert(s);
	} else {
		s.reset( new UDPServerSocket(*this,addr,port) );
		_who[id] = s;
	}
	_handler(s, buf);
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
	for (auto& kv : _who) {
		auto tsock_p = kv.second.lock();
		assert(tsock_p);
		tsock_p->send(buf);
	}
}
