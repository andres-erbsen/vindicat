#include "UDPClientTransport.h"

#include <exception.hpp> // libsocket

#include <cassert>
#include <cerrno>

UDPClientSocket::
UDPClientSocket(packet_callback handler, const std::string& host, const std::string& port)
	: _sock(host,port,get_address_family(host.c_str()),SOCK_NONBLOCK)
	, _handler(handler)
	{
	// Setup libev for this socket
	_read_watcher.set <UDPClientSocket, &UDPClientSocket::read_cb> (this);
	_read_watcher.start (_sock.getfd(), ev::READ);
    }

void UDPClientSocket::send(const std::string& buf) {
	_sock.snd(buf.c_str(), buf.size()); // should not throw
}

void UDPClientSocket::read_cb(ev::io &w, int revents) {
	// Callback for libev loop
	// Reads data and gives it to handler
	std::string buf(1500,'\0'); // ethernet MTU size
	try {
		_sock >> buf;
		_handler(shared_from_this(), buf);
	} catch (libsocket::socket_exception exc) {
		if ( exc.err == ECONNREFUSED) {} // not found? Just wait.
		else {
			std::cerr << exc.mesg;
			throw exc;
		}
	}
}




UDPClientTransport::
UDPClientTransport(const std::string& host, const std::string& port)
	: _host(host)
	, _port(port)
	{}

void UDPClientTransport::onPacket(packet_callback handler) {
	_handler = handler;
}

void UDPClientTransport::enable() {
	assert(!_trs);
	_trs.reset( new UDPClientSocket(_handler,_host,_port) );
}

void UDPClientTransport::broadcast(const std::string& buf) {
	_trs->send(buf);
}
