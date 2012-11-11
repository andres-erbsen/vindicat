#include "UDPClientTransport.hpp"

#include <exception.hpp> // libsocket

#include <errno.h>

UDPClientSocket::
UDPClientSocket(packet_callback handler, std::string host, std::string port)
	: _sock(host,port,get_address_family(host.c_str()),SOCK_NONBLOCK)
	, _handler(handler)
	{
	// Setup libev for this socket
	_read_watcher.set <UDPClientSocket, &UDPClientSocket::read_cb> (this);
	_read_watcher.start (_sock.getfd(), ev::READ);
    // Give dummy packet to handler
	_handler(this, "");
    }

void UDPClientSocket::send(const std::string& buf) {
	_sock.snd(buf.c_str(), buf.size()); // should not throw
}

void UDPClientSocket::useless() {
	// This socket was manually specified by user,
	// so no automagic calling this useless is correct.
	// Let's just push it pack to the handshake loop.
	// Let's pretend we received a lot of nothing!
	//  ... which is actually correct
	_handler(this,std::string("")); // TODO: launch this as an idle event instead
}

void UDPClientSocket::read_cb(ev::io &w, int revents) {
	// Callback for libev loop
	// Reads data and gives it to handler
	std::string buf(1500,'\0'); // ethernet MTU size
	try {
		_sock >> buf;
		_handler(this, buf);
	} catch (libsocket::socket_exception exc) {
		if ( exc.err == ECONNREFUSED) {} // not found? Just wait.
		else {
			std::cerr << exc.mesg;
			throw exc;
		}
	}
}




UDPClientTransport::
UDPClientTransport(std::string host, std::string port)
	: _host(host)
	, _port(port)
	{}

void UDPClientTransport::onPacket(packet_callback handler) {
	_handler = handler;
}

void UDPClientTransport::enable() {
	_trs = new UDPClientSocket(_handler,_host,_port);
}

void UDPClientTransport::broadcast(const std::string& buf) {
	_trs->send(buf);
}
