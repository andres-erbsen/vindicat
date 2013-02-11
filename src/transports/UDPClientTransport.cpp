#include "UDPClientTransport.h"

#include <exception.hpp> // libsocket

#include <cassert>
#include <cerrno>

void UDPClientTransport::read_cb(ev::io &w, int revents) {
	// Callback for libev loop
	// Reads data and gives it to handler
	std::string buf(1500,'\0'); // ethernet MTU size
	try {
		_sock >> buf;
		_handler(std::bind(std::mem_fn(&UDPClientTransport::send), this, std::placeholders::_1), buf);
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
	: _sock(host, port, get_address_family(host.c_str()), SOCK_NONBLOCK)
	{}

void UDPClientTransport::onPacket(packet_callback handler) {
	_handler = handler;
}

void UDPClientTransport::enable() {
	_read_watcher.set<UDPClientTransport, &UDPClientTransport::read_cb>(this);
	_read_watcher.start(_sock.getfd(), ev::READ);
}

void UDPClientTransport::broadcast(const std::string& buf) {
	send(buf);
}

bool UDPClientTransport::send(const std::string& buf) {
	_sock.snd(buf.c_str(), buf.size());
	return true;
}
