#include "UDPClientTransport.h"

#include <cassert>
#include <cerrno>
#include <cstring>

#include <iostream>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>

void UDPClientTransport::read_cb(ev::io &w, int revents) {
	// Callback for libev loop
	// Reads data and gives it to handler
	char *buf = new char[1500]; // ethernet MTU size
	ssize_t read = recv(_fd, buf, 1500, 0);
	if(read == -1 && errno != ECONNREFUSED)
	{
		std::perror("UDPClientTransport::read_cb");
		std::abort();
	}
	_handler(std::bind(std::mem_fn(&UDPClientTransport::send), this, std::placeholders::_1), std::string(buf, read));
	delete[] buf;
}

UDPClientTransport::
UDPClientTransport(const std::string& host, const std::string& port):
	_fd(-1)
{
	struct addrinfo hints, *res;
	std::memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_DGRAM;
	int err;
	if((err=getaddrinfo(host.c_str(), port.c_str(), &hints, &res)) != 0)
	{
		if(err == EAI_SYSTEM)
			std::perror("UDPClientTransport::UDPClientTransport");
		else
			std::cerr << "UDPClientTransport::UDPClientTransport: " << gai_strerror(err) << std::endl;
		std::abort();
	}

	_fd = socket(res->ai_addr->sa_family, SOCK_DGRAM | SOCK_NONBLOCK, 0);
	if(_fd == -1)
	{
		std::perror("UDPClientTransport::UDPClientTransport");
		std::abort();
	}
	
	_addr = reinterpret_cast<struct sockaddr*>(new struct sockaddr_storage);
	std::memcpy(_addr, res->ai_addr, res->ai_addrlen);
	_addrlen = res->ai_addrlen;

	freeaddrinfo(res);
}

UDPClientTransport::UDPClientTransport(struct sockaddr *addr, socklen_t addrlen):
	_fd(socket(addr->sa_family, SOCK_DGRAM | SOCK_NONBLOCK, 0))
{
	if(_fd == -1)
	{
		std::perror("UDPClientTransport::UDPClientTransport");
		std::abort();
	}
	_addr = reinterpret_cast<struct sockaddr*>(new struct sockaddr_storage);
	std::memcpy(_addr, addr, addrlen);
	_addrlen = addrlen;
}

UDPClientTransport::~UDPClientTransport()
{
	close(_fd);
}

void UDPClientTransport::onPacket(packet_callback handler) {
	_handler = handler;
}

void UDPClientTransport::enable() {
	_read_watcher.set<UDPClientTransport, &UDPClientTransport::read_cb>(this);
	_read_watcher.start(_fd, ev::READ);
}

void UDPClientTransport::broadcast(const std::string& buf) {
	send(buf);
}

bool UDPClientTransport::send(const std::string& buf) {
	return sendto(_fd, buf.c_str(), buf.size(), 0, _addr, _addrlen) != -1;
}
