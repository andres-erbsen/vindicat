// vim: set ts=4 sw=4 :
#include "UDPServerTransport.h"
#include <cassert>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <arpa/inet.h>

#include <cstring>
#include <iostream>

UDPServerTransport::
UDPServerTransport( const std::string& host
                  , const std::string& port):
	_advert(host+':'+port)
{
	struct addrinfo hints, *res;
        std::memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_DGRAM;
        int err;
        if((err=getaddrinfo(host.c_str(), port.c_str(), &hints, &res)) != 0)
        {
		if(err == EAI_SYSTEM)
			std::perror("UDPServerTransport::UDPServerTransport");
else
		{
			std::cerr << "UDPServerTransport::UDPServerTransport: ";
			std::cerr << gai_strerror(err) << std::endl;
		}
		std::abort();
	}

	for(auto i = res; i != nullptr; i = i->ai_next)
	{
		if((_fd = socket(i->ai_addr->sa_family, SOCK_DGRAM | SOCK_NONBLOCK, 0)) == -1)
			continue;
		if(bind(_fd, i->ai_addr, i->ai_addrlen) == -1)
		{
			close(_fd);
			_fd = -1;
			continue;
		}

		if(i->ai_addr->sa_family == AF_INET)
		{
			struct sockaddr_in *group = new struct sockaddr_in;
			std::memset(group, 0, sizeof(sockaddr_in));
			group->sin_family = AF_INET;
			// Multicast address reserved for experimentation
			group->sin_addr.s_addr = inet_addr("224.0.0.254");
			group->sin_port = htons(30307);
			_group = reinterpret_cast<struct sockaddr*>(group);
			_group_length = sizeof(struct sockaddr_in);
		}
		else if(i->ai_addr->sa_family == AF_INET6)
		{
			struct sockaddr_in6 *group = new struct sockaddr_in6;
			std::memset(group, 0, sizeof(sockaddr_in6));
			group->sin6_family = AF_INET6;
			// An unused link-local multicast address
			inet_pton(AF_INET6, "ff02::dc", group->sin6_addr.s6_addr);
			group->sin6_port = htons(30307);
			_group = reinterpret_cast<struct sockaddr*>(group);
			_group_length = sizeof(struct sockaddr_in6);
		}

		if(setsockopt(_fd, IPPROTO_IP, IP_MULTICAST_IF, i->ai_addr, i->ai_addrlen) == -1)
		{
			close(_fd);
			continue;
		}

		break;
	}
	
	freeaddrinfo(res);

	if(_fd == -1)
	{
		std::perror("UDPServerTransport::UDPServerTransport");
		std::abort();
	}
}

UDPServerTransport::~UDPServerTransport() {
	close(_fd);
}

void UDPServerTransport::onPacket(packet_callback handler) {
	_handler = handler;
}

bool UDPServerTransport::send(const std::string& buf, const sockaddr *addr, socklen_t addrlen)
{
	return sendto(_fd, buf.c_str(), buf.size(), 0, addr, addrlen) != -1;
}

void UDPServerTransport::incoming() {
	char *buf = new char[1500]; // ethernet MTU size
	struct sockaddr *addr = reinterpret_cast<struct sockaddr*>(new sockaddr_storage);
	socklen_t addrlen = sizeof(struct sockaddr_storage);
	ssize_t read = recvfrom(_fd, buf, 1500, 0, addr, &addrlen);
	if(read < 0)
	{
		std::perror("UDPServerTransport::incoming");
		std::abort();
	}
	auto iter = _who.insert(std::make_pair(addr, addrlen));

	_handler(std::bind(std::mem_fn(&UDPServerTransport::send), this, std::placeholders::_1, addr, addrlen), std::string(buf, read));

	if(!iter.second)
		delete addr;
	delete[] buf;
}

void UDPServerTransport::enable() {
	// setup libev for this transport here with the incoming() from above
    _read_watcher.set <UDPServerTransport, &UDPServerTransport::read_cb> (this);
	_read_watcher.start (_fd, ev::READ);
}

void UDPServerTransport::read_cb(ev::io &w, int revents) {
	// Callback for libev loop
	// Calls incoming()
	UDPServerTransport::incoming();
}

void UDPServerTransport::broadcast(const std::string& buf) {
	for (auto& kv : _who)
		send(buf, kv.first, kv.second);
	sendto(_fd, _advert.c_str(), _advert.size(), 0, _group, _group_length);
}
