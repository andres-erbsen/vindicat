// vim: set ts=4 sw=4 :
#include "UDPServerTransport.h"
#include <cassert>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <net/if.h>

#include <cstring>
#include <iostream>

UDPServerTransport::
UDPServerTransport( const std::string& host
                  , const std::string& port)
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
			_group[0] = reinterpret_cast<struct sockaddr*>(group);
			_group_length[0] = sizeof(struct sockaddr_in);
			_group[1] = nullptr;

			{
				int loop = 0;
				setsockopt(_fd, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof(int));
			}
			
			if(setsockopt(_fd, IPPROTO_IP, IP_MULTICAST_IF,
			    &reinterpret_cast<struct sockaddr_in*>(i->ai_addr)->sin_addr, sizeof(struct in_addr)) == -1)
			{
				close(_fd);
				_fd = -1;
				delete _group[0];
				continue;
			}
		}
		else if(i->ai_addr->sa_family == AF_INET6)
		{
			struct sockaddr_in6 *group = new struct sockaddr_in6;
			std::memset(group, 0, sizeof(sockaddr_in6));
			group->sin6_family = AF_INET6;
			// An unused link-local multicast address
			inet_pton(AF_INET6, "ff02::dc", group->sin6_addr.s6_addr);
			group->sin6_port = htons(30307);
			_group[0] = reinterpret_cast<struct sockaddr*>(group);
			_group_length[0] = sizeof(struct sockaddr_in6);

			// UDPv4 clients can connect to UDPv6 sockets
			group = new struct sockaddr_in6;
			std::memset(group, 0, sizeof(sockaddr_in6));
			group->sin6_family = AF_INET6;
			inet_pton(AF_INET6, "::ffff:224.0.0.254", group->sin6_addr.s6_addr);
			group->sin6_port = htons(30307);
			_group[1] = reinterpret_cast<struct sockaddr*>(group);
			_group_length[1] = sizeof(struct sockaddr_in6);

			{
				int loop = 0;
				setsockopt(_fd, IPPROTO_IPV6, IPV6_MULTICAST_LOOP, &loop, sizeof(int));
			}

			if(setsockopt(_fd, IPPROTO_IPV6, IPV6_MULTICAST_IF,
			    &reinterpret_cast<struct sockaddr_in6*>(i->ai_addr)->sin6_addr, sizeof(struct in6_addr)) == -1)
			{
				delete _group[0];
				delete _group[1];
				close(_fd);
				_fd = -1;
				continue;
			}
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

bool UDPServerTransport::send(const std::string& buf, const std::shared_ptr<sockaddr>& addr, socklen_t addrlen)
{
	return sendto(_fd, buf.c_str(), buf.size(), 0, addr.get(), addrlen) != -1;
}

void UDPServerTransport::incoming() {
	char *buf = new char[1500]; // ethernet MTU size
	std::shared_ptr<sockaddr> addr(reinterpret_cast<struct sockaddr*>(new sockaddr_storage));
	socklen_t addrlen = sizeof(struct sockaddr_storage);
	ssize_t read = recvfrom(_fd, buf, 1500, 0, addr.get(), &addrlen);
	if(read < 0)
	{
		std::perror("UDPServerTransport::incoming");
		std::abort();
	}
	_who.insert(std::make_pair(addr, addrlen));

	std::string addr_UID;
	if(addr->sa_family == AF_INET)
		addr_UID = "IPv4"+std::string(reinterpret_cast<char*>(&reinterpret_cast<sockaddr_in*>(addr.get())->sin_addr.s_addr), 4)+std::string(reinterpret_cast<char*>(&reinterpret_cast<sockaddr_in*>(addr.get())->sin_port), 2);
	else if(addr->sa_family == AF_INET6)
		addr_UID = "IPv6"+std::string(reinterpret_cast<char*>(reinterpret_cast<sockaddr_in6*>(addr.get())->sin6_addr.s6_addr), 16)+std::string(reinterpret_cast<char*>(&reinterpret_cast<sockaddr_in6*>(addr.get())->sin6_port), 2);
	else
		addr_UID = "DGRAM"+std::string(reinterpret_cast<char*>(addr.get()), addrlen);

	_receive_cb(TransportSocket(std::bind(std::mem_fn(&UDPServerTransport::send), this, std::placeholders::_1, addr, addrlen), addr_UID), std::string(buf, read));

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
	sendto(_fd, nullptr, 0, 0, _group[0], _group_length[0]);
	if(_group[1])
		sendto(_fd, nullptr, 0, 0, _group[1], _group_length[1]);
}

bool UDPServerTransport::compare::operator()(const std::pair<std::shared_ptr<sockaddr>, socklen_t>& a, const std::pair<std::shared_ptr<sockaddr>, socklen_t>& b)
{
	if(a.first->sa_family != b.first->sa_family)
		return a.first->sa_family < b.first->sa_family;
	if(a.first->sa_family == AF_INET) // IPv4
		return reinterpret_cast<struct sockaddr_in*>(a.first.get())->sin_addr.s_addr < reinterpret_cast<struct sockaddr_in*>(b.first.get())->sin_addr.s_addr;
	if(a.first->sa_family == AF_INET6) // IPv6
		return std::memcmp(reinterpret_cast<struct sockaddr_in6*>(a.first.get())->sin6_addr.s6_addr, reinterpret_cast<struct sockaddr_in6*>(b.first.get())->sin6_addr.s6_addr, sizeof(in6_addr)) < 0;
	return a < b;
}
