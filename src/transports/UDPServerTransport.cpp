// vim: set ts=4 sw=4 :
#include "UDPServerTransport.h"
#include "UDPClientTransport.h"
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
UDPServerTransport(UDPClientTransport *clients, const std::string& host
                  , const std::string& port)
	: _clients(clients)
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
	delete _group[0];
	delete _group[1];
	close(_fd);
}

void UDPServerTransport::enable() {
  _clients->enable(_read_watcher, _fd);
}

void UDPServerTransport::to_unknown(const std::string&) {
  sendto(_fd, nullptr, 0, 0, _group[0], _group_length[0]);
  if(_group[1])
    sendto(_fd, nullptr, 0, 0, _group[1], _group_length[1]);
}
