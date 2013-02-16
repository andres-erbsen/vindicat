#ifndef UDPCLIENTTRANSPORT_H_
#define UDPCLIENTTRANSPORT_H_

#include "Transport.h"
#include <ev++.h>

#include <sys/socket.h>

class UDPClientTransport : public Transport {
public:
	UDPClientTransport(const std::string& host, const std::string& port = std::string("30307"));
	UDPClientTransport(struct sockaddr *addr, socklen_t addrlen);
	~UDPClientTransport();
	void onPacket(packet_callback);
	void enable();
	void broadcast(const std::string&);
	bool send(const std::string&);
	void read_cb(ev::io& w, int revents);
	const struct sockaddr* address() const
	{
		return _addr;
	}
private:
	packet_callback _handler;
	int _fd;
	ev::io _read_watcher;
	struct sockaddr* _addr;
	socklen_t _addrlen;
	std::string _addr_UID;
};


#endif // UDPCLIENTTRANSPORT_H_
