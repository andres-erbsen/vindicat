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
	void enable();
	void broadcast(const std::string&);
	bool send(const std::string&);
	const struct sockaddr* address() const
	{
		return _addr;
	}
private:
	void read_cb(ev::io&, int);
	friend ev::io;
	int _fd;
	ev::io _read_watcher;
	struct sockaddr* _addr;
	socklen_t _addrlen;
	std::string _addr_UID;
};


#endif // UDPCLIENTTRANSPORT_H_
