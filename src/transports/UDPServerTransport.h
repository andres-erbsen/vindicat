#ifndef UDPSERVERTRANSPORT_H_
#define UDPSERVERTRANSPORT_H_

#include "Transport.h"
#include <ev++.h>
#include <set>
#include <utility>
#include <memory>

#include <sys/socket.h>

class UDPServerTransport : public Transport {
public:
	UDPServerTransport(const std::string&, const std::string&);
	virtual ~UDPServerTransport();

	void onPacket(packet_callback);
	void enable();
	void broadcast(const std::string&);
    void read_cb(ev::io& w, int revents);
    bool send(const std::string&, const struct sockaddr*, socklen_t);

    class compare
    {
	    public:
    	bool operator()(const std::pair<struct sockaddr*, socklen_t>&,
		    const std::pair<struct sockaddr*, socklen_t>&);
    };
private:
	void incoming();
	std::set<std::pair<struct sockaddr*,socklen_t>, compare> _who;
	// UDPv6 shares port space with UDPv4
	struct sockaddr* _group[2];
	socklen_t _group_length[2];
	packet_callback _handler;
	int _fd;
    ev::io _read_watcher;
};

#endif // UDPSERVERTRANSPORT_H_
