#ifndef UDPSERVERTRANSPORT_H_
#define UDPSERVERTRANSPORT_H_

#include "Transport.h"
#include <ev++.h> // include before <inetclientdgram.hpp>
#include <inetserverdgram.hpp> // pollutes namespace?
#include <set>
#include <utility>
#include <memory>

class UDPServerTransport : public Transport {
public:
	UDPServerTransport(const std::string&, const std::string&);
	virtual ~UDPServerTransport() = default;

	void onPacket(packet_callback);
	void enable();
	void broadcast(const std::string&);
    void read_cb(ev::io& w, int revents);
    bool send(const std::string&, const std::string&, const std::string&);

private:
	void incoming();
	std::string _host, _port;
	std::set<std::pair<std::string, std::string>> _who;
	packet_callback _handler;
	std::unique_ptr<libsocket::inet_dgram_server> _sock;
    ev::io _read_watcher;
};

#endif // UDPSERVERTRANSPORT_H_
