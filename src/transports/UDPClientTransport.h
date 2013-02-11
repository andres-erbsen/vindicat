#ifndef UDPCLIENTTRANSPORT_H_
#define UDPCLIENTTRANSPORT_H_

#include "Transport.h"
#include <ev++.h> // include before <inetclientdgram.hpp>

#include <memory>
#include <inetclientdgram.hpp> // pollutes namespace?

class UDPClientTransport : public Transport {
public:
	UDPClientTransport(const std::string& host, const std::string& port = std::string("30307"));
	void onPacket(packet_callback);
	void enable();
	void broadcast(const std::string&);
	bool send(const std::string&);
	void read_cb(ev::io& w, int revents);
private:
	packet_callback _handler;
	libsocket::inet_dgram_client _sock;
	ev::io _read_watcher;
};


#endif // UDPCLIENTTRANSPORT_H_
