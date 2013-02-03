#ifndef UDPCLIENTTRANSPORT_H_
#define UDPCLIENTTRANSPORT_H_

#include "Transport.h"
#include <ev++.h> // include before <inetclientdgram.hpp>

#include <memory>
#include <inetclientdgram.hpp> // pollutes namespace?

class UDPClientSocket : public TransportSocket, public std::enable_shared_from_this<UDPClientSocket> {
public:
	UDPClientSocket(packet_callback, const std::string&, const std::string&);

	void send(const std::string&);
	void read_cb(ev::io& w, int revents);

private:
	libsocket::inet_dgram_client _sock;
	ev::io _read_watcher;
	packet_callback _handler;
};


class UDPClientTransport : public Transport {
public:
	UDPClientTransport(const std::string& host, const std::string& port = std::string("30307"));
	void onPacket(packet_callback);
	void enable();
	void broadcast(const std::string&);
private:
	std::string _host, _port;
	packet_callback _handler;
	std::shared_ptr<UDPClientSocket> _trs;
};


#endif // UDPCLIENTTRANSPORT_H_
