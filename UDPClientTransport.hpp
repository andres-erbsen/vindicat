#include "Transport.hpp"
#define EV_STANDALONE 1
#include <ev++.h> // include before <inetclientdgram.hpp>

#include <inetclientdgram.hpp> // pollutes namespace?
// collision with libev variable name
#undef READ
#undef WRITE


class UDPClientSocket : public TransportSocket {
public:
	UDPClientSocket(packet_callback,std::string, std::string);
	void send(const std::string&);
	void useless();
	void read_cb(ev::io& w, int revents);
private:
	libsocket::inet_dgram_client _sock;
	ev::io _read_watcher;
	packet_callback _handler;
};

class UDPClientTransport : public Transport {
public:
	UDPClientTransport(std::string host, std::string port = std::string("30307"));
	void onPacket(packet_callback);
	void enable();
	void broadcast(const std::string&);
private:
	std::string _host, _port;
	packet_callback _handler;
	UDPClientSocket* _trs;
};
