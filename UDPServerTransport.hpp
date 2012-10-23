#include "Transport.hpp"
#define EV_STANDALONE 1
#include <ev++.h> // include before <inetclientdgram.hpp>

#include <inetserverdgram.hpp> // pollutes namespace?
// collision with libev variable name
#undef READ
#undef WRITE

#include <unordered_map>

class UDPServerTransport;
class UDPServerSocket : public TransportSocket {
public:
	UDPServerSocket(UDPServerTransport&, std::string, std::string);
	void send(const std::string&);
	void useless();
private:
	UDPServerTransport& _tr;
	std::string _host, _port;
};


class UDPServerTransport : public Transport {
public:
	friend class UDPServerSocket;
	UDPServerTransport(std::string, std::string);
	void onPacket(packet_callback);
	void enable();
	void broadcast(const std::string&);
    void read_cb(ev::io& w, int revents);
private:
	void incoming();
	std::string _host, _port;
	std::unordered_map< std::string,UDPServerSocket*> _who;
	packet_callback _handler;
	libsocket::inet_dgram_server* _sock;
    ev::io _read_watcher;
};
