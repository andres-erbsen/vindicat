#ifndef UDPSERVERTRANSPORT_H_
#define UDPSERVERTRANSPORT_H_

#include "Transport.h"
#include <ev++.h> // include before <inetclientdgram.hpp>
#include <inetserverdgram.hpp> // pollutes namespace?
#include <unordered_map>
#include <memory>

class UDPServerTransport;
class UDPServerSocket : public TransportSocket {
public:
	UDPServerSocket(UDPServerTransport&, const std::string&, const std::string&);
	virtual ~UDPServerSocket();

	void send(const std::string&);

private:
	UDPServerTransport& _tr;
	std::string _host, _port;
};


class UDPServerTransport : public Transport {
public:
	friend class UDPServerSocket;
	UDPServerTransport(const std::string&, const std::string&);
	virtual ~UDPServerTransport();

	void onPacket(packet_callback);
	void enable();
	void broadcast(const std::string&);
    void read_cb(ev::io& w, int revents);

private:
	void incoming();
	std::string _host, _port;
	std::unordered_map< std::string, std::weak_ptr<UDPServerSocket> > _who;
	packet_callback _handler;
	std::unique_ptr<libsocket::inet_dgram_server> _sock;
    ev::io _read_watcher;
};

#endif // UDPSERVERTRANSPORT_H_
