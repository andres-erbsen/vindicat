#include "Transport.hpp"
#include <inetclientdgram.hpp>

class UDPClientSocket : public TransportSocket {
public:
	UDPClientSocket(packet_callback,std::string, std::string);
	void send(const std::string&);
	void useless();
private:
	libsocket::inet_dgram_client _sock;
	packet_callback _handler;
};

class UDPClientTransport : public Transport {
public:
	UDPClientTransport(std::string, std::string);
	void onPacket(packet_callback);
	void enable();
	void broadcast(const std::string&);
private:
	std::string _host, _port;
	packet_callback _handler;
	UDPClientSocket* _trs;
};
