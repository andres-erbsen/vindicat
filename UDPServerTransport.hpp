#include "Transport.hpp"
#include <inetserverdgram.hpp>
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
private:
	void incoming();
	std::string _host, _port;
	std::unordered_map< std::string,UDPServerSocket*> _who;
	packet_callback _handler;
	libsocket::inet_dgram_server* _sock;
};
