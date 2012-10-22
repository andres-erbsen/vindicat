#include <functional>
#include <string>


class TransportSocket {
public:
	virtual void send(const std::string&) = 0;
	virtual void useless() = 0;
};


typedef std::function< void(TransportSocket*, const std::string&) > packet_callback;


class Transport {
public:
	// set the receive callback
	virtual void onPacket(packet_callback) = 0;	
	// Transports start in a dormant state. activate this.
	virtual void enable() = 0;
	virtual void broadcast(const std::string&) = 0; // send to all
};

