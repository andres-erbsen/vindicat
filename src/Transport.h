#ifndef TRANSPORT_H_
#define TRANSPORT_H_

#include <functional>
#include <string>
#include <memory>

class TransportSocket {
public:
	TransportSocket(const TransportSocket&) = delete;
	const TransportSocket& operator= (const TransportSocket&) = delete;	
	virtual ~TransportSocket() {};

	virtual void send(const std::string&) = 0;

protected:
	TransportSocket() = default;
};


typedef std::function< void(std::shared_ptr<TransportSocket>, const std::string&) > packet_callback;


class Transport {
public:
	Transport(const Transport&) = delete;
	const Transport& operator= (const Transport&) = delete;	
	virtual ~Transport() {};

	// set the receive callback
	virtual void onPacket(packet_callback) = 0;	
	// Transports start in a dormant state. activate this.
	virtual void enable() = 0;
	virtual void broadcast(const std::string&) = 0; // send to all

protected:
	Transport() = default;
};

#endif // TRANSPORT_H_
