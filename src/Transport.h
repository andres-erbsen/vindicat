#ifndef TRANSPORT_H_
#define TRANSPORT_H_

#include <functional>
#include <string>
#include <memory>

typedef std::function<bool(const std::string&)> TransportSocket;

bool no_socket(const std::string&);

typedef std::function< void(const TransportSocket&, const std::string&) > packet_callback;


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
