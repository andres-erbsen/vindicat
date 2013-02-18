#ifndef TRANSPORT_H_
#define TRANSPORT_H_

#include <functional>
#include <string>
#include <memory>

typedef std::function<bool(const std::string&)> TransportSocket;

const static TransportSocket no_socket([](const std::string&){return false;});

typedef std::function< void(const TransportSocket&, const std::string&, const std::string&) > packet_callback;


class Transport {
public:
	Transport(const Transport&) = delete;
	const Transport& operator= (const Transport&) = delete;	
	virtual ~Transport() = default;

	// set the receive callback
	void onPacket(packet_callback);	
	// Transports start in a dormant state. activate this.
	virtual void enable() = 0;
	virtual void broadcast(const std::string&) = 0; // send to all

protected:
	Transport() = default;
	packet_callback _receive_cb;
};

#endif // TRANSPORT_H_
