#ifndef TRANSPORT_H_
#define TRANSPORT_H_

#include <functional>
#include <string>
#include <memory>

class TransportSocket
{
public:
	typedef std::function<bool(const std::string&)> send_function;
	TransportSocket(send_function, const std::string&);
	bool send(const std::string&) const;
	bool operator==(const TransportSocket&) const;
	bool operator<(const TransportSocket&) const;
	std::hash<std::string>::result_type hash() const;
	static TransportSocket no_socket();
private:
	send_function _send;
	std::string _id;
};

namespace std
{
	template <> struct hash<TransportSocket>
	{
		hash<string>::result_type operator()(const TransportSocket& ts) const
		{
			return ts.hash();
		}
	};
}

typedef std::function< void(TransportSocket&&, std::string&&) > packet_callback;


class Transport {
public:
	Transport(const Transport&) = delete;
	const Transport& operator= (const Transport&) = delete;	
	virtual ~Transport() = default;

	// set the receive callback
	void onPacket(packet_callback);	
	// Transports start in a dormant state. activate this.
	virtual void enable() = 0;
	virtual void to_unknown(const std::string&) = 0; // send to all

protected:
	Transport() = default;
	packet_callback _receive_cb;
};

#endif // TRANSPORT_H_
