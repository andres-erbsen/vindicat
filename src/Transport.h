#ifndef TRANSPORT_H_
#define TRANSPORT_H_

#include <functional>
#include <string>
#include <memory>

class TransportSocket
{
 public:
  TransportSocket(std::function<bool(const std::string&)> &&s, std::string &&a):
      _send(s), _addr(a)
  {
  }

  bool operator()(const std::string& msg) const
  {
    return _send(msg);
  }

  bool operator==(const TransportSocket &other) const
  {
    return _addr == other._addr;
  }

  bool operator<(const TransportSocket &other) const
  {
    return _addr < other._addr;
  }

 protected:
  std::function<bool(const std::string&)> _send;
  std::string _addr;
};

const static TransportSocket no_socket([](const std::string&){return false;}, "");

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
