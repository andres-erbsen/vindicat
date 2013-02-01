#ifndef FORWARDING_H_
#define FORWARDING_H_

#include "Device.h"
#include "Transport.h"
#include <lemon/list_graph.h>

#include <stdint.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <assert.h>

class Device;

class Forwarding {
public:
    Forwarding(const Forwarding&) = delete;
    Forwarding& operator=(const Forwarding&) = delete;
	virtual ~Forwarding(); // remove the other in this pair

	uint32_t id();

	// Forwardings come in pairs...
	virtual void detatch() = 0;
	// remove registrered pointer to this forwarding
	virtual bool forward_out(const std::string&) = 0;
	// called by other forwarding in the pair to send the packet out

	virtual bool forward(const std::string&) = 0;
	// All the hard work happens in the first of a pair, before forward_out

protected:
	std::weak_ptr<Forwarding> _pair_other;
	uint32_t _id;
};

class ForeignForwarding : public Forwarding {
public:
	void detatch();
	bool forward_out(const std::string&);
protected:
	std::weak_ptr<Device> _owner;
};

#endif // FORWARDING_H_
