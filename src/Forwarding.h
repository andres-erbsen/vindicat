#ifndef FORWARDING_H_
#define FORWARDING_H_

#include "Device.h"
#include "Transport.h"
#include "NetworkMap.h"
#include <lemon/list_graph.h>

#include <stdint.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <assert.h>

class Device;

class Forwarding {
public:
    Forwarding() = default;
    Forwarding(const Forwarding&) = delete;
    Forwarding& operator=(const Forwarding&) = delete;
	virtual ~Forwarding(); // remove the other in this pair

	uint32_t id();

	// Forwardings come in pairs...
	virtual void detatch() = 0;
	// remove registrered pointer to this forwarding
	virtual bool forward_out(const std::string&) = 0;
	// called by other forwarding in the pair to send the packet out
	bool forward(const std::string&);
    // calls forward_out of the other forwarding in the pair

protected:
	std::weak_ptr<Forwarding> _pair_other;
	uint32_t _id;
};

class ForeignForwarding : public Forwarding {
public:
    void detatch();
protected:
    std::weak_ptr<Device> _owner;
};

class SimpleForwarding : public ForeignForwarding {
public:
    SimpleForwarding(NetworkMap&); // move to ForeignForwarding w _nm if necessary
    bool forward_out(const std::string&);
private:
    NetworkMap& _nm; // move to ForeignForwarding w constructor if necessary
};

class NoForwarding : public ForeignForwarding {
public:
    bool forward_out(const std::string&);
};

class UpForwarding : public Forwarding {
public:
    void detach();
    bool forward_out(const std::string&);
};

#endif // FORWARDING_H_
