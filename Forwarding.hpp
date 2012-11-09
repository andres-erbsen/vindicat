#ifndef FORWARDING_HPP
#define FORWARDING_HPP

#include "Transport.hpp"
#include <lemon/list_graph.h>

#include <stdint.h>
#include <string>
#include <unordered_map>
#include <assert.h>

class Forwarding {
	friend class NetworkMap;
public:
	uint32_t id;
	std::string to_device_id;
	// if a packet came from the node, where should it go?
	// if the packet should be dropped, the source node is returned
	virtual const lemon::ListGraph::Node&
		dst(const lemon::ListGraph::Node&) const = 0;
	// This packet should go over this socket. Do what was requested.
	virtual bool forward(TransportSocket*, std::string) = 0;
protected:
	lemon::ListGraph::Node rq_owner;
	lemon::ListGraph::Node other_owner;
public://memory management:
	Forwarding() = default;
	Forwarding(Forwarding&&) = default;
	Forwarding& operator=(Forwarding&&) = default;
	Forwarding(uint32_t, std::string);
};


class SimpleForwarding : public Forwarding {
	const lemon::ListGraph::Node&
		dst(const lemon::ListGraph::Node&) const = 0;
	bool forward(TransportSocket*, std::string);
};

class TwowayForwarding : public SimpleForwarding {
public:
	uint32_t id;
	std::string to_device_id;
	TwowayForwarding(uint32_t, std::string);
	const lemon::ListGraph::Node&
		dst(const lemon::ListGraph::Node&) const;
};

class OnewayForwarding : public SimpleForwarding {
public:
	uint32_t id;
	std::string to_device_id;
	OnewayForwarding(uint32_t, std::string);
	const lemon::ListGraph::Node&
		dst(const lemon::ListGraph::Node&) const;
};

#endif // FORWARDING_HPP
