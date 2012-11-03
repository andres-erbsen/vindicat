#ifndef NETWORKMAP_HPP
#define NETWORKMAP_HPP

#include "Transport.hpp"
#include "Forwarding.hpp"
#include <lemon/list_graph.h>
#include <stdint.h>
#include <unordered_map>
#include <vector>


using lemon::ListGraph;
class NetworkMap {
public:
	typedef ListGraph::Node Node;
	NetworkMap();
	// start forwarding packets that come from a socket. true if all ok.
	bool addForwarding(TransportSocket*, Forwarding*);
	// forwrard this packet that came from this socket, return 0 otherwise
	bool forward(TransportSocket*, uint32_t, std::string);
	TransportSocket* socketTo(const std::string&);
private:
	ListGraph _graph;
	typedef std::unordered_map<uint32_t,Forwarding*> ForwardingMap;
	ListGraph::NodeMap<ForwardingMap> _forwardings;
	ListGraph::EdgeMap<TransportSocket*> _sockets;
	ListGraph::Node _our_node;
	std::unordered_map<TransportSocket*,ListGraph::Edge> _edge_by_sock;
	std::unordered_map<std::string,ListGraph::Node> _node_by_id;
};

/*

// instance Hashable Device...
namespace std { template <> struct hash<Device> {
	size_t operator()(const Device& v) const {
		// your code here, e.g. "return hash<int>()(x.value);"
	}
}; }

*/

#endif // NETWORKMAP_HPP
