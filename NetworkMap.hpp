#ifndef NETWORKMAP_HPP
#define NETWORKMAP_HPP

#include "Transport.hpp"
#include "Forwarding.hpp"
#include <lemon/list_graph.h>
#include <stdint.h>
#include <unordered_map>
#include <vector>
#include "vindicat.pb.h"


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
    bool addDevice(const DeviceInfo&);
    bool addLink(const LinkInfo&);
    bool mergeGraph(const Subgraph&);
private:
	ListGraph _graph;
	ListGraph::Node _our_node;

	typedef std::unordered_map<uint32_t,Forwarding*> ForwardingMap;
	typedef std::unordered_map<PkencAlgo,std::string> EncKeyMap;
	typedef std::unordered_map<SigAlgo,std::string> SigKeyMap;
	ListGraph::NodeMap<ForwardingMap> _forwardings;
	ListGraph::NodeMap<EncKeyMap> _enckeys;
	ListGraph::NodeMap<SigKeyMap> _sigkeys;
	ListGraph::NodeMap<uint64_t> _dev_mtimes;

	ListGraph::EdgeMap<TransportSocket*> _sockets;
	ListGraph::EdgeMap<uint64_t> _link_mtimes;
	ListGraph::EdgeMap<LinkInfo::Status> _link_statuses;

	std::unordered_map<TransportSocket*,ListGraph::Edge> _edge_by_sock;
	std::unordered_map<std::string,ListGraph::Node> _node_by_id;
};

// instance Hashable SigAlgo
namespace std { template <> struct hash<SigAlgo> {
	size_t operator()(const SigAlgo& algo) const {
		return algo;
		// return hash<int>()(algo);"
	}
}; }

// instance Hashable PkencAlgo
namespace std { template <> struct hash<PkencAlgo> {
	size_t operator()(const PkencAlgo& algo) const {
		return algo;
		// return hash<int>()(algo);"
	}
}; }

#endif // NETWORKMAP_HPP
