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
	friend class LinkNegotiator;
public:
	NetworkMap();
	void setOurDeviceBusinesscard(const DeviceBusinesscard&);

	bool addSocket(TransportSocket*);

	// start forwarding packets that come from a socket. true if all ok.
	bool addForwarding(TransportSocket*, Forwarding*);
	// forwrard this packet that came from this socket, return 0 otherwise
	bool forward(TransportSocket*, uint32_t, std::string);

    bool mergeGraph(const Subgraph&);
	void beacon(TransportSocket*, const DeviceBusinesscard&);
	bool has_link(TransportSocket*);

	bool dev_bcard(const std::string&, DeviceBusinesscard&) const;
	bool dev_bcard(TransportSocket*,   DeviceBusinesscard&) const;
	bool dev_info (const std::string&, DeviceInfo&) const; // with all identifiers
	bool dev_info (TransportSocket*,   DeviceInfo&) const; // with all identifiers
	TransportSocket* dev_socket(const std::string&) const;
	const std::vector<std::string>& dev_ids(TransportSocket*) const;
private:
    ListGraph::Node nodeForDevice(const DeviceInfo&);
    bool addLink(const LinkInfo&);

    bool mergeToGraph(const DeviceBusinesscard&, ListGraph::Node, const DeviceInfo&);
    bool mergeToGraph(const DeviceBusinesscard&, const DeviceInfo&);
    bool mergeToGraph(const DeviceBusinesscard&, ListGraph::Node);
    bool mergeToGraph(const DeviceBusinesscard&);

	ListGraph _graph;

	typedef std::unordered_map<uint32_t,Forwarding*> ForwardingMap;
	ListGraph::NodeMap<ForwardingMap> _forwardings;
	ListGraph::NodeMap<DeviceBusinesscard> _dev_bcards;
	ListGraph::NodeMap< std::vector<std::string> > _dev_ids;
	ListGraph::NodeMap<uint64_t> _dev_mtimes;

	ListGraph::EdgeMap<uint64_t> _link_mtimes;
	ListGraph::EdgeMap<LinkInfo::Status> _link_statuses;
	ListGraph::EdgeMap<TransportSocket*> _sockets;

	ListGraph::Node _our_node;

	std::unordered_map<TransportSocket*,ListGraph::Edge> _edge_by_sock;
	std::unordered_map<std::string,ListGraph::Node> _node_by_id;
};

# if 0
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
#endif // 0

#endif // NETWORKMAP_HPP
