#include <unordered_map>
#include <lemon/list_graph.h>
#include <assert.h>

#include "NetworkMap.hpp"

NetworkMap::NetworkMap()
	: _forwardings(_graph)
	, _sockets(_graph)
	, _our_node( _graph.addNode() )
	{}

TransportSocket* NetworkMap::socketTo(const std::string& device_id) {
	auto it = _node_by_id.find( device_id );
	if ( it == _node_by_id.end() ) return NULL;
	ListGraph::Edge to_edge = findEdge(_graph, _our_node, it->second);
	return _sockets[to_edge];
}

bool NetworkMap::addForwarding( TransportSocket* from_trs
							  , Forwarding* fwd) {
	// From whom?
	ListGraph::Edge from_edge = _edge_by_sock.at(from_trs);
	ListGraph::Node from_node = _graph.oppositeNode(_our_node,from_edge);
	// To what device?
	auto it = _node_by_id.find( fwd->to_device_id );
	if ( it == _node_by_id.end() ) return 0;
	ListGraph::Node to_node = it->second;
	// can we send to them?
	ListGraph::Edge to_edge = findEdge(_graph,_our_node,to_node);
	if ( to_edge == lemon::INVALID ) return 0;
	// set it up
	_forwardings[from_node][fwd->id] = fwd;
	_forwardings[to_node][fwd->id]   = fwd;
	fwd->rq_owner = from_node;
	fwd->other_owner = to_node;
	return 1;
}

bool NetworkMap::forward( TransportSocket* from_trs
						, uint32_t rid
						, std::string packet
						) {
	// From whom?
	ListGraph::Edge from_edge = _edge_by_sock.at(from_trs);
	ListGraph::Node from_node = _graph.oppositeNode(_our_node,from_edge);
	// Forward how?
	auto it = _forwardings[from_node].find(rid);
	if ( it == _forwardings[from_node].end() ) return 0; // destination lost
	Forwarding* fwd = it->second;
	assert (rid == fwd->id);
	// Which way next?
	ListGraph::Node to_node = fwd->dst(from_node);
	if (to_node == from_node) return 0; // This route does not accept
										// packets from $from_node
	ListGraph::Edge to_edge = findEdge(_graph,_our_node,to_node);
	if ( to_edge == lemon::INVALID ) return 0; // conn to destination lost
	if ( _sockets[to_edge] == NULL ) return 0;
	return fwd->forward(_sockets[to_edge],packet);
}

bool NetworkMap::mergeGraph( const Subgraph& s ) {
    int devicenum = s.devices_size();
    int linknum = s.links_size();
    
	// Adding devices
	for (int i = 0; i < devicenum; i++) {
		const DeviceBusinesscard& dbc = s.devices(i);
		
		if (_node_by_id.find(dbc.device_info_msg()) == _node_by_id.end()) {
			// Device not in graph, insert it there
			ListGraph::Node added_node = _graph.addNode();
			_node_by_id[dbc.device_info_msg()] = added_node;
		}
	}
	
	// Adding links
    bool added_to_graph = false; // For deciding whether to broadcast
	for (int i = 0; i < linknum; i++) {
		const LinkPromise& lp = s.links(i);
		LinkInfo l_info;
		if ( l_info.ParseFromString(lp.link_info_msg()) == 0 ) return 0;
		
        // Get nodes by id if in graph
        if (_node_by_id.find(l_info.left()) == _node_by_id.end()) continue;
		ListGraph::Node left = _node_by_id[l_info.left()];
        if (_node_by_id.find(l_info.right()) == _node_by_id.end()) continue;
		ListGraph::Node right = _node_by_id[l_info.right()];
        
        ListGraph::Edge wanted_edge = findEdge(_graph,left,right); // Edge already in graph?
        if ( wanted_edge == lemon::INVALID ) {
            _graph.addEdge(left, right);
            added_to_graph = true; // For deciding whether to broadcast
        }
	}
	
	// Limiting what to broadcast
    // Right now broadcast everything with two devices and one new link
	if (devicenum == 2 && linknum == 1 && added_to_graph) return 1;
	return 0;
}
