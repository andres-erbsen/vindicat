#include <unordered_map>
#include <lemon/list_graph.h>
#include <assert.h>

#include "NetworkMap.hpp"

using namespace std;

NetworkMap::NetworkMap()
	: _forwardings(_graph)
	, _sockets(_graph)
	, _our_node( _graph.addNode() )
	{}

bool
NetworkMap::addForwarding( TransportSocket* from_trs
                         , Forwarding* fwd) {
	// From whom?
	ListGraph::Edge from_edge = _edge_by_sock.at(from_trs);
	ListGraph::Node from_node = _graph.oppositeNode(_our_node,from_edge);
	// To what device?
	auto it = _node_by_id.find( fwd->to_device_id );
	if ( it == _node_by_id.end() ) return 0;
	ListGraph::Node to_node = it->second;
	// now we know
	_forwardings[from_node][fwd->id] = fwd;
	_forwardings[to_node][fwd->id]   = fwd;
	fwd->rq_owner = from_node;
	fwd->other_owner = to_node;
	return 1;
}

bool NetworkMap::forward( TransportSocket* from_trs
                        , uint32_t rid
                        , string packet
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
	if ( _sockets[to_edge] == NULL ) return 0; // conn to destination lost
	return fwd->forward(_sockets[to_edge],packet);
}
