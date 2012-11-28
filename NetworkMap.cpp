#include "NetworkMap.hpp"
#include "CryptoIdentity.hpp"

#include <unordered_map>
#include <lemon/list_graph.h>
#include <assert.h>

#include <ctime>


NetworkMap::NetworkMap()
	: _forwardings  (_graph)
	, _sigkeys      (_graph)
	, _enckeys      (_graph)
	, _dev_mtimes   (_graph, 0)
	, _link_mtimes  (_graph, 0)
	, _link_statuses(_graph, LinkInfo::DEAD)
	, _sockets      (_graph, nullptr)
	, _our_node( _graph.addNode() )
	{}

bool NetworkMap::addSocket(TransportSocket* trs) {
	if ( _edge_by_sock.count(trs) ) return 0; // not new
	// new edge, add a new node of which we know nothing and an edge to it
	auto node = _graph.addNode();
	auto edge = _graph.addEdge(_our_node, node);
	_sockets[edge] = trs;
	_edge_by_sock[trs] = edge;
	return 1;
}

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

void NetworkMap::mergeDevice(const DeviceInfo& device, ListGraph::Node node) {
	bool override = 0;
	if ( device.has_time() && device.time() > _dev_mtimes[node] ) {
		override = 1;
		_dev_mtimes[node] = device.time();
	}

	for ( const SigKey& key : device.sig_keys() ) {
		if ( override || ! _sigkeys[node].count(key.algo()) ) {
			_sigkeys[node][key.algo()] = key.key();
		}
	}

	for ( const EncKey& key : device.enc_keys() ) {
		if ( override || ! _enckeys[node].count(key.algo()) ) {
			_enckeys[node][key.algo()] = key.key();
		}
	}

	for ( const auto& id : device.identifiers() ) _node_by_id[id] = node;
}

void NetworkMap::beacon(TransportSocket* trs, const DeviceBusinesscard& card) {
	DeviceInfo device;
	if ( ! verify(card,device) ) return;
	ListGraph::Edge edge = _edge_by_sock.at(trs);
	ListGraph::Node node = _graph.oppositeNode(_our_node, edge);
	mergeDevice(device, node);
}

void NetworkMap::addDevice(const DeviceInfo& device) {
	// Is this device already represented in the network map? Multiple times?
	std::vector<Node> matching_nodes;

	for ( const auto& id : device.identifiers() ) {
		auto iter = _node_by_id.find(id);
		if ( iter != _node_by_id.end() ) {
			matching_nodes.push_back(iter->second);
		}
	}

	ListGraph::Node node;
	if (matching_nodes.empty()) { // completely new device
		node = _graph.addNode();
	} else if (matching_nodes.size() == 1) { // already known
		node = *matching_nodes.begin();
	// some devices we know are actually one
	} else assert(/*Merging multiple devices not implemented*/ 0);

	mergeDevice(device,node);
}

bool NetworkMap::addLink(const LinkInfo& link) {
	if ( _node_by_id.count(link.left()) == 0 ) return 0;
	if ( _node_by_id.count(link.left()) == 0 ) return 0;
	auto lnode = _node_by_id[link.left() ];
	auto rnode = _node_by_id[link.right()];
	ListGraph::Edge edge = findEdge(_graph, lnode, rnode);
	if ( edge == lemon::INVALID ) { // no link between these nodes before this
		edge = _graph.addEdge(lnode, rnode);
		if (link.has_time()) _link_mtimes[edge] = link.time();
		if (link.has_status()) _link_statuses[edge] = link.status();
		return 1;
	} else {
		if ( link.has_time() && link.time() > _link_mtimes[edge] ) {
			// newer link description
			_link_mtimes[edge] = link.time();
			if (link.has_status()) _link_statuses[edge] = link.status();
		} else if ( !link.has_time() && _link_mtimes[edge] == 0 ) {
			// neither has a timestamp
			if (link.has_status()) _link_statuses[edge] = link.status();
		} else return 0; // outdated
	}
}

bool NetworkMap::mergeGraph( const Subgraph& sgr ) {
	if ( sgr.devices_size() == 2 && sgr.links_size() == 1) { //Link announcement
		DeviceInfo ldev, rdev;
		LinkInfo link;
		// Parse and verify the two devices to `rdev` and `ldev`
		if ( ! verify(sgr.devices(0), ldev) ) return 0;
		if ( ! verify(sgr.devices(1), rdev) ) return 0;
		if ( ! verify(sgr.links(0), ldev, rdev, link) ) return 0;
		bool have_ldev = 0, have_rdev = 0;
		for ( auto& id :ldev.identifiers() ) have_ldev |= _node_by_id.count(id);
		for ( auto& id :rdev.identifiers() ) have_rdev |= _node_by_id.count(id);
		if ( ! have_ldev && ! have_ldev ) return 0;
		bool ret;
		addDevice(rdev);
		addDevice(ldev);
		return addLink(link);
	}
	return 0;
}
