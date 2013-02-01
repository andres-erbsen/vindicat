#include "NetworkMap.h"

#include <lemon/dijkstra.h>
#include <lemon/maps.h>
#include <unordered_map>
#include <assert.h>

NetworkMap::NetworkMap()
	: _g_device(_graph)
	, _g_link  (_graph)
	, _our_node( _graph.addNode() )
	{}

void NetworkMap::add(std::shared_ptr<Device>&& dev_p) {
	assert(dev_p.unique());
	// Is this device already represented in the network map? Multiple times?
	std::vector<lemon::ListGraph::Node> matching_nodes;

	for ( const auto& id : dev_p->ids() ) {
		auto iter = _node_by_id.find(id);
		if ( iter != _node_by_id.end() ) {
			matching_nodes.push_back(iter->second);
		}
	}

	lemon::ListGraph::Node node;
	if ( matching_nodes.empty() ) { // completely new device
		node = _graph.addNode();
		for ( const auto& id : dev_p->ids() ) _node_by_id[id] = node;
		_g_device[node] = dev_p;
	} else {
		assert(matching_nodes.size() == 1);
		// TODO: handle the case of mutiple matches; when nodes have merged.
		node = *matching_nodes.begin();
		for ( const auto& id : dev_p->ids() ) _node_by_id[id] = node;
		_g_device[node]->merge( std::move(*dev_p) );
	}
}

class Measure {
public:
	typedef lemon::ListGraph::Edge argument_type;
	typedef double result_type;
	const lemon::ListGraph::EdgeMap<std::shared_ptr<Link> >& _links;
	Measure(const lemon::ListGraph::EdgeMap<std::shared_ptr<Link> >& links)
		: _links(links) {}

	double operator()(const lemon::ListGraph::Edge& e) const {
		return _links[e]->measure();
	}
};

std::vector< std::tuple< std::weak_ptr<Link>, std::weak_ptr<Device> > >
		NetworkMap::path_to(const Device& dev) const {
	std::vector<std::tuple<std::weak_ptr<Link>,std::weak_ptr<Device> > > ret;

	// Find the node that corresponds to the referenced Device
	auto iter = _node_by_id.find( dev.id() );
	if ( iter == _node_by_id.end() ) return ret; // empty path to nowhere
	lemon::ListGraph::Node t = iter->second;

	// Run the search
	auto measure = lemon::functorToMap(Measure(_g_link));
	lemon::ListPath<lemon::ListGraph> path;
	bool got = lemon::dijkstra(_graph, measure).path(path).run(_our_node,t);
	assert(got);

	// Convert the path to a globally meaningful format
	ret.reserve(path.length());
	lemon::ListGraph::Node prev = _our_node;
	lemon::ListGraph::Node next;
	lemon::ListGraph::Edge hop;
	while ( !path.empty() ) {
		hop = path.front();
		next = _graph.oppositeNode(prev, hop);
		ret.push_back( std::make_tuple(_g_link[hop], _g_device[next]) );
		path.eraseFront();
	}
	assert(next == t);
	assert(path.empty());
	return ret;
}
