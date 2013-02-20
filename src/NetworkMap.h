#ifndef NETWORKMAP_H_
#define NETWORKMAP_H_

#include "Transport.h"
#include "Device.h"
#include "PrefixMap.h"
class Link;

#include <lemon/list_graph.h>

#include <memory>
#include <vector>
#include <tuple>
#include <unordered_map>

typedef std::vector< std::tuple<
			std::weak_ptr<Link>, std::weak_ptr<Device>
		> > Path;

// THREE RESPONSIBILITIES
// 1) Maintain information containers for the $k$ closest devices
// 2) Maintain a many-to-one mapping between device id-s and devices
// 3) Mainain a one-to-one mapping between tsockets and devices behind them

class NetworkMap {
public:
	NetworkMap(std::shared_ptr<Device>&&);
	NetworkMap(const NetworkMap&) = delete;
	const NetworkMap& operator= (const NetworkMap&) = delete;

	void add(std::shared_ptr<Device>&&);
	bool add(std::shared_ptr<Link>&&);

	std::shared_ptr<Device> device(const std::string&) const;
	std::shared_ptr<Device> device(const TransportSocket&) const;
	Device& our_device() const;
	TransportSocket tsock_to(const std::string&) const;
	
	std::vector< std::tuple<
			std::weak_ptr<Device>, std::weak_ptr<Link>, std::weak_ptr<Device>
		> > shake();
	// Return the links and the respective devices that were added to the map
	// Remove and discard all that we tried to add but had no room for

	Path path_to(const Device&) const;
	// Return the "best" path to the specified Device. Empty if none.
	
private:
	lemon::ListGraph _graph;
	lemon::ListGraph::NodeMap<std::shared_ptr<Device> > _g_device;
	lemon::ListGraph::EdgeMap<std::shared_ptr<Link> > _g_link;
	PrefixMap<lemon::ListGraph::Node> _node_by_id;
	std::unordered_map<TransportSocket, lemon::ListGraph::Node> _node_by_socket;

	lemon::ListGraph::Node _our_node;
};


#endif // NETWORKMAP_H_
