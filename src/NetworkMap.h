#ifndef NETWORKMAP_H_
#define NETWORKMAP_H_

#include "Device.h"
#include "Link.h"

#include <lemon/list_graph.h>

#include <memory>
#include <vector>
#include <tuple>
#include <unordered_map>


// TWO RESPONSIBILITIES :(
// 1) Maintain information containers for the $k$ closest devices
// 2) Maintain a many-to-one mapping between device id-s and devices

class NetworkMap {
public:
	NetworkMap();
	NetworkMap(const NetworkMap& _) = delete;
	const NetworkMap& operator= (const NetworkMap& _) = delete;

	void add(std::shared_ptr<Device>&&); // transfer ownership
	
	std::vector< std::tuple<
			std::weak_ptr<Device>, std::weak_ptr<Link>, std::weak_ptr<Device>
		> > shake();
	// Return the links and the respective devices that were added to the map
	// Remove and discard all that we tried to add but had no room for

	std::vector< std::tuple<
			std::weak_ptr<Link>, std::weak_ptr<Device>
		> > path_to(const Device&) const;
	// Return the "best" path to the specified Device. Empty if none.
	
private:
	lemon::ListGraph::Node nodeFor(const Device&);

	lemon::ListGraph _graph;
	lemon::ListGraph::NodeMap<std::shared_ptr<Device> > _g_device;
	lemon::ListGraph::EdgeMap<std::shared_ptr<Link> > _g_link;
	std::unordered_map<std::string,lemon::ListGraph::Node> _node_by_id;

	lemon::ListGraph::Node _our_node;
};


#endif // NETWORKMAP_H_
