#include "NetworkMap.h"
#include "Link.h"

#include <lemon/dijkstra.h>
#include <lemon/maps.h>
#include <utility>
#include <assert.h>

NetworkMap::NetworkMap(std::shared_ptr<Device>&& our_dev)
  : _g_device(_graph)
  , _g_link  (_graph)
  , _our_node( _graph.addNode() )
  {
    _node_by_id.insert( std::make_pair(our_dev->id(), _our_node) );
    std::swap(_g_device[_our_node], our_dev);
  }

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
    _g_device[node] = dev_p;
  } else {
    assert(matching_nodes.size() == 1);
    /// \TODO handle the case of mutiple matches; when nodes have merged.
    node = *matching_nodes.begin();
    for ( const auto& id : _g_device[node]->ids() ) {
      auto erased = _node_by_id.erase(id);
      assert(erased == 1);
    }
    _g_device[node]->merge( std::move(*dev_p) );
    dev_p.reset();
  }
  for ( const auto& id : _g_device[node]->ids() ) {
    _node_by_id.insert( std::make_pair(id,node) );
  }
}

bool NetworkMap::add(std::shared_ptr<Link>&& link) {
  lemon::ListGraph::Node left, right;
  {
    auto it = _node_by_id.find( link->left_id() );
    if ( it == _node_by_id.end() ) return 0;
    left = it->second;
  }
  {
    auto it = _node_by_id.find( link->right_id() );
    if ( it == _node_by_id.end() ) return 0;
    right = it->second;
  }
  auto edge = lemon::findEdge(_graph, left, right);
  if (edge == lemon::INVALID) edge = _graph.addEdge(left, right);  
  if ( ! _g_link[edge] || _g_link[edge]->mtime() < link->mtime() ) {
    auto ts = link->tsocket();
    if ( ! (ts == TransportSocket::no_socket()) ) {
      assert( left == _our_node || right == _our_node);
      if (left == _our_node) {
        _node_by_socket.insert( std::make_pair(ts,right) );
      } else {
        _node_by_socket.insert( std::make_pair(ts,left) );
      }
    }
    std::swap(_g_link[edge], link);
    return 1;
  } else {
    return 0;
  }
}

std::shared_ptr<Device> NetworkMap::
device(const TransportSocket& ts) const {
  auto it = _node_by_socket.find(ts);
  if ( it == _node_by_socket.end() ) return nullptr;
  return _g_device[it->second];
}

std::shared_ptr<Device> NetworkMap::device(const std::string& id) const {
  auto it = _node_by_id.find(id);
  if ( it == _node_by_id.end() ) return nullptr;
  return _g_device[it->second];
}

Device& NetworkMap::our_device() const {
  return *_g_device[_our_node];
}

TransportSocket
NetworkMap::tsock_to(const std::string& id) const {
  auto it = _node_by_id.find(id);
  if ( it == _node_by_id.end() ) return TransportSocket::no_socket();
  auto edge = findEdge(_graph, _our_node, it->second);
  return _g_link[edge]->tsocket();
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
