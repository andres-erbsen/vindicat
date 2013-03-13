#include "Link.h"


std::shared_ptr<Link> Link::
fromPromise( std::shared_ptr<LinkPromise>&& promise, const NetworkMap& nm) {
  std::unique_ptr<LinkInfo> info(new LinkInfo());
  if ( ! promise->has_link_info_msg()
    || ! promise->left_sig_algos_size() 
    || ! promise->right_sig_algos_size() 
    || ! promise->left_sigs_size() 
    || ! promise->right_sigs_size() 
    || ! info->ParseFromString(promise->link_info_msg()) ) {
    return std::shared_ptr<Link>();
  }
  
  // Look up the relevant devices from the network map
  std::shared_ptr<Device> left_device  = nm.device(info->left() );
  std::shared_ptr<Device> right_device = nm.device(info->right());
  if ( ! left_device || ! right_device ) return std::shared_ptr<Link>();

  // verify the signatures
  const auto& msg = promise->link_info_msg();
  int n = std::min(promise->left_sigs_size(), promise->left_sig_algos_size());
  bool got_left = 0;
  for (int i=0; i<n; ++i) {
    const auto& sig = promise->left_sigs(i);
    const auto algo = static_cast<SigAlgo>( promise->left_sig_algos(i) );
    if ( left_device->verifySignature(msg, sig, algo) ) {
      got_left = 1;
    }
  }

  int m = std::min(promise->right_sigs_size(), promise->right_sig_algos_size());
  bool got_right = 0;
  for (int i=0; i<m; ++i) {
    const auto& sig = promise->right_sigs(i);
    const auto algo = static_cast<SigAlgo>( promise->right_sig_algos(i) );
    if ( right_device->verifySignature(msg, sig, algo) ) {
      got_right = 1;
    }
  }

  if ( !got_left || !got_right ) return nullptr;

  if (info->status() == LinkInfo::PUBLIC) {
    return std::make_shared<PublicLink>( info->left()
                                      , info->right()
                                      , info->time()
                                      , std::move(promise) );
  } else if (info->status() == LinkInfo::DEAD) {
    return std::make_shared<DeadLink>( info->left()
                                    , info->right()
                                    , info->time()
                                    , std::move(promise) );
  } else return nullptr;
}


Link::Link( const std::string& left_id, const std::string& right_id
          , uint64_t mtime, std::shared_ptr<LinkPromise>&& promise)
          : _left_id(left_id)
          , _right_id(right_id)
          , _promises{promise}
          , _mtime(mtime)
          {}

const std::string& Link::left_id() const {
  return _left_id;
}

const std::string& Link::right_id() const {
  return _right_id;
}

std::vector< std::weak_ptr<LinkPromise> > Link::promises() const {
  std::vector< std::weak_ptr<LinkPromise> > ret(_promises.size());
  for (const auto& p : _promises)
    ret.push_back( std::weak_ptr<LinkPromise>(p) );
  return ret;
}

uint64_t Link::mtime() const {
  return _mtime;
}

TransportSocket Link::tsocket() const {
  return TransportSocket::no_socket();
}

double Link::measure() const {
  return 1.0;
}


DirectLink::DirectLink( const std::string& left_id
                      , TransportSocket&& ts
                      , const std::string& right_id )
                      : Link(left_id, right_id, std::time(NULL), nullptr) 
                      , _tsocket(ts) 
                      {}

TransportSocket DirectLink::tsocket() const {
  return _tsocket;
}

PublicLink::PublicLink( const std::string& left_id, const std::string& right_id
                      , uint64_t mtime, std::shared_ptr<LinkPromise>&& promise)
                      : Link(left_id, right_id, mtime, std::move(promise) ) {}

DeadLink::DeadLink( const std::string& left_id, const std::string& right_id
                  , uint64_t mtime, std::shared_ptr<LinkPromise>&& promise)
                  : Link(left_id, right_id, mtime, std::move(promise) ) {}
