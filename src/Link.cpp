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

  return std::make_shared<Link>( info->left()
                               , info->right()
                               , info->time()
                               , info->status() != LinkInfo::DEAD
                               , TransportSocket::no_socket()
                               , std::move(promise) );
}

Link::Link( const std::string& left_id, const std::string& right_id
          , uint64_t mtime, bool operational, TransportSocket&& tsocket
          , std::shared_ptr<LinkPromise>&& promise)
          : _left_id(left_id)
          , _right_id(right_id)
          , _operational(operational)
          , _mtime(mtime)
          , _tsocket(tsocket)
          , _promise(promise)
          {}

Link::Link( const std::string& left_id
          , TransportSocket&& socket
          , const std::string& right_id)
 : Link( left_id, right_id, std::time(NULL), true, std::move(socket), nullptr )
 {}

const std::string& Link::left_id() const {
  return _left_id;
}

const std::string& Link::right_id() const {
  return _right_id;
}

std::shared_ptr<LinkPromise> Link::promise() const {
  return _promise;
}

uint64_t Link::mtime() const {
  return _mtime;
}

TransportSocket Link::tsocket() const {
  return _tsocket;
}

double Link::measure() const {
  return _operational ? 1.0 : 1e400;
}
