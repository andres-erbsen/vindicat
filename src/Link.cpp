#include "Link.h"

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
	return no_socket;
}

double Link::measure() const {
	return 1.0;
}


DirectLink::DirectLink( const std::string& left_id
                      , TransportSocket&& ts
                      , const std::string& right_id )
                      : _tsocket(ts) {
	_left_id = left_id;
	_right_id = right_id;
	_mtime = std::time(NULL);
}

TransportSocket DirectLink::tsocket() const {
	return _tsocket;
}

PublicLink::PublicLink( const std::string& left_id, const std::string& right_id
                      , uint64_t mtime, std::shared_ptr<LinkPromise>&& promise)
                      : Link(left_id, right_id, mtime, std::move(promise) ) {}

DeadLink::DeadLink( const std::string& left_id, const std::string& right_id
                  , uint64_t mtime, std::shared_ptr<LinkPromise>&& promise)
                  : Link(left_id, right_id, mtime, std::move(promise) ) {}
