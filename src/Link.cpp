#include "Link.h"

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

std::weak_ptr<TransportSocket> Link::tsocket() const {
	return std::weak_ptr<TransportSocket>();
}

double Link::measure() const {
	return 1.0;
}


DirectLink::DirectLink( const std::string& left_id
                      , std::shared_ptr<TransportSocket>&& ts
                      , const std::string& right_id )
                      : _tsocket(ts) {
	_left_id = left_id;
	_right_id = right_id;
	_mtime = std::time(NULL);
}

std::weak_ptr<TransportSocket> DirectLink::tsocket() const {
	return _tsocket;
}


bool ForeignLink::parseFrom( std::shared_ptr<LinkPromise>&& promise
                           , const NetworkMap& nm) {
	
}
