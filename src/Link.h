#ifndef LINK_H_
#define LINK_H_

#include "vindicat.pb.h"
#include "NetworkMap.h"

#include <vector>
#include <memory>
#include <cstdint>
#include <ctime>

class Link {
public:
	Link() = default;
	Link(const Link&) = delete;
	const Link& operator= (const Link&) = delete;
	virtual ~Link() = default;

	const std::string& left_id() const;
	const std::string& right_id() const;
	std::vector< std::weak_ptr<LinkPromise> > promises() const;	

	uint64_t mtime() const;	
	virtual double measure() const;
	virtual std::weak_ptr<TransportSocket> tsocket() const;

	virtual void merge(Link&&); // TODO: implement	

protected:
	std::string _left_id, _right_id;
	std::vector< std::shared_ptr<LinkPromise> > _promises;	
	uint64_t _mtime;	
};

class DirectLink : public Link {
public:
	DirectLink(const std::string&, std::shared_ptr<TransportSocket>&&, const std::string& );
	std::weak_ptr<TransportSocket> tsocket() const;
private:
	std::shared_ptr<TransportSocket>  _tsocket;
};

class ForeignLink : public Link {
	bool parseFrom(std::shared_ptr<LinkPromise>&&, const NetworkMap&);	
};

#endif // LINK_H_
