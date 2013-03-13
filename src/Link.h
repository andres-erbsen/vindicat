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
  static std::shared_ptr<Link>
    fromPromise(std::shared_ptr<LinkPromise>&&, const NetworkMap& nm);
  Link( const std::string&, TransportSocket&&, const std::string&);
  Link( const std::string&, const std::string&
      , uint64_t, bool, TransportSocket&& socket
      , std::shared_ptr<LinkPromise>&&);
  Link(const Link&) = delete;
  const Link& operator= (const Link&) = delete;
  virtual ~Link() = default;

  void merge(Link&&);

  const std::string& left_id() const;
  const std::string& right_id() const;
  std::shared_ptr<LinkPromise> promise() const;  

  uint64_t mtime() const;  
  virtual double measure() const;
  virtual TransportSocket tsocket() const;

private:
  std::string _left_id, _right_id;
  uint64_t _mtime;
  bool _operational;
  TransportSocket _tsocket;
  std::shared_ptr<LinkPromise> _promise; 
};

#endif // LINK_H_
