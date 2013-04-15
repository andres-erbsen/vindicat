#include "InterfaceHandler.h"
#include "Connection.h"
#include "Log.h"

InterfaceHandler::InterfaceHandler
  (CryptoIdentity& ci, NetworkMap& nm, ConnectionPool& cp, ConnectionHandler& ch)
  : _ci(ci)
  , _nm(nm)
  , _cp(cp)
  , _ch(ch)
  {}

void InterfaceHandler::operator()(std::string&& to, std::string&& packet) {
  auto it = _cp.find(to);
  if (it != _cp.end() ) {
    it->second->connection_forward(packet);
  } else {
    {
      auto *logger = new INFO();
      *logger << "connecting to " << std::hex;
      for(auto c : to)
        *logger << +static_cast<unsigned char>(c);
      *logger << std::dec;
      delete logger;
    }

    Path path;
    {
      auto dst_dev = _nm.device(to);
      if (!dst_dev) return;
      path = _nm.path_to(*dst_dev);
    }

    auto conn = std::make_shared<Connection>(_ci, path, _cp, _ch);
    
    auto rfwd = std::make_shared<SimpleForwarding>(_nm, conn->id());
    auto next_dev = std::get<1>(path.at(0));
    next_dev->addForwarding(rfwd);

    Forwarding::pair(conn, rfwd);

    _cp.insert( std::make_pair(to, conn) );
    conn->request();
  }
}
