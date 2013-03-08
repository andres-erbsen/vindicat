#ifndef CONNECTIONHANDLER_H_
#define CONNECTIONHANDLER_H_

#include "Interface.h"
#include <memory>
#include <cstdint>
#include <string>
#include <vector>

class Interface;

class ConnectionHandler {
 public:
  ConnectionHandler() = default;
  ConnectionHandler(ConnectionHandler&&) = default;
  ~ConnectionHandler() = default;	 

  ConnectionHandler(const ConnectionHandler&) = delete;
  const ConnectionHandler& operator= (const ConnectionHandler&) = delete;

  void addInterface(std::unique_ptr<Interface>&&);
  void operator() (const std::string&, std::uint8_t, const std::string&);
 
 private:
  std::vector< std::unique_ptr<Interface> > _ifaces;
};


#endif // CONNECTIONHANDLER_H_
