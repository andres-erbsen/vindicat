#ifndef INTERFACEHANDLER_H_
#define INTERFACEHANDLER_H_

#include "CryptoIdentity.h"
#include "NetworkMap.h"
#include "ConnectionPool.h"
#include "ConnectionHandler.h"

class InterfaceHandler {
public:
  InterfaceHandler(CryptoIdentity&, NetworkMap&, ConnectionPool&, ConnectionHandler&);
    void operator()(std::string&& from, std::string&& packet);
private:
  CryptoIdentity& _ci;
  NetworkMap& _nm;
  ConnectionPool& _cp;
  ConnectionHandler& _ch;
};

#endif // INTERFACEHANDLER_H_
