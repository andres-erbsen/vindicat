#ifndef LIBVINDICAT_H_
#define LIBVINDICAT_H_

#include "libvindicat.pb.h"

namespace libvindicat {
  class Connection {
   private:
    sockaddr_un _server, _client;
    std::string _ip;
    int _fd;
    std::vector<Socket*> _sockets;
  };

  class Socket {
  };

  class RawSocket: public Socket {
   public:
    virtual ~RawSocket();
    bool sendto(const std::string& to, const std::string& payload) const;
    bool recvfrom(std::string& from, std::string& payload) const;

   private:
    RawSocket(Connection& conn, std::uint8_t proto);
    Connection& _conn;
    std::uint8_t _proto;
  };

  class UDPSocket: public Socket {
   public:
    virtual ~UDPSocket();
    bool sendto(const std::string& to, std::uint16_t port,
                const std::string& payload) const;
    bool recvfrom(std::string& from, std::uint16_t &port,
                  std::string& payload) const;

   private:
    UDPSocket(Connection& conn, std::uint16_t port);
    Connection& _conn;
    std::uint16_t port;
  };
}

#endif
