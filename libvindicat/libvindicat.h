#ifndef LIBVINDICAT_H_
#define LIBVINDICAT_H_

#include <string>
#include <vector>
#include <cstdint>

struct sockaddr_un;

namespace libvindicat {
  class Socket;
  class RawSocket;
  class UDPSocket;

  class Connection {
   public:
    Connection(const std::string& server_path, const std::string& client_path);
    virtual ~Connection();
    std::string identifier() const;
    std::string ipv6() const;
    RawSocket* forward(std::uint8_t proto);
    UDPSocket* forwardUDP(std::uint16_t local_port);
    void wait();
   private:
    void send(const std::string& payload) const;
    std::string recv() const;
    sockaddr_un *_server, *_client;
    std::string _identifier, _ipv6;
    int _fd;
    std::vector<Socket*> _sockets;
  };

  class Socket {
   public:
    virtual ~Socket() = default;

	 private:
		friend Connection;
    virtual void forward(const std::string& to, std::uint8_t next_header,
                         const std::string& payload);
  };

  class RawSocket: public Socket {
   public:
    virtual ~RawSocket() noexcept = default;
    bool sendto(const std::string& to, const std::string& payload) const;
    bool recvfrom(std::string& from, std::string& payload) const;

   private:
		friend Connection;
    RawSocket(Connection& conn, std::uint8_t proto);
    Connection& _conn;
    std::uint8_t _proto;
  };

  class UDPSocket: public Socket {
   public:
    virtual ~UDPSocket() noexcept = default;
    bool sendto(const std::string& to, std::uint16_t port,
                const std::string& payload) const;
    bool recvfrom(std::string& from, std::uint16_t &port,
                  std::string& payload) const;

   private:
		friend Connection;
    UDPSocket(Connection& conn, std::uint16_t port);
    Connection& _conn;
    std::uint16_t port;
  };
}

#endif
