#ifndef LIBVINDICAT_H_
#define LIBVINDICAT_H_

#include <string>
#include <vector>
#include <cstdint>
#include <functional>

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
		friend RawSocket;
    RawSocket* forward(std::uint8_t proto);
		friend UDPSocket;
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
    virtual void forward(const std::string& from, std::uint8_t next_header,
                         const std::string& payload) = 0;
  };

  class RawSocket: public Socket {
   public:
    virtual ~RawSocket() noexcept;
    bool sendto(const std::string& to, const std::string& payload) const;
    void set_callback(
        std::function<void(const std::string&, const std::string&)> &&cb);

   protected:
    virtual void forward(const std::string& from, std::uint8_t next_header,
                         const std::string& payload);
   
   private:
    std::function<void(const std::string&, const std::string&)> _cb;
		friend Connection;
    RawSocket(Connection& conn, std::uint8_t proto);
    Connection& _conn;
    std::uint8_t _proto;
  };

  class UDPSocket: public Socket {
   public:
    virtual ~UDPSocket() noexcept;
    bool sendto(const std::string& to, std::uint16_t port,
                const std::string& payload) const;
    void set_callback(std::function<void(const std::string&, std::uint16_t,
                                         const std::string&)> &&cb);

   protected:
    virtual void forward(const std::string& from, std::uint8_t next_header,
                         const std::string& payload);

   private:
    std::function<void(const std::string&, std::uint16_t,
                       const std::string&)> _cb;
		friend Connection;
    UDPSocket(Connection& conn, std::uint16_t port);
    Connection& _conn;
    std::uint16_t _port;
  };
}

#endif
