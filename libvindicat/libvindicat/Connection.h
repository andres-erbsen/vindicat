#ifndef LIBVINDICAT_CONNECTION_H_
#define LIBVINDICAT_CONNECTION_H_

#include <string>
#include <vector>
#include <cstdint>
#include <functional>

struct sockaddr_un;
/** \brief Namespace for client library. */
namespace libvindicat {
  class Socket;
  class RawSocket;
  class UDPSocket;

  /** \brief %Connection to the local node. */
  class Connection {
   public:
    /// \brief Constructor
    /// \param server_path Path to server Unix Domain %Socket
    /// \param client_path Path to client Unix Domain %Socket to create
    Connection(const std::string& server_path, const std::string& client_path);
    virtual ~Connection();
    /** \brief Get server's unique identifier */
    std::string identifier() const;
    /** \brief Get server's %IPv6 address in vindicat address space */
    std::string ipv6() const;
    /// Get a file descriptor on which one can do a select() or poll() to wait
    /// for it to be possible to read packets without blocking.
    int selectable_fd() const;
    /// \brief Read a packet.
    ///
    /// If no packets are available read() will block.
    void read();
		friend RawSocket;
    /// \brief Request a forwarding for an IP protocol that isn't UDP or TCP.
    /// \param proto Protocol to forward
    /// \returns a RawSocket to which such packets are forwarded.
    RawSocket* forward(std::uint8_t proto);
		friend UDPSocket;
    /// \brief Request a forwarding for UDP datagrams.
    /// \param local_port Port to forward
    /// \returns a UDPSocket to which such packets are forwarded.
    UDPSocket* forwardUDP(std::uint16_t local_port);

   private:
    /// \brief Send a packet to server
    /// \param payload packet
    void send(const std::string& payload) const;
    /** \brief Receive packet from server */
    std::string recv() const;
    sockaddr_un *_server, *_client;
    std::string _identifier, _ipv6, _ipv6_prefix;
    int _fd;
    std::vector<Socket*> _sockets;
  };
}

#endif  // LIBVINDICAT_CONNECTION_H_
