#ifndef LIBVINDICAT_UDPSOCKET_H_
#define LIBVINDICAT_UDPSOCKET_H_

#include <libvindicat/Socket.h>
#include <string>
#include <cstdint>
#include <functional>

namespace libvindicat {
  class Connection;
  /** \brief %Socket for UDP datagrams */
  class UDPSocket: public Socket {
   public:
    virtual ~UDPSocket() noexcept;
    /// \brief Send a UDP packet.
    /// \param to Destination node identifier
    /// \param port Destination port
    /// \param payload Payload
    void sendto(const std::string& to, std::uint16_t port,
                const std::string& payload) const;
    /// \brief Set callback to be called when packets are received.
    ///
    /// If this function isn't called before packets arrive a std::bad_function
    /// exception is thrown.
    ///
    /// \param cb Callback. First parameter is source node identifier, the
    /// second is source port number and the third is datagram payload.
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

#endif  // LIBVINDICAT_UDPSOCKET_H_
