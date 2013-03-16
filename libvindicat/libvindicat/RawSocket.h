#ifndef LIBVINDICAT_RAWSOCKET_H_
#define LIBVINDICAT_RAWSOCKET_H_

#include <libvindicat/Socket.h>
#include <string>
#include <cstdint>
#include <functional>

namespace libvindicat {
  class Connection;
  /** \brief %Socket for raw IP datagrams with specified protocol */
  class RawSocket: public Socket {
   public:
    virtual ~RawSocket() noexcept;
    /// \brief Send a packet of specified protocol
    /// \param to Destination node identifier
    /// \param payload Payload
    void sendto(const std::string& to, const std::string& payload) const;
    /// \brief Set callback to be called when packets are received.
    ///
    /// If this function isn't called before packets arrive a std::bad_function
    /// exception is thrown.
    //
    /// \param cb Callback. First parameter is source node identifier, the
    /// second parameter is the payload.
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
}

#endif  // LIBVINDICAT_RAWSOCKET_H_
