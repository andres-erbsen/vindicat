#ifndef LIBVINDICAT_SOCKET_H_
#define LIBVINDICAT_SOCKET_H_

#include <string>
#include <cstdint>

namespace libvindicat {
  class Connection;
  /** \brief Base class for all sockets */
  class Socket {
   public:
    virtual ~Socket() = default;
	 private:
		friend Connection;
    /// \brief All packets that have been forwarded are passed to this function.
    ///
    /// It is up to the implementation to accept and dispatch them.
    /// \param from Source node identifier
    /// \param next_header IP protocol number of the packet
    /// \param payload Payload
    virtual void forward(const std::string& from, std::uint8_t next_header,
                         const std::string& payload) = 0;
  };
}

#endif  // LIBVINDICAT_SOCKET_H_
