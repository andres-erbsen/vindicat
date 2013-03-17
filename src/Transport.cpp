#include "Transport.h"

TransportSocket::TransportSocket(send_function f, const std::string& id):
  _send(f), _id(id)
{
}

bool TransportSocket::send(const std::string& packet) const
{
  return _send(packet);
}

bool TransportSocket::operator==(const TransportSocket& ts) const
{
  return _id == ts._id;
}

bool TransportSocket::operator<(const TransportSocket& ts) const
{
  return _id < ts._id;
}

std::hash<std::string>::result_type TransportSocket::hash() const
{
  return std::hash<std::string>()(_id);
}

TransportSocket TransportSocket::no_socket()
{
  return TransportSocket([](const std::string&){return false;}, "NO-SOCKET");
}

void Transport::onPacket(packet_callback cb)
{
  _receive_cb = cb;
}
