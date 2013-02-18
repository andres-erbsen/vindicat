#include "Transport.h"

void Transport::onPacket(packet_callback cb)
{
  _receive_cb = cb;
}
