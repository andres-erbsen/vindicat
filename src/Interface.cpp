#include "Interface.h"

void Interface::onPacket(interface_callback cb) {
  _receive_cb = cb;
}
