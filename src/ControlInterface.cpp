#include "UDPInterface.h"
#include "Util.h"

const uint8_t VC_CONTROL_PROTOCOL = 0xDC;

bool ControlInterface::match( const std::string&,
                          uint8_t protocol,
                          const std::string&) {
  return protocol == VC_CONTROL_PROTOCOL;
}
