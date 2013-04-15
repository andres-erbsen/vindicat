#include "Interface.h"
#include "Log.h"
#include <iostream>

class DummyInterface : public Interface {
  void send(const std::string&, uint8_t, const std::string&) {
    DEBUG();
  };
};
