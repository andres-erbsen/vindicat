#include "Interface.h"
#include <iostream>

class DummyInterface : public Interface {
	void send(const std::string&, uint8_t, const std::string&) {
		std::cout << "DummyInterface::send" << std::endl;
	};
};
