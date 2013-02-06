#include "InterfaceHandler.h"
#include <cassert>
InterfaceHandler::InterfaceHandler(NetworkMap& nm, ConnectionPool& cp)
	: _nm(nm)
	, _cp(cp)
	{}

void InterfaceHandler::operator()(std::string&& from, std::string&& packet)
{
	assert(0);
}
