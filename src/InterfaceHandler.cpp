#include "InterfaceHandler.h"
#include <cassert>
#include <utility>

InterfaceHandler::InterfaceHandler(NetworkMap& nm, ConnectionPool& cp)
	: _nm(nm)
	, _cp(cp)
	{}

void InterfaceHandler::operator()(std::string&& to, std::string&& packet)
{
	assert(0);
}
