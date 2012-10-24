#include "FirstChat.hpp"
#include <exception.hpp>

int main (int argc, char** argv)
{
	try {
	FirstChatServer chatclient("localhost", argv[1]);
	// now wait for events to arrive
	ev_run (EV_DEFAULT_ 0);
	} catch (libsocket::socket_exception exc) {
	    std::cerr << exc.mesg;
	}
	return 0;
}
