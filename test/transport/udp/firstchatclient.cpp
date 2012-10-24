#include "FirstChat.hpp"
int main (int argc, char** argv)
{
	FirstChatClient chatclient(argv[1]);
	// now wait for events to arrive
	ev_run (EV_DEFAULT_ 0);
	return 0;
}
