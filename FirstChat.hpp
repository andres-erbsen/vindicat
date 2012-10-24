#include "UDPServerTransport.hpp"
#include "UDPClientTransport.hpp"
#include <ev++.h>

class FirstChatClient {
public:
	FirstChatClient(std::string host, std::string port = std::string("30307"));
	void read_cb(ev::io& w, int revents);
private:
	packet_callback _handler;
	ev::io _stdin_watcher;
	UDPClientTransport _transport;
	TransportSocket* _sock;
};

class FirstChatServer {
public:
	FirstChatServer(std::string host = std::string("localhost"), std::string port = std::string("30307"));
	void read_cb(ev::io& w, int revents);
private:
	packet_callback _handler;
	ev::io _stdin_watcher;
	UDPServerTransport _transport;
	TransportSocket* _sock;
};

