// vim: set ts=4 sw=4 :
#include "FirstChat.hpp"
#include <iostream>
#include <assert.h>

FirstChatClient::
FirstChatClient(std::string host, std::string port)
	: _sock(NULL)
    , _transport(host, port)
	, _handler( [this] (TransportSocket* s, std::string p) {
		std::cout << p << std::endl;
		this->_sock = s;
	})
	{
	// Watcher for stdin
	_stdin_watcher.set <FirstChatClient, &FirstChatClient::read_cb> (this);
	_stdin_watcher.start (0, ev::READ);
	
	// Initialising client
	_transport.onPacket(_handler);
	_transport.enable();
	}

void FirstChatClient::read_cb(ev::io &w, int revents) {
	// Callback for libev loop watching stdin
	std::string s;
	std::getline(std::cin, s); // std::cin >> s;
	assert(_sock);
	_sock->send(s);
}


FirstChatServer::
FirstChatServer(std::string host, std::string port)
	: _sock(NULL)
    , _transport(host, port)
	, _handler( [this] (TransportSocket* s, std::string p) {
		std::cout << p << std::endl;
		this->_sock = s;
	})
	{
	// Watcher for stdin
	_stdin_watcher.set <FirstChatServer, &FirstChatServer::read_cb> (this);
	_stdin_watcher.start (0, ev::READ);
	
	// Initialising client
	_transport.onPacket(_handler);
	_transport.enable();
	}

void FirstChatServer::read_cb(ev::io &w, int revents) {
	// Callback for libev loop watching stdin
	std::string s;
	std::getline(std::cin, s); // std::cin >> s;
	assert(_sock);
	_sock->send(s);
}
