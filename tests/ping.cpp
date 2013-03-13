#include <iostream>
#include <sstream>
#include <random>
#include <cstdint>
#include <libvindicat.h>
#include <unistd.h>
#include <cerrno>
#include <system_error>
#include <sys/types.h>
#include <signal.h>

template <typename T> std::string toString(const T& value) {
  std::stringstream ss;
  ss << value;
  return ss.str();
}

int main(int argc, char *argv[]) {
  pid_t server[2];
  std::random_device rand;
  std::string server_port = toString(std::uniform_int_distribution<std::uint16_t>(1025)(rand));

  if((server[0] = fork()) == 0) {
    char *args[] = {argv[1], "-s", "::1", const_cast<char*>(server_port.c_str()), nullptr};
    execv(argv[1], args);
    throw std::system_error(errno, std::system_category());
  } else if(server[0] == -1) {
    throw std::system_error(errno, std::system_category());
  }

  if((server[1] = fork()) == 0) {
    char *args[] = {argv[1], "-c", "::1", const_cast<char*>(server_port.c_str()), nullptr};
    execv(argv[1], args);
    throw std::system_error(errno, std::system_category());
  } else if(server[1] == -1) {
    throw std::system_error(errno, std::system_category());
  }

  sleep(2);
  for(auto pid : server)
    if(kill(pid, 0) == -1)
      throw std::system_error(errno, std::system_category());

  try {
    libvindicat::Connection conn_server("/tmp/vindicat."+toString(server[0]), "/tmp/test1"), conn_client("/tmp/vindicat."+toString(server[1]), "/tmp/test2");

    libvindicat::RawSocket *server_socket = conn_server.forward(0xFE);
    libvindicat::RawSocket *client_socket = conn_client.forward(0xFE);

    conn_server.read();
    conn_client.read();

    bool from_server = false, from_client = false;

    server_socket->set_callback([&from_server](const std::string&, const std::string&) {from_server = true;});
    client_socket->set_callback([&from_client](const std::string&, const std::string&) {from_client = true;});
    server_socket->sendto(conn_client.identifier(), "PING");
    client_socket->sendto(conn_server.identifier(), "PING");

    for(int i = 0; (i < 2) && !(from_server && from_client); i++) {
      conn_server.read();
      conn_client.read();
    }

    kill(server[0], SIGINT);
    kill(server[1], SIGINT);

    return (from_server && from_client? 0: 1);
  } catch(...) {
    kill(server[0], SIGINT);
    kill(server[1], SIGINT);
    throw;
  }
  return 1;
}
