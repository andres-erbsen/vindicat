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

int main(int argc, char *argv[]) {
  pid_t server[2];
  std::random_device rand;
  std::string server_port = dynamic_cast<std::stringstream&>(std::stringstream() << std::uniform_int_distribution<std::uint16_t>(1025)(rand)).str();

  if((server[0] = fork()) == 0) {
    char *args[] = {argv[1], "-s", "::", const_cast<char*>(server_port.c_str()), nullptr};
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

  std::cout << "server: " << server[0] << std::endl;
  std::cout << "client: " << server[1] << std::endl;

  sleep(2);

  kill(server[0], SIGINT);
  kill(server[1], SIGINT);

  return 0;
}
