#ifndef NONCESESSION64_H_
#define NONCESESSION64_H_

#include <string>
#include <cstdint>
#include "NonceGen64.h"

class NonceSession {
public:
  NonceSession(std::uint32_t const key[4], bool);
  NonceSession(const std::string&, bool);
  std::string next();  
  bool open(const std::string&);
private:
  NonceGen64 _gen;
  char _parity;
  uint64_t _their_next;
};

#endif // NONCESESSION64_H_
