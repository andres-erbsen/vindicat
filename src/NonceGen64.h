#ifndef NONCEGEN64_H_
#define NONCEGEN64_H_

#include <cstdint>
#include <string>

///
/// \brief Generate nonces by encrypting a counter with XTEA.
///
class NonceGen64 {
public:
	NonceGen64();
	NonceGen64(std::uint32_t const key[4]);
	std::string next();
private:
	friend class NonceSession; // uses the key
	std::uint32_t _key[4];
	std::uint64_t _next;
};

#endif // NONCEGEN64_H_
