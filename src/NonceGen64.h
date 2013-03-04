#ifndef NONCEGEN64_H_
#define NONCEGEN64_H_

#include <cstdint>
#include <string>

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

void xtea_enc(uint32_t v[2], uint32_t const key[4]);
void xtea_dec(uint32_t v[2], uint32_t const key[4]);

#endif // NONCEGEN64_H_
