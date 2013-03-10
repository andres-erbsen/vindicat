#include "NonceGen64.h"
#include <xtea.h>
#include <sodium/randombytes.h>
#include <cassert>
/// \FIXME where do the endianness functions come from?

NonceGen64::NonceGen64(std::uint32_t const key[4])
	: _next(0)
	{
	_key[0] = key[0];
	_key[1] = key[1];
	_key[2] = key[2];
	_key[3] = key[3];
}

NonceGen64::NonceGen64()
	: _next(0)
	{
	randombytes(reinterpret_cast<unsigned char*>(_key), sizeof(_key));
}

std::string NonceGen64::next() {
	uint32_t v[2];
	*(reinterpret_cast<uint64_t*>(v)) = htole64(_next++);
	assert(_next != 0); // overflow back to 0, we have used up all values

	xtea_enc_32(v,_key);
	return std::string( reinterpret_cast<char*>(v), 8 );
}
