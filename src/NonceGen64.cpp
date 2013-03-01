#include "NonceGen64.h"
#include <randombytes.h>
#include <cassert>

NonceGen64::NonceGen64(std::uint32_t const key[4]) {
	_key[0] = key[0];
	_key[1] = key[1];
	_key[2] = key[2];
	_key[3] = key[3];
	_next = 0;
}

NonceGen64::NonceGen64() {
	randombytes(reinterpret_cast<unsigned char*>(_key), sizeof(_key));
	_next = 0;
}

// https://en.wikipedia.org/wiki/XTEA
// take 64 bits of data in v[0] and v[1] and 128 bits of key[0] - key[3]
static void xtea_enc(uint32_t v[2], uint32_t const key[4]) {
    uint32_t v0=v[0], v1=v[1], sum=0, delta=0x9E3779B9;
    for (unsigned i=32; i; --i) { // 32 cycles = 64 feistel rounds
        v0 += (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
        sum += delta;
        v1 += (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum>>11) & 3]);
    }
    v[0]=v0; v[1]=v1;
}

std::string NonceGen64::next() {
	uint32_t v[2];
	*(reinterpret_cast<uint64_t*>(v)) = _next;
	++_next;
	assert(_next != 0); // overflow back to 0, we have used up all values
	xtea_enc(v,_key);
	return std::string( reinterpret_cast<char*>(v), 8 );
}
