#include <stdint.h>

// https://en.wikipedia.org/wiki/XTEA
// take 64 bits of data in v[0] and v[1] and 128 bits of key[0] - key[3]
void xtea_enc_32(uint32_t v[2], uint32_t const key[4]) {
    uint32_t v0=v[0], v1=v[1], sum=0, delta=0x9E3779B9;
	unsigned i;
    for (i=32; i; --i) { // 32 cycles = 64 feistel rounds
        v0 += (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
        sum += delta;
        v1 += (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum>>11) & 3]);
    }
    v[0]=v0; v[1]=v1;
}

void xtea_dec_32(uint32_t v[2], uint32_t const key[4]) {
    uint32_t v0=v[0], v1=v[1], delta=0x9E3779B9, sum=delta*32;
	unsigned i;
    for (i=32; i; --i) { // 32 cycles = 64 feistel rounds
        v1 -= (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum>>11) & 3]);
        sum -= delta;
        v0 -= (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
    }
    v[0]=v0; v[1]=v1;
}

