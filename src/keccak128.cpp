#include <KeccakSponge.h>

#include <string>

void keccak128(const unsigned char* in, const unsigned long long inlen, unsigned char *out) {
    spongeState state;
    InitSponge(&state, 1344, 256);
    Absorb(&state, in, inlen*8);
    Squeeze(&state, out, 128);
}


std::string keccak128(const std::string& in) {
  char out[16];
  keccak128( reinterpret_cast<const unsigned char*>( in.data() )
           , in.size()
           , reinterpret_cast<unsigned char*>(out) );
  return std::string(out, 16);
}
