#include "nacl25519_nm.h"
#include <sodium/crypto_scalarmult_curve25519.h>

#include <Util.h>
#include <cassert>
#include <cstring>

nacl25519_nm::nacl25519_nm(const std::string& pubk, const std::string& seck) {
  assert(seck.size() == crypto_box_SECRETKEYBYTES);
  for (int i=0; i<crypto_box_SECRETKEYBYTES; ++i) _sk[i] = seck[i];
  pk(pubk);
  crypto_scalarmult_curve25519_base(_our_pk,_sk);
}

nacl25519_nm::nacl25519_nm(const std::string& pubk) {
  randombytes(_sk, crypto_box_SECRETKEYBYTES);
  pk(pubk);
  crypto_scalarmult_curve25519_base(_our_pk,_sk);
}


void nacl25519_nm::pk(const std::string& pubk) {
  assert(pubk.size() == crypto_box_PUBLICKEYBYTES);
  for (int i=0; i<crypto_box_PUBLICKEYBYTES; ++i) _pk[i] = pubk[i];
  crypto_box_beforenm(_k,_pk,_sk);
}

std::string nacl25519_nm::pk() {
  return std::string((char *) _pk, crypto_box_PUBLICKEYBYTES);
}

std::string nacl25519_nm::our_pk() {
  return std::string((char *) _our_pk, crypto_box_PUBLICKEYBYTES);
}

bool nacl25519_nm::nonce_bit() {
  return std::memcmp(_our_pk, _pk, crypto_box_PUBLICKEYBYTES) > 0;
}

std::string nacl25519_nm::
encrypt(const std::string& m, const std::string& n) const {
  if (n.size() != crypto_box_NONCEBYTES) throw "incorrect nonce length";  
  int mlen = m.size() + crypto_box_ZEROBYTES;
  unsigned char mpad[mlen];
  for (int i=0; i< crypto_box_ZEROBYTES; ++i) mpad[i] = 0;
  for (int i=crypto_box_ZEROBYTES; i<mlen; ++i)
    mpad[i] = m[i - crypto_box_ZEROBYTES];
  unsigned char cpad[mlen];
  crypto_box_afternm(cpad, mpad, mlen, (const unsigned char*) n.data(), _k);
  return std::string(  (char*) cpad+crypto_box_BOXZEROBYTES,
      mlen-crypto_box_BOXZEROBYTES );
}

bool nacl25519_nm::
decrypt(const std::string& c, const std::string& n, std::string& m) const {
  if (n.size() != crypto_box_NONCEBYTES) throw "incorrect nonce length";  
  int clen = c.size() + crypto_box_BOXZEROBYTES;
  unsigned char cpad[clen];
  for (int i = 0;i < crypto_box_BOXZEROBYTES;++i) cpad[i] = 0;
  for (int i = crypto_box_BOXZEROBYTES;i < clen;++i)
    cpad[i] = c[i - crypto_box_BOXZEROBYTES];
  unsigned char mpad[clen];
  if (crypto_box_open_afternm(mpad,cpad,clen,
      (const unsigned char *) n.data(), _k ) != 0) {
    return 0;
  }
  m = std::string( (char *) mpad + crypto_box_ZEROBYTES,
    clen - crypto_box_ZEROBYTES );  
  return 1;
}

