#include <sodium/crypto_box.h>
#include <string>

// Copied from nacl-20110221/crypto_box/wrapper-box.cpp
static inline std::string crypto_box(const std::string &m,
                                     const std::string &n,
                                     const std::string &pk,
                                     const std::string &sk) {
  if (pk.size() != crypto_box_PUBLICKEYBYTES)
    throw "incorrect public-key length";
  if (sk.size() != crypto_box_SECRETKEYBYTES)
    throw "incorrect secret-key length";
  if (n.size() != crypto_box_NONCEBYTES)
    throw "incorrect nonce length";
  std::size_t mlen = m.size() + crypto_box_ZEROBYTES;
  unsigned char mpad[mlen];
  for (int i = 0; i < crypto_box_ZEROBYTES; ++i)
    mpad[i] = 0;
  for (int i = crypto_box_ZEROBYTES; i < mlen; ++i)
    mpad[i] = m[i - crypto_box_ZEROBYTES];
  unsigned char cpad[mlen];
  crypto_box(cpad, mpad, mlen,
             reinterpret_cast<const unsigned char*>(n.c_str()),
             reinterpret_cast<const unsigned char*>(pk.c_str()),
       reinterpret_cast<const unsigned char*>(sk.c_str()));
  return std::string(reinterpret_cast<char*>(cpad + crypto_box_BOXZEROBYTES),
                     mlen - crypto_box_BOXZEROBYTES);
}

// Copied from nacl-20110221/crypto_box/wrapper-keypair.cpp
static inline std::string crypto_box_keypair(std::string *sk_string) {
  unsigned char pk[crypto_box_PUBLICKEYBYTES];
  unsigned char sk[crypto_box_SECRETKEYBYTES];
  crypto_box_keypair(pk, sk);
  *sk_string = std::string(reinterpret_cast<char*>(sk), sizeof(sk));
  return std::string(reinterpret_cast<char*>(pk), sizeof(pk));
}

// Copied from nacl-20110221/crypto_box/wrapper-open.cpp
static inline bool crypto_box_open_to(const std::string &c,
                                      const std::string &n,
                                      const std::string &pk,
                                      const std::string &sk,
                                            std::string *m) {
  if (pk.size() != crypto_box_PUBLICKEYBYTES)
    throw "incorrect public-key length";
  if (sk.size() != crypto_box_SECRETKEYBYTES)
    throw "incorrect secret-key length";
  if (n.size() != crypto_box_NONCEBYTES)
    throw "incorrect nonce length";
  std::size_t clen = c.size() + crypto_box_BOXZEROBYTES;
  unsigned char cpad[clen];
  for (int i = 0; i < crypto_box_BOXZEROBYTES; ++i)
    cpad[i] = 0;
  for (int i = crypto_box_BOXZEROBYTES; i < clen; ++i)
    cpad[i] = c[i - crypto_box_BOXZEROBYTES];
  unsigned char mpad[clen];
  if (crypto_box_open(mpad, cpad, clen,
                      reinterpret_cast<const unsigned char*>(n.c_str()),
                      reinterpret_cast<const unsigned char*>(pk.c_str()),
          reinterpret_cast<const unsigned char*>(sk.c_str())) != 0)
    return 0;
  if (clen < crypto_box_ZEROBYTES)
    throw "ciphertext too short"; // should have been caught by _open
  *m = std::string(reinterpret_cast<char*>(mpad + crypto_box_ZEROBYTES),
                     clen - crypto_box_ZEROBYTES);
}

