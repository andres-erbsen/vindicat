#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif
/// \brief Encrypt using XTEA.
///
/// Code from https://en.wikipedia.org/wiki/XTEA
/// \param v 64-bit plaintext
/// \param key 128-bit key
void xtea_enc_32(uint32_t v[2], uint32_t const key[4]);
/// \brief Decrypt using XTEA.
///
/// Code from https://en.wikipedia.org/wiki/XTEA
/// \param v  64-bit ciphertext
/// \param key 128-bit key
void xtea_dec_32(uint32_t v[2], uint32_t const key[4]);

#ifdef __cplusplus
}
#endif
