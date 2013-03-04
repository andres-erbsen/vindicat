#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void xtea_enc_32(uint32_t v[2], uint32_t const key[4]);
void xtea_dec_32(uint32_t v[2], uint32_t const key[4]);

#ifdef __cplusplus
}
#endif
