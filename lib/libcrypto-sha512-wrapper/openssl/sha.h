#ifndef OPENSSL_SHA_H_
#define OPENSSL_SHA_H_

#undef inline
#include <sodium/crypto_hash.h>
#include <stdlib.h>

typedef struct
{
  unsigned char *data;
  size_t length;
} SHA512_CTX;

unsigned char *SHA512(const unsigned char *d, unsigned long n, unsigned char *md)
{
  crypto_hash_sha512(md, d, n);
  return md;
}

int SHA512_Init(SHA512_CTX *c)
{
  c->data = NULL;
  c->length = 0;
  return 1;
}

int SHA512_Update(SHA512_CTX *c, const void *data, unsigned long len)
{
  unsigned char *area = (unsigned char*)realloc(c->data, c->length+len);
  int i;
  if(!area)
    return 0;
  c->data = area;
  for(i = 0; i < len; i++)
    c->data[c->length+i] = ((unsigned char*)data)[i];
  c->length += len;
  return 1;
}

int SHA512_Final(unsigned char *md, SHA512_CTX *c)
{
  crypto_hash_sha512(md, c->data, c->length);
  free(c->data);
  c->length = 0;
  return 1;
}

#endif // OPENSSL_SHA_H_
