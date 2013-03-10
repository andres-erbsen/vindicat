#include "NonceSession.h"
#include <xtea.h>
#include <cassert>

NonceSession::NonceSession(std::uint32_t const key[4], bool parity)
  : _gen(key)
  , _parity(parity ? 1 : 0)
  , _their_next(0)
  {}

NonceSession::NonceSession(const std::string& key, bool parity)
  : _gen(( assert(key.size() == 16)
         , reinterpret_cast<const uint32_t*>(key.data()) ))
  , _parity(parity ? 1 : 0)
  , _their_next(0)
  {}

std::string NonceSession::next() {
  std::string ret;
  do ret = _gen.next(); while ( (*ret.rbegin()&1) != _parity );
  assert(ret.size() == 8);
  return ret;
}

bool NonceSession::open(const std::string& their_nonce) {
  if (their_nonce.size() != 8) return 0;
  if ((*their_nonce.rbegin()&1) == _parity) return 0;

  uint32_t v[2];
  v[0] = *(reinterpret_cast<const uint32_t*>(their_nonce.data()  ));
  v[1] = *(reinterpret_cast<const uint32_t*>(their_nonce.data()+4));
  xtea_dec_32(v, _gen._key);

  // is it newer than the previous one, i.e., not a replay attack?
  uint64_t x = le64toh( *(reinterpret_cast<uint64_t*>(v)) );
  if (x >= _their_next) {
    _their_next = x+1;
    return 1;
  }
  return 0;
}
