#include "Device.h"
#include "Forwarding.h"
#include "CryptoIdentity.h"
#include "keccak128.h"
#include "Util.h"
#include "nacl25519_nm.h"

#include <sodium/crypto_sign_ed25519.h>
#include <algorithm>


uint64_t Device::mtime() const {
  return _mtime;
}

const std::string& Device::id() const {
  return _ids[ _sig.at(sig_algo()) ];
}

const std::vector<std::string>& Device::ids() const {
  return _ids;
}

const std::string& Device::enc_key() const {
  return _enc.at( enc_algo() );
}

std::shared_ptr<DeviceBusinesscard> Device::card() const {
  return _card;
}

SigAlgo Device::sig_algo() const {
  static const std::vector<SigAlgo>
    preference {SigAlgo::ED25519};
  for (const auto& a : preference) if ( _sig.find(a) != _sig.end() ) return a;
  assert(0);
}

PkencAlgo Device::enc_algo() const {
  static const std::vector<PkencAlgo>
    preference {PkencAlgo::CURVE25519XSALSA20POLY1305};
  for (const auto& a : preference) if ( _enc.find(a) != _enc.end() ) return a;
  assert(0);
}

static bool verifySig( const std::string& message
                     , const std::string& sig
                     , SigAlgo algo
                     , const std::string& key ) {
  if ( algo == SigAlgo::ED25519 ) {
    if ( sig.size() != crypto_sign_ed25519_BYTES ) return 0;
    if ( key.size() != crypto_sign_ed25519_PUBLICKEYBYTES ) return 0;
    std::string msg = sig+message;
    unsigned char *m = new unsigned char[msg.size()];
    unsigned long long mlen = 0;
    auto res = crypto_sign_ed25519_open(m, &mlen,
      reinterpret_cast<const unsigned char*>(msg.data()),
      msg.size()
          , reinterpret_cast<const unsigned char*>(key.data()));
    delete[] m;
    return res == 0;
  }
  return 0;
}

bool Device::verifySignature(const std::string& message, const std::string& sig
    , SigAlgo algo) const {
  auto it = _sig.find(algo);
  if (it == _sig.end()) return 0;
  return verifySig(message, sig, algo, _sig_keys[ it->second ]);
}

bool Device::open( const std::string& ct
                 , std::string nonce
                 , const std::string& our_sk
                 , PkencAlgo algo
                 , std::string& ret
                 ) const {
  auto it = _enc.find(algo);
  if (it == _enc.end()) return 0;
  const std::string& pk = it->second;
  return nacl25519_nm(pk, our_sk).decrypt(ct, nonce, ret);
}

// Device is a container for forwardings...

void Device::addForwarding(std::shared_ptr<ForeignForwarding>&& fwd) {
  fwd->owner(shared_from_this());
  _forwardings.insert(std::make_pair(fwd->id(), std::move(fwd)));
}

std::shared_ptr<ForeignForwarding> Device::getForwarding(uint64_t id) {
  auto it = _forwardings.find(id);
  if (it == _forwardings.end() ) return nullptr;
  return it->second;
}

void Device::removeForwarding(uint64_t id) {
  _forwardings.erase(id);
}


// Deserialization

bool Device::parseFrom(std::shared_ptr<DeviceBusinesscard> card_p) {
  clear();
  // Is there any hope to use this bcard at all?
  DeviceInfo dev_info;
  if ( ! card_p->has_device_info_msg() 
    || ! card_p->sigs_size() 
    || ! dev_info.ParseFromString(card_p->device_info_msg()) ) {
    return 0;
  }
  if ( ! dev_info.enc_keys_size() ) return 0;

  // Are there valid signatures?
  int n = card_p->sigs_size();
  n = std::min(n, dev_info.sig_keys_size());
  n = std::min(n, dev_info.sig_algos_size());
  bool got = 0;
  for (int i=0; i<n; ++i) {
    const std::string& sig = card_p->sigs(i);
    const std::string& key = dev_info.sig_keys(i);
    const SigAlgo algo = static_cast<SigAlgo>( dev_info.sig_algos(i) );
    if ( verifySig(card_p->device_info_msg(), sig, algo, key) ) {
      got = 1;
      _sig.insert( std::make_pair(algo, _ids.size()) );
      _ids.push_back( keccak128(key) );
      _sig_keys.push_back( key );
    }
  }

  // Extract other information
  if (!got) return 0;
  _mtime = dev_info.has_time() ? dev_info.time() : 0;

  n = dev_info.enc_keys_size();
  n = std::min(n, dev_info.enc_algos_size());
  for (int i=0; i<n; i++) {
    const PkencAlgo algo = static_cast<PkencAlgo>( dev_info.enc_algos(i) );
    _enc.insert( std::make_pair(algo, dev_info.enc_keys(i)) );
  }

  std::swap(_card, card_p);
  return 1;
}

bool Device::parseFrom(const std::string& card_string) {
  auto card = std::make_shared<DeviceBusinesscard>();
  if ( ! card->ParseFromString( card_string ) ) return 0;
  return parseFrom(card);
}

void Device::merge(Device&& other) {
  for (const auto& kvp : other._forwardings) {
    addForwarding( other.getForwarding(kvp.first) );
    other.removeForwarding(kvp.first);
  }
  if (other.mtime() > mtime()) {
    std::swap(_sig, other._sig);
    std::swap(_ids, other._ids);
    std::swap(_sig_keys, other._sig_keys);
    std::swap(_enc, other._enc);
    std::swap(_mtime, other._mtime);
    std::swap(_card, other._card);
  }
}

void Device::clear() {
  _sig.clear();
  _ids.clear();
  _sig_keys.clear();
  _enc.clear();
  _forwardings.clear();
  _mtime = 0;
  _card.reset();
}
