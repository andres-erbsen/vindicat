#include "Device.h"
#include "Transport.h"
#include "Forwarding.h"
#include "CryptoIdentity.h"
#include "keccak128.h"

#include <ed25519.h>
#include <algorithm>


const std::vector<std::string>&  Device::ids()   const { return _ids;   }
uint64_t                         Device::mtime() const { return _mtime; }
PkencAlgo                     Device::enc_algo() const { return _enc_algo; }

// TOOD: move to an utils file
template<typename C, typename E>
static bool contains(C c,E e) {
	return std::find(c.begin(), c.end(), e) != c.end();
}

SigAlgo Device::sig_algo() const {
	if ( contains(_sig_algos, SigAlgo::ED25519) ) return SigAlgo::ED25519;
	assert(0);
}

std::vector< std::weak_ptr<DeviceBusinesscard> > Device::cards() const {
	std::vector< std::weak_ptr<DeviceBusinesscard> > ret(_cards.size());
	for (const auto& p : _cards)
		ret.push_back( std::weak_ptr<DeviceBusinesscard>(p) );
	return ret;
}

static bool verifySig( const std::string& message
                     , const std::string& sig
                     , SigAlgo algo
                     , const std::string& key ) {
	if ( algo == SigAlgo::ED25519 ) {
		if ( sig.size() != sizeof(ed25519_signature) ) return 0;
		if ( key.size() != sizeof(ed25519_public_key) ) return 0;
		return 0 == ed25519_sign_open(
				reinterpret_cast<const unsigned char*>( message.data()   )
		      ,                                         message.size()
		      , reinterpret_cast<const unsigned char*>( key.data() )
		      , reinterpret_cast<const unsigned char*>( sig.data() )
			  );
	}
	return 0;
}

// Deserialization

bool Device::parseFrom(std::shared_ptr<DeviceBusinesscard>&& card_p) {
	clear();
	assert (card_p.unique());
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
		const SigAlgo& algo = dev_info.sig_algos(i);
		if ( verifySig(card_p->device_info_msg(), sig, algo, key) ) {
			got = 1;
			_sig_keys.push_back( key );
			_sig_algos.push_back( algo );
			_ids.push_back( keccak128(key) );
		}
	}

	// Extract other information
	if (!got) return 0;
	_cards.push_back(card_p);
	_mtime = dev_info.has_time() ? dev_info.time() : 0;

	// Pick favorite encryption key
	n = dev_info.enc_keys_size();
	n = std::min(n, dev_info.enc_algos_size());
	for (int i=0; i<n; i++) {
		const PkencAlgo& algo = dev_info.enc_algos(i);
		if ( algo == PkencAlgo::CURVE25519XSALSA20POLY1305 ) {
			_enc_algo = algo;
			_enc_key = dev_info.enc_keys(i);
		}
	}
	return 1;
}

void Device::merge(Device&& other) {
	assert(0); // TODO: implement
}


// Device is a container for forwardings...

void Device::addForwarding(std::shared_ptr<Forwarding>&& fwd) {
	_forwardings.insert(std::make_pair(fwd->id(), std::move(fwd)));
}

std::weak_ptr<Forwarding> Device::getForwarding(uint32_t id) {
	auto it = _forwardings.find(id);
	if (it == _forwardings.end() ) return std::weak_ptr<Forwarding>();
	return it->second;
}

void Device::removeForwarding(uint32_t id) {
	_forwardings.erase(id);
}


void Device::tsocket(std::shared_ptr<TransportSocket> socket) {
	_tsocket = socket;
}

bool Device::send(const std::string& packet) {
	_tsocket->send(packet);
}


void Device::clear() {
	_sig_keys.clear();
	_sig_algos.clear();
	_ids.clear();
	_cards.clear();
	_mtime = 0;
	_enc_key = "";
}
