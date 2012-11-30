#include "CryptoIdentity.hpp"

#include <randombytes.h>
#include <crypto_box.h>
#include <crypto_hash_sha256.h>

#include <google/protobuf/repeated_field.h>

#include <algorithm>
#include <ctime>

std::vector<std::string> identifiers(const std::string& keybytes) {
	return {keybytes.substr(0,16), crypto_hash_sha256(keybytes).substr(0,16)};
}

CryptoIdentity::CryptoIdentity() {
	randombytes(_secretkey_edsig, sizeof(ed25519_secret_key));	
	ed25519_publickey(_secretkey_edsig, &(_verkey_edsig[0]));

	_enckey_naclbox = crypto_box_keypair(&_secretkey_naclbox);;

	_our_identifiers = identifiers(std::string(
				reinterpret_cast<char*>(_verkey_edsig)
				,sizeof(ed25519_public_key))
	);
};


bool CryptoIdentity::envelope( const std::string& message
                             ,       EncEnvelope& dst
                             ,       std::string nonce
                             , const EncKey& key) const
{
	if ( key.algo() == PkencAlgo::CURVE25519XSALSA20POLY1305 ) {
		if ( key.key().size() != crypto_box_PUBLICKEYBYTES ) return 0;
		nonce.resize(crypto_box_NONCEBYTES);
		dst.set_ciphertext( crypto_box( message
					      , nonce
					      , key.key()
					      , _secretkey_naclbox) );
		dst.set_algo( key.algo() );
		return 1;
	}
	return 0;
}


bool CryptoIdentity::open(const EncEnvelope& enc, std::string& dst) const {
	if ( enc.algo() == PkencAlgo::IDENTITY_ENC ) {
		dst = enc.ciphertext();
		return 1;
	} else if ( enc.algo() == PkencAlgo::CURVE25519XSALSA20POLY1305 ) {
		std::string nonce = enc.nonce();
		nonce.resize(crypto_box_NONCEBYTES);
		try {
			dst = crypto_box_open( enc.ciphertext()
                                 , nonce
                                 , enc.enckey()
                                 , _secretkey_naclbox );
		} catch(...) {
			return 0;
		} 
		return 1;
	}
}


bool CryptoIdentity::sign(const std::string& message, Signature& sig) const {
	ed25519_signature rawsig;
	ed25519_sign( reinterpret_cast<const unsigned char*>( message.data() )
	            , message.size()
	            , _secretkey_edsig
	            , _verkey_edsig
	            , rawsig);
	sig.Clear();
	sig.set_sig ( std::string( reinterpret_cast<char*>(rawsig)
				, sizeof(ed25519_signature)  )   );
	sig.set_algo( SigAlgo::ED25519 );
	return 1;
}

void CryptoIdentity::our_businesscard(DeviceBusinesscard& card) const {
	// TODO: do not recompute this ecery time!
	DeviceInfo dev;
	{
		SigKey key;
		key.set_algo(SigAlgo::ED25519);
		key.set_key(_verkey_edsig, sizeof(_verkey_edsig) );
		*dev.add_sig_keys() = key;
	}
	{
		EncKey key;
		key.set_algo(PkencAlgo::CURVE25519XSALSA20POLY1305);
		key.set_key(_enckey_naclbox);
		*dev.add_enc_keys() = key;
	}
	dev.set_time( std::time(NULL) );

	std::string infomsg;
	dev.SerializeToString(&infomsg);
	card.set_device_info_msg(infomsg);

	Signature sig;
	sign(infomsg, sig);
	*card.add_sigs() = sig;

	dev.Clear();
	assert( verify(card,dev) );
}

const std::vector<std::string>& CryptoIdentity::our_identifiers() const {
	return _our_identifiers;
}

// TODO: where to put these functions?

bool pick_key(const DeviceInfo& recipient, EncKey& key) {
	int bestval = 0;
	for (int i=0; i<recipient.enc_keys_size(); ++i ) {
		const EncKey& k = recipient.enc_keys(i);
		if ( k.algo() == PkencAlgo::CURVE25519XSALSA20POLY1305 
		   && k.key().size() == crypto_box_PUBLICKEYBYTES) {
			bestval = 1;
			key = k;
		}
	}
	return (bestval > 0);
}


bool verify( const std::string& message
           , const Signature& sig
           , const SigKey& key)
{
	if ( sig.algo() != key.algo() ) return 0;
	if ( key.algo() == SigAlgo::ED25519 ) {
		if ( sig.sig().size() != sizeof(ed25519_signature) ) return 0;
		if ( key.key().size() != sizeof(ed25519_public_key) ) return 0;
		return 0 == ed25519_sign_open(
				reinterpret_cast<const unsigned char*>( message.data()   )
		      ,                                         message.size()
		      , reinterpret_cast<const unsigned char*>( key.key().data() )
		      , reinterpret_cast<const unsigned char*>( sig.sig().data() )
			  );
	}
	return 0;
}

bool verify( const std::string& message
           , const Signature& sig
           , const DeviceInfo& device )
{
	for ( const auto& key : device.sig_keys() ) {
		if ( verify(message, sig, key ) ) return 1;
	}
	return 0;
}

bool verify(const DeviceBusinesscard& card, DeviceInfo& ret) {
	ret.Clear(); // TODO: really not needed??
	if ( ! card.sigs_size() 
	  || ! card.has_device_info_msg() 
	  || ! ret.ParseFromString(card.device_info_msg()) ) {
		return 0;
	}
	google::protobuf::RepeatedPtrField<SigKey> verified_keys;
	google::protobuf::RepeatedPtrField<std::string> verified_identifiers;
	int n = std::min(ret.sig_keys_size(), card.sigs_size());
	bool got = 0;
	for (int i=0; i<n; ++i) {
		const Signature& sig = card.sigs(i);
		const SigKey&    key = ret.sig_keys(i);
		if ( verify(card.device_info_msg(), sig, key) ) {
			got = 1;
			*verified_keys.Add() = key;
			// the signing key itself is the main identity
			// also happens to be the hash of itself using id()
			*verified_identifiers.Add() = key.key().substr(0,16);
			// SHA256-128
			*verified_identifiers.Add() = crypto_hash_sha256(key.key()).substr(0,16);
		}
	}
	verified_keys.Swap( ret.mutable_sig_keys() );
	verified_identifiers.Swap( ret.mutable_identifiers() );
	return got;
}


bool verify( const LinkPromise& promise
           , const DeviceInfo& ldev
           , const DeviceInfo& rdev
		   , LinkInfo& ret) {
	ret.Clear(); // TODO: really not needed??
	if ( ! promise.has_link_info_msg()
	  || ! promise.left_sigs_size() 
	  || ! promise.right_sigs_size() 
	  || ! ret.ParseFromString(promise.link_info_msg()) ) {
		return 0;
	}
	const auto& msg = promise.link_info_msg();
	return (1 
	&&     std::any_of( ldev.identifiers().begin(), ldev.identifiers().end()
	                  , [&](const std::string& id) {return id == ret.left ();} )
	&&     std::any_of( rdev.identifiers().begin(), rdev.identifiers().end()
	                  , [&](const std::string& id) {return id == ret.right();} )
	&& std::any_of( promise.left_sigs().begin(), promise.left_sigs().end()
	              , [&](const Signature& sig) {return verify(msg, sig, ldev);} )
	&& std::any_of( promise.right_sigs().begin(), promise.right_sigs().end()
	              , [&](const Signature& sig) {return verify(msg, sig, rdev);} )
	);
}


bool verify( const LinkProposal& proposal
           , const DeviceInfo& ldev
		   , LinkInfo& ret) {
	ret.Clear(); // TODO: really not needed?::?
	if ( ! proposal.has_link_info_msg()
	  || ! proposal.left_sigs_size() 
	  || ! ret.ParseFromString(proposal.link_info_msg()) ) {
		return 0;
	}
	const auto& msg = proposal.link_info_msg();
	return (1 
	&& std::any_of( ldev.identifiers().begin(), ldev.identifiers().end()
	              , [&](const std::string& id) {return id == ret.left ();} )
	&& std::any_of( proposal.left_sigs().begin(), proposal.left_sigs().end()
	              , [&](const Signature& sig) {return verify(msg, sig, ldev);} )
	);
}
