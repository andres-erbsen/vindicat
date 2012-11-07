#include "CryptoIdentity.hpp"

#include <randombytes.h>
#include <crypto_box.h>

CryptoIdentity::CryptoIdentity() {
	randombytes(_secretkey_edsig, sizeof(ed25519_secret_key));	
	ed25519_publickey(_secretkey_edsig, &(_verkey_edsig[0]));

	_enckey_naclbox = crypto_box_keypair(&_secretkey_naclbox);;
};


bool CryptoIdentity::pick_key(const DeviceInfo& recipient, EncKey& key) {
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

bool CryptoIdentity::envelope( const std::string& message
                             ,       EncEnvelope& dst
                             ,       std::string nonce
                             , const EncKey& key)
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


bool CryptoIdentity::open(const EncEnvelope& enc, std::string& dst) {
	if ( enc.algo() == PkencAlgo::IDENTITY_ENC ) {
		dst = enc.ciphertext();
	} else if ( enc.algo() == PkencAlgo::CURVE25519XSALSA20POLY1305 ) {
		std::string nonce = enc.nonce();
		nonce.resize(crypto_box_NONCEBYTES);
		try {
			dst = crypto_box_open( enc.ciphertext()
					     , nonce
					     , enc.enckey()
					     , _secretkey_naclbox);
		} catch(...) {
			return 0;
		} 
		return 1;
	}
}


bool CryptoIdentity::sign(const std::string& message, Signature& sig) {
	ed25519_signature rawsig;
	ed25519_sign( reinterpret_cast<const unsigned char*>( message.data() )
	            , message.size()
	            , _secretkey_edsig
	            , _verkey_edsig
	            , rawsig);
	sig.set_sig ( std::string( reinterpret_cast<char*>(rawsig)
				, sizeof(ed25519_signature)  )   );
	sig.set_algo( SigAlgo::ED25519 );
	return 1;
}


// TODO: where to put this function?
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
	for ( int i=0; i<device.sig_keys_size(); ++i ) {
		if ( verify(message, sig, device.sig_keys(i) ) ) return 1;
	}
	return 0;
}

