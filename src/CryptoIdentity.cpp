#include "CryptoIdentity.h"

#include <Util.h>
#include <crypto_box.h>

#include <ctime>
#include <cassert>

CryptoIdentity::CryptoIdentity(): _our_businesscard(new DeviceBusinesscard) {
	randombytes(_secretkey_edsig, sizeof(ed25519_secret_key));	
	ed25519_publickey(_secretkey_edsig, &(_verkey_edsig[0]));

	_enckey_naclbox = crypto_box_keypair(&_secretkey_naclbox);;
	update_businesscard();
};

bool CryptoIdentity::encrypt( const std::string& message
                            , std::string nonce
                            , PkencAlgo algo
                            , const std::string& pk
                            , std::string& ret
                            ) const {
	assert(nonce.size() >= 8);
	if ( algo == PkencAlgo::CURVE25519XSALSA20POLY1305 ) {
		if ( pk.size() != crypto_box_PUBLICKEYBYTES ) return 0;
		nonce.resize(crypto_box_NONCEBYTES);
		ret = crypto_box( message, nonce, pk, _secretkey_naclbox );
		return 1;
	}
	return 0;
}


bool CryptoIdentity::open( const std::string& ct
                         , std::string nonce
                         , const std::string& pk
                         , PkencAlgo algo
                         , std::string& ret
                         ) const {
	if ( algo == PkencAlgo::CURVE25519XSALSA20POLY1305 ) {
		nonce.resize(crypto_box_NONCEBYTES);
		try { /// \TODO remove exceptions
			ret = crypto_box_open( ct, nonce, pk, _secretkey_naclbox );
			return 1;
		} catch(...) {
			return 0;
		} 
	}
	return 0;
}


bool CryptoIdentity::sign(const std::string& message, SigAlgo algo, std::string& ret) const {
	assert( algo == SigAlgo::ED25519 );
	ed25519_signature rawsig;
	ed25519_sign( reinterpret_cast<const unsigned char*>( message.data() )
	            , message.size()
	            , _secretkey_edsig
	            , _verkey_edsig
	            , rawsig);
	ret = std::string( reinterpret_cast<char*>(rawsig)
	                 , sizeof(ed25519_signature) );
	return 1;
}




void CryptoIdentity::update_businesscard() {
	DeviceInfo dev;
	dev.add_sig_algos( enumval(SigAlgo::ED25519) );
	*dev.add_sig_keys() = std::string( reinterpret_cast<const char*>(_verkey_edsig)
	                                 , sizeof(_verkey_edsig));

	dev.add_enc_algos( enumval(PkencAlgo::CURVE25519XSALSA20POLY1305) );
	*dev.add_enc_keys() = _enckey_naclbox;

	dev.set_time( std::time(NULL) );

	_our_businesscard->set_device_info_msg( dev.SerializeAsString() );
	sign(_our_businesscard->device_info_msg(), SigAlgo::ED25519, *_our_businesscard->add_sigs());
}

std::shared_ptr<DeviceBusinesscard> CryptoIdentity::our_businesscard() const {
	return _our_businesscard;
}
