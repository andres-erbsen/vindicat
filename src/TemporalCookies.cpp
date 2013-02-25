#include "TemporalCookies.h"
#include "Util.h"
#include <cassert>

TemporalCookies::TemporalCookies(float refresh_interval) {
	randombytes(_minutekey, sizeof(_minutekey));
	randombytes(_lastminutekey, sizeof(_lastminutekey));
	_w.set(refresh_interval, refresh_interval);
	_w.set(this);
	_w.start();
}

std::string TemporalCookies::cookie(const std::string& plain) const {
	size_t len = plain.size() + crypto_secretbox_ZEROBYTES;
	unsigned char mpad[len];
	unsigned int i;
	for (i=0; i<crypto_secretbox_ZEROBYTES; ++i) mpad[i] = 0;
	for (i=crypto_secretbox_ZEROBYTES; i<len; ++i) mpad[i] = plain[i - crypto_secretbox_ZEROBYTES];
	unsigned char cpad[len];
	unsigned char nonce[crypto_secretbox_NONCEBYTES];
	assert(crypto_secretbox_NONCEBYTES >= 16);
	randombytes(nonce,16);
	for (i = 16; i<crypto_secretbox_NONCEBYTES; ++i) nonce[i] = 0;
	crypto_secretbox(cpad, mpad, len, nonce, _minutekey );
	return std::string((char*) nonce,16) + std::string(
		(char *) cpad + crypto_secretbox_BOXZEROBYTES,
		len - crypto_secretbox_BOXZEROBYTES
	);
}

bool TemporalCookies::
open(const std::string& cookie, std::string& ret) const {
	if (cookie.size() < 32) return 0;
	// 16 first bytes are nonce
	unsigned char nonce[crypto_secretbox_NONCEBYTES];
	assert(crypto_secretbox_NONCEBYTES >= 16);
	unsigned int i;
	for (i = 0; i<16; ++i) nonce[i] = cookie[i];
	for (i = 16; i<crypto_secretbox_NONCEBYTES; ++i) nonce[i] = 0;

	size_t clen = cookie.size() - 16 + crypto_secretbox_BOXZEROBYTES;
	assert(clen >= crypto_secretbox_ZEROBYTES);
	unsigned char cpad[clen];
	for (i=0; i<crypto_secretbox_BOXZEROBYTES; ++i) cpad[i] = 0;
	for (i=crypto_secretbox_BOXZEROBYTES; i<clen; ++i) cpad[i] = cookie[i + 16 - crypto_secretbox_BOXZEROBYTES];
	unsigned char mpad[clen];
	if ( crypto_secretbox_open(mpad, cpad, clen, nonce, _minutekey)
	  && crypto_secretbox_open(mpad, cpad, clen, nonce, _lastminutekey) ) {
		return 0;
	}
	ret = std::string(
		(char *) mpad + crypto_secretbox_ZEROBYTES,
		clen - crypto_secretbox_ZEROBYTES
	);
	return 1;
}


void TemporalCookies::operator() (ev::timer& /*w*/, int /*revents*/) {
	unsigned int i;
	for (i=0; i<sizeof(_minutekey); ++i) _lastminutekey[i] = _minutekey[i];
	randombytes(_minutekey, sizeof(_minutekey));
}
