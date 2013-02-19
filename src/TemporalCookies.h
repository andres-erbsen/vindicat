#ifndef TEMPORALCOOKIES_H_
#define TEMPORALCOOKIES_H_

#include <crypto_secretbox.h>
#include <ev++.h>
#include <string>

class TemporalCookies {
public:
	TemporalCookies(float refresh_interval=60.0);
	TemporalCookies(const TemporalCookies&) = delete;
	TemporalCookies(TemporalCookies&&) = default;
	const TemporalCookies& operator= (const TemporalCookies&) = delete;

	std::string cookie(const std::string&);
	bool open(const std::string&, std::string&);

	void operator() (ev::timer&, int);
private:
	ev::timer _w;

	unsigned char _minutekey[crypto_secretbox_KEYBYTES];
	unsigned char _lastminutekey[crypto_secretbox_KEYBYTES];
};


#endif // TEMPORALCOOKIES_H_
