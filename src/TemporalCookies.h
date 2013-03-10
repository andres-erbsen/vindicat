#ifndef TEMPORALCOOKIES_H_
#define TEMPORALCOOKIES_H_

#include <sodium/crypto_secretbox.h>
#include <ev++.h>
#include <string>
#include <unordered_set>

class TemporalCookies {
public:
  TemporalCookies(float refresh_interval=60.0);
  TemporalCookies(const TemporalCookies&) = delete;
  TemporalCookies(TemporalCookies&&) = default;
  const TemporalCookies& operator= (const TemporalCookies&) = delete;

  std::string cookie(const std::string&) const;
  bool open(const std::string&, std::string&) const;

  void blacklist(const std::string&);
  bool allowed(const std::string&) const;

  void operator() (ev::timer&, int);
private:
  ev::timer _w;

  unsigned char _minutekey[crypto_secretbox_KEYBYTES];
  unsigned char _lastminutekey[crypto_secretbox_KEYBYTES];

  std::unordered_set<std::string> _minuteblacklist;
  std::unordered_set<std::string> _lastminuteblacklist;
};


#endif // TEMPORALCOOKIES_H_
