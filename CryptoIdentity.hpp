#ifndef CRYPTOIDENTITY_HPP
#define CRYPTOIDENTITY_HPP

#include "vindicat.pb.h"

#include <ed25519.h>

#include <string>
#include <list>
#include <array>
#include <tuple>

class CryptoIdentity {
public:
	CryptoIdentity();
	bool envelope(const std::string&, EncEnvelope&, std::string, const EncKey&) const;
	bool open(const EncEnvelope&, std::string&) const;
	bool sign(const std::string&, Signature&) const;

	void our_businesscard(DeviceBusinesscard&) const;
	const std::vector<std::string>& our_identifiers() const;
private:
	ed25519_secret_key _secretkey_edsig;
	ed25519_public_key _verkey_edsig;

	std::string _secretkey_naclbox;
	std::string _enckey_naclbox;

	std::vector<std::string> _our_identifiers;
};

bool pick_key(const DeviceInfo& recipient, EncKey& key);

bool verify(const std::string&, const Signature&, const SigKey& key);
bool verify(const std::string&, const Signature&, const DeviceInfo&);

bool verify(const DeviceBusinesscard&, DeviceInfo&); // results in $1
bool verify(const LinkPromise&, const DeviceInfo&
           , const DeviceInfo&, LinkInfo&); // results in $3

bool verify(const LinkProposal&, const DeviceInfo&, LinkInfo&);

#endif // CRYPTOIDENTITY_HPP
