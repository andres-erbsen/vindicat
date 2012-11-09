#include "vindicat.pb.h"

#include <ed25519.h>

#include <string>
#include <list>
#include <array>
#include <tuple>

class CryptoIdentity {
public:
	CryptoIdentity();
	bool pick_key(const DeviceInfo& recipient, EncKey& key);
	bool envelope(const std::string&, EncEnvelope&, std::string, const EncKey&);
	bool open(const EncEnvelope&, std::string&);	
	bool sign(const std::string&, Signature&);
private:
	ed25519_secret_key _secretkey_edsig;
	ed25519_public_key _verkey_edsig;

	std::string _secretkey_naclbox;
	std::string _enckey_naclbox;
};

bool verify(const std::string&, const Signature&, const SigKey& key);
bool verify(const std::string&, const Signature&, const DeviceInfo&);

bool verify(const DeviceBusinesscard&, DeviceInfo&); // results in $1
bool verify(const LinkPromise&, const DeviceInfo&
           , const DeviceInfo&, LinkInfo&); // results in $3
