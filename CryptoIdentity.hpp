#include "vindicat.pb.h"

#include <ed25519.h>

#include <string.h>

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

