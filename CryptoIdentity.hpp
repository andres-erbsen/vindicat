#include "lib/ed25519-donna/ed25519.h"
#include "vindicat.pb.h"

#include <string.h>

class CryptoIdentity {
public:
	bool pick_key(const DeviceInfo& recipient, EncKey& key);
	bool envelope(const std::string&, EncEnvelope&, std::string, const EncKey&);
	bool open(const EncEnvelope&, std::string&);	
	bool sign(const std::string&, Signature&);
private:
	std::string _secretkey_naclbox;
	ed25519_secret_key _secretkey_edsig;

	ed25519_public_key _verkey_edsig;
	std::string _enckey_naclbox;
};

