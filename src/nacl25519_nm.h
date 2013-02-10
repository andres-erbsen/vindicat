#ifndef NACL25519_NM_H_
#define NACL25519_NM_H_

#include <crypto_box.h>
#include <string>

class nacl25519_nm {
public:
	nacl25519_nm() = delete;
	nacl25519_nm(const nacl25519_nm&) = delete;
	const nacl25519_nm& operator= (const nacl25519_nm&) = delete;

	nacl25519_nm(const std::string& pk); // generates secret key
	nacl25519_nm(const std::string& pk, const std::string& sk);

	void pk(const std::string& pk);
	std::string our_pk();

	std::string encrypt(const std::string&, const std::string&) const;
	bool decrypt(const std::string&, const std::string&, std::string&) const;

private:
	unsigned char _k[crypto_box_BEFORENMBYTES];
	unsigned char _pk[crypto_box_PUBLICKEYBYTES];
	unsigned char _sk[crypto_box_SECRETKEYBYTES];
	unsigned char _our_pk[crypto_box_PUBLICKEYBYTES];
};



#endif // NACL25519_NM_H_
