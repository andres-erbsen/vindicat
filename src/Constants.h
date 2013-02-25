#ifndef CONSTANTS_H_
#define CONSTANTS_H_

#include <cstdint>
#include <unordered_map>

// EdDSA signatures from https://github.com/floodyberry/ed25519-donna or libsodium
enum class SigAlgo : std::uint8_t { ED25519 = 1 };
// djb nacl crypto_box http://nacl.cr.yp.to/box.html
enum class PkencAlgo : std::uint8_t { CURVE25519XSALSA20POLY1305 = 1 };

namespace std {
template <> struct hash<SigAlgo> {
	size_t operator()(const SigAlgo& x) const {
		return hash<uint8_t>()( static_cast<uint8_t>(x) );
	}
};

template <> struct hash<PkencAlgo> {
	size_t operator()(const PkencAlgo& x) const {
		return hash<uint8_t>()( static_cast<uint8_t>(x) );
	}
};
}


#endif // CONSTANTS_H_
