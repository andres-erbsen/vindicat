#include "Util.h"
#include <string>

uint64_t randint64() {
	unsigned char b[sizeof(uint64_t)];
	randombytes(b, sizeof(uint64_t) );
	return *((uint64_t*) b);
}

std::string randomstring(unsigned int n) {
	unsigned char* bytes = new unsigned char[n];
	randombytes(bytes, n);
	return std::string (reinterpret_cast<char*>(bytes), n);
}
