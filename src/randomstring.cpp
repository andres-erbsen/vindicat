#include "randomstring.h"

std::string randomstring(unsigned int n) {
	unsigned char* bytes = new unsigned char[n];
	randombytes(bytes, n);
	return std::string (reinterpret_cast<char*>(bytes), n);
}
