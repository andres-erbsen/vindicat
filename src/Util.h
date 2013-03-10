#ifndef UTIL_H_
#define UTIL_H_

#include <string>
#include <algorithm>
#include <cstdint>
#include <sodium/randombytes.h>

std::string randomstring(unsigned int);
uint64_t randint64();

template<typename C, typename E>
static bool contains(C c,E e) {
	return std::find(c.begin(), c.end(), e) != c.end();
}

template <typename T>
std::string bytes(T n) {
	char* p = reinterpret_cast<char*> (&n);
	return std::string(p, sizeof(T) );
}

template <typename T>
typename std::underlying_type<T>::type enumval(const T& x) {
	return static_cast<typename std::underlying_type<T>::type>(x);
}

#endif // UTIL_H_
