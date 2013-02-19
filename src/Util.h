
#ifndef UTIL_H_
#define UTIL_H_

template<typename C, typename E>
static bool contains(C c,E e) {
	return std::find(c.begin(), c.end(), e) != c.end();
}

#endif // UTIL_H_
