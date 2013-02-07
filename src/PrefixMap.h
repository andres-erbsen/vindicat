#ifndef PREFIXMAP_H_
#define PREFIXMAP_H_

#include <string>
#include <map>
#include <utility>

template <typename T>
class PrefixMap {
public:
	typedef typename std::map<std::string, T>::iterator iterator;
	typedef typename std::map<std::string, T>::const_iterator const_iterator;
	typedef typename std::pair<const std::string, T> value_type;
	const_iterator end() const;
	std::pair<iterator,bool> insert(const value_type&);

	const_iterator find(const std::string& prefix) const;
	// return the value for the only key that starts with `prefix`
	// return _end() on no match or multiple matches
	
	std::size_t erase(const std::string& prefix);
	// remove all matches, return how many were removed

private:
	std::map<std::string, T> _map;
};


template <typename T>
typename PrefixMap<T>::const_iterator PrefixMap<T>::end() const {
	return _map.end();
}

template <typename T>
std::pair<typename PrefixMap<T>::iterator,bool>
PrefixMap<T>::insert( const PrefixMap<T>::value_type& kvp ) {
	return _map.insert(kvp);
}
template <typename T>
typename PrefixMap<T>::const_iterator PrefixMap<T>::
find(const std::string& prefix) const {
	const_iterator it(_map.lower_bound(prefix));
	if (it == _map.end()) return it;
	if (it->first.substr(0, prefix.size()) != prefix) return _map.end();
	++it; // look at the next one; if it also matches we have an ambiguity
	if (it == _map.end()) return (--it); // last item matches => only match
	if (it->first.substr(0, prefix.size()) == prefix) return _map.end();
	return --it;
}

template <typename T>
size_t PrefixMap<T>::erase(const std::string& prefix) {
	size_t ret = 0;
	iterator it(_map.lower_bound(prefix));
	while (it != _map.end() && it->first.substr(0, prefix.size()) == prefix) {
		ret += _map.erase(it++);
	}
	return ret;
}


#endif // PREFIXMAP_H_
