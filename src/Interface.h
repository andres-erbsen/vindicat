#ifndef INTERFACE_H_
#define INTERFACE_H_

#include <cstdint>
#include <string>
#include <functional>

typedef std::function< void(std::string&&, std::string&&) > interface_callback;

class Interface {
public:
	Interface() = default;
	Interface(const Interface&) = delete;
	const Interface& operator= (const Interface&) = delete;
	virtual ~Interface() = default;

	void onPacket(interface_callback);
	virtual void send(const std::string&, uint8_t, const std::string&) = 0;

protected:
	interface_callback _receive_cb;
};

#endif // INTERFACE_H_
