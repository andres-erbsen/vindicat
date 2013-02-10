#ifndef DEVICE_H_
#define DEVICE_H_

class Forwarding;

#include "vindicat.pb.h"

#include <memory>
#include <vector>
#include <string>
#include <unordered_map>

#include <stdint.h>

class Device {
public:
	Device() = default;
	Device(const Device&) = delete;
	Device(Device&&) = default;
	const Device& operator= (const Device&) = delete;

	const std::vector<std::string>& ids() const;
	const std::string& id() const { return ids()[0]; }

	uint64_t mtime() const;
	std::vector< std::weak_ptr<DeviceBusinesscard> > cards() const;

	bool verifySignature(const std::string& message, const std::string& sig, SigAlgo algo) const;

	// Pick the best
	PkencAlgo enc_algo() const;
	std::string enc_key() const;
	SigAlgo sig_algo() const;

	void addForwarding(std::shared_ptr<Forwarding>&&);
	std::weak_ptr<Forwarding> getForwarding(uint32_t);
	void removeForwarding(uint32_t);

	// Deserialization
	bool parseFrom(std::shared_ptr<DeviceBusinesscard>&& card_p);

	static std::shared_ptr<Device> merge(Device&&, Device&&);

	void clear();

private:
	std::vector<std::string> _ids;
	std::vector<std::string> _sig_keys;
	std::vector<SigAlgo> _sig_algos;

	PkencAlgo _enc_algo;
	std::string _enc_key;

	uint64_t _mtime;
	std::vector<std::shared_ptr<DeviceBusinesscard> > _cards;

	std::unordered_map<uint32_t, std::shared_ptr<Forwarding> > _forwardings;
};

#endif // DEVICE_H_

