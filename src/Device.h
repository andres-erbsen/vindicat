#ifndef DEVICE_H_
#define DEVICE_H_

class ForeignForwarding;

#include "vindicat.pb.h"

#include <memory>
#include <vector>
#include <string>
#include <unordered_map>

#include <stdint.h>

class Device : public std::enable_shared_from_this<Device> {
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

	void addForwarding(std::shared_ptr<ForeignForwarding>&&);
	std::weak_ptr<ForeignForwarding> getForwarding(uint64_t);
	void removeForwarding(uint64_t);

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

	std::unordered_map<uint64_t, std::shared_ptr<ForeignForwarding> > _forwardings;
};

#endif // DEVICE_H_

