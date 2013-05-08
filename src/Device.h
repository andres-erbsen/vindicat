#ifndef DEVICE_H_
#define DEVICE_H_

class ForeignForwarding;

#include "Constants.h"
#include "vindicat.pb.h"

#include <memory>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <stdint.h>

class Device : public std::enable_shared_from_this<Device> {
public:
  Device() = default;
  Device(const Device&) = delete;
  const Device& operator= (const Device&) = delete;
  Device(Device&&) = default;
  Device& operator= (Device&&) = default;

  uint64_t mtime() const;
  const std::string& id() const;
  const std::vector<std::string>& ids() const;
  const std::string& enc_key() const;
  std::shared_ptr<DeviceBusinesscard> card() const;

  // Pick the best
  SigAlgo sig_algo() const;
  PkencAlgo enc_algo() const;

  bool link_not_exaggerated(const LinkInfo&);

  bool verifySignature(const std::string& message, const std::string& sig, SigAlgo algo) const;
  bool open(const std::string&, std::string, const std::string&, PkencAlgo, std::string&) const;

  // Store Forwarding objects
  void addForwarding(std::shared_ptr<ForeignForwarding>&&);
  std::shared_ptr<ForeignForwarding> getForwarding(uint64_t);
  void removeForwarding(uint64_t);

  // Deserialization
  bool parseFrom(std::shared_ptr<DeviceBusinesscard> card_p);
  bool parseFrom(const std::string&);

  void merge(Device&&);

  // Capabilities
  std::unordered_set<std::string>& capabilities();
  const std::unordered_set<std::string>& capabilities() const;

private:
  void clear();

  std::unordered_map<SigAlgo, size_t> _sig; // index of vectors:
  std::vector<std::string> _ids;
  std::vector<std::string> _sig_keys;

  std::unordered_map<PkencAlgo, std::string > _enc; // key
  std::unordered_map<uint64_t, std::shared_ptr<ForeignForwarding> > _forwardings;
  uint64_t _mtime;
  std::shared_ptr<DeviceBusinesscard> _card;

  std::unordered_set<std::string> _capabilities;
};

#endif // DEVICE_H_

