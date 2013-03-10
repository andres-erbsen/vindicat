#include "ControlInterface.h"
#include "Constants.h"
#include "vindicat.pb.h"

const uint8_t VC_CONTROL_PROTOCOL = 0xDC;

bool ControlInterface::match( const std::string&
                            , uint8_t protocol
                            , const std::string&) {
  return protocol == VC_CONTROL_PROTOCOL;
}

bool parse_all_or_nothing(const std::string& ss, LinkInfo& replica) {
  LinkInfo original;
  original.ParseFromString(ss);
  replica.Clear();
  replica.set_status(original.status());
  replica.set_left  (original.left()  );
  replica.set_right (original.right() );
  if ( original.has_time() ) {
    replica.set_time(original.time()  );
  }
  if ( replica.SerializeAsString() == ss ) {
    return 1;
  } else {
    replica.Clear();
    return 0;
  }
}

void ControlInterface::send( const std::string& from
                           , uint8_t protocol
                           , const std::string& packet ) {
  if (packet.size() < 1) return;
  uint8_t tag = packet[0];
  if (0) {
  } else if (tag == 2) {
    LinkProposal lp;
    LinkInfo li;
    if ( ! lp.ParseFromArray(packet.data()+1, packet.size()-1)
      || ! parse_all_or_nothing(lp.link_info_msg(), li) ) {
      return;
    }
    auto dev = _nm.device( li.left() );
    bool has_valid_signature = 0;
    int n = std::min(lp.left_sigs_size(), lp.left_sig_algos_size());
    for (int i=0; i<n; ++i) {
      const SigAlgo algo = static_cast<SigAlgo>( lp.left_sig_algos(i) );
      if ( dev->verifySignature(lp.link_info_msg(), lp.left_sigs(i), algo) ) {
        has_valid_signature = 1;
        break;
      }
    }
    if ( ! has_valid_signature || ! dev->link_not_exaggerated(li) ) return;

    bool got_;
    Subgraph sg; // will contain descriptions of the new link and its endpoints
    LinkPromise* lm = sg.add_links();
    got_ = lm->ParseFromArray(packet.data()+1, packet.size()-1);
    assert(got_);
    got_ = _ci.sign(lm->link_info_msg(),SigAlgo::ED25519,*lm->add_right_sigs());
    assert(got_);
    sg.add_devices()->CheckTypeAndMergeFrom(*dev->card()            );
    sg.add_devices()->CheckTypeAndMergeFrom(*_nm.our_device().card());

    std::string promise_packet(2, '\3');
    promise_packet[0] = VC_CONTROL_PROTOCOL;
    sg.AppendToString(&promise_packet);
    _receive_cb( std::string(from), std::move(promise_packet) );
  }
}
