#include <ctime>
#include <iostream>
#include "ControlInterface.h"
#include "Util.h"
#include "Link.h"
#include "Device.h"
#include "Constants.h"
#include "vindicat.pb.h"

const uint8_t VC_CONTROL_PROTOCOL = 0xDC;
const int VC_MAINTENANCE_INTERVAL = 10;

ControlInterface::ControlInterface(NetworkMap& nm, CryptoIdentity& ci)
  : _nm(nm)
  , _ci(ci)
{
  _w.set(0.00001,VC_MAINTENANCE_INTERVAL);
  _w.set(this);
  _w.start();
}

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
    LinkPromise lp;
    LinkInfo li;
    if ( ! lp.ParseFromArray(packet.data()+1, packet.size()-1)
      || ! lp.right_sig_algos_size() == 0
      || ! lp.right_sigs_size() == 0
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

    lp.add_right_sig_algos( enumval(SigAlgo::ED25519) );
    _ci.sign(lp.link_info_msg(), SigAlgo::ED25519, *lp.add_right_sigs());

    Subgraph sg;
    *sg.add_links() = lp;
    sg.add_devices()->CheckTypeAndMergeFrom(*dev->card()            );
    sg.add_devices()->CheckTypeAndMergeFrom(*_nm.our_device().card());

    std::string promise_packet(2, '\3');
    promise_packet[0] = VC_CONTROL_PROTOCOL;
    sg.AppendToString(&promise_packet);
    _receive_cb( std::string(from), std::move(promise_packet) );
  } else if (tag == 3) {
    std::cout << "Received link promise" << std::endl;
  }
}

void ControlInterface::operator()(ev::timer&, int) {
  LinkInfo info;
  info.set_status( LinkInfo::PUBLIC      );
  info.set_left  ( _nm.our_device().id() );
  info.set_time  ( std::time(NULL)       );
  LinkPromise proposal;
  proposal.add_left_sig_algos( enumval(SigAlgo::ED25519) );
  proposal.add_left_sigs();
  for (const auto& dev : _nm.neighbors()) {
    if ( _nm.link_to(dev->id())->promises().empty() ) { // negotiate
      info.set_right ( dev->id() );
      info.SerializeToString( proposal.mutable_link_info_msg() );
      _ci.sign( proposal.link_info_msg()
              , SigAlgo::ED25519
              , *proposal.mutable_left_sigs(0)
      );
      std::string proposal_packet(2, '\2');
      proposal_packet[0] = VC_CONTROL_PROTOCOL;
      proposal.AppendToString(&proposal_packet);
      _receive_cb( std::string(dev->id()), std::move(proposal_packet) );
    } else { // TODO: ping and check connectivity
    }
  }
}
