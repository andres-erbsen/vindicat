#include "LinkNegotiator.hpp"
#include <cassert>
#include <ctime>

LinkNegotiator::LinkNegotiator(NetworkMap& nm, CryptoIdentity& ci)
	: _nm(nm), _ci(ci) {}

void LinkNegotiator::beacon(TransportSocket* trs) {
	LinkInfo info;
	info.set_status(  LinkInfo::PUBLIC         );
	info.set_left(    _ci.our_identifiers()[0] );
	info.set_right(   _nm.dev_ids(trs)[0]      );
	info.set_time(    std::time(NULL)          );

	std::string info_msg;
	info.SerializeToString(&info_msg);

	Signature sig;
	_ci.sign(info_msg, sig);

	LinkProposal proposal;
	proposal.set_link_info_msg(info_msg);
	*proposal.add_left_sigs() = sig;
	
	std::string proposal_packet = "\3";
	proposal.AppendToString(&proposal_packet);

	trs->send(proposal_packet);
}
