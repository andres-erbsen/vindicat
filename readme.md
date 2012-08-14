# Authenticated link-state routing
To provide a secure and extendable foundation for decentralized networking.

## Representation of nodes
Nodes can be addressed by their public signing keys or hashes of these.
Using keys instead of hashes has the advantage that keys are needed for
validating signatures either way.
A key useable for authenticated public-key encryption is also needed.
Using hashes and retrieving signing and encryption keys on demand is
possible, may be good for performance, but seems to be a hassle.
Ideally, cryptosystems should be plugable -- if a weakness is found in
one of them, it won't brake the routing for thos who have dropped that system.

## The anatomy of a route
Each route consists of links. A link is represented by a statement by two
nodes, that packets can be routed from one to the other, with specified
properties like latency, bandwidth, or the fact that the link works in
both directions. For a link to be valid, it has to be signed by both
parties. Route is a list of links such that the previous links end node
is the same as the next links start node. A route is valid if all links
in it are valid.

## Implications
### Validity
If somebody has a valid route, one of the following must be true

- The route is current and all the links are still there and can be used
- The route is obsolete: some of the links are not operational any more
- Two or more nodes adjacent in the route have collaborated to forge a
link between them.

Note that anybody whose promise to route packets is not already inevitably
crucial for this route to work cannot tamper with it or create a believeable
forgery because he cannot forge digital signatures by any node in the route.

### Composeability
Having two routes, for example from `A` through `M` and `N` to `X` and another
route from `B` through `N` through `L` to `Y`,
a valid route from `M` to `L` through `N` can be constructed by any node by
concatenating links from `M` to `N` in the first route with links from `N` to
`L` from the second. Bidirectional links are even more useful here.

## Actual routing
If we know the route to the node we want to reach, how do we use it?
There are multiple ways to do this. A privacy-aware variant is described here.

### Tunnel requests and tunnels
A tunnel request is request by one node `A` to another node `B` to route all
packets with an arbitrarily picked identifier to a third node `C`. This is
encrypted using a public-key authenticated encryption scheme by `A` to be
readable by `B`. A tunnel request may contain another packet.
If `B` accepts `A`'s tunnel through him, he should forward the additional
packet to to `C`. If `B` refuses to route `A`'s packets
(maybe because he does not know a route to `C`), he should send a
authenticated statement of that back to `A` and is encouraged to include
the reason. This way, a route can be encoded as
a list of encrypted tunnel requests, negotiating packets to be forwarded
without giving away the the source or destination to arbitrary eavesdroppers.
The first packet of a session can be piggypacked on the tunnel request.
Adversaries who can inject packets cannot make `A` drop its tunnels if
refusals to route need authentication.
All following packets only need to contain the tunnel identifier, which saves
space compared to including the full route every time.

## Finding the routes
Any routing scheme, authenticated or not, can be used to find routes for
authenticated link-state routing. Imitating the behaivour of the IP traceroute,
instead of sending packets using the non-authenticated routing scheme, send
a request to all nodes to send back the link between them and the next hop.
However, this does not make the route-finding scheme authenticated, if it could
have been brought to a halt with bogus messages, it still can. The improvement
is, that routes can be checked for validity.

Any node should share links known to him with other nodes on demand, possibly
with exceptions for privacy. Links known to be dead should not be shared.

### Dealing with dead links
There may be no use for this, but still. A link can be marked dead (no more
useable) by either pary without the others consent -- he could drop just the
packets either way. Therefore, a link with a special property meaning "dead"
signed by one of the parties this link connects is a good representation for
a dead link.
