#ifndef CONNECTIONPOOL_H_
#define CONNECTIONPOOL_H_

#include "Connection.h"
#include "PrefixMap.h"
#include <memory>

typedef PrefixMap< std::shared_ptr<Connection> > ConnectionPool;
// maps device identifiers to our connections to them

#endif // CONNECTIONPOOL_H_
