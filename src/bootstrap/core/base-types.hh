#ifndef CORE_TYPEDEFS_HH
#define CORE_TYPEDEFS_HH

#include <boost/shared_ptr.hpp>

#include "bootstrap/core/namespace-macros.hh"

BEGIN_NAMESPACE()

using boost::shared_ptr;

class gnarly_value;

typedef shared_ptr<gnarly_value> gnarly_ref;

class gnarly_value {
public:
  // No cv-modifier here. The call must be able to alter the closure state, which
  // is presumably encoded as instance data.
  virtual gnarly_ref operator() (const gnarly_ref v) = 0;
};

END_NAMESPACE()

#endif
