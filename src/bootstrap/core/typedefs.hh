#ifndef CORE_TYPEDEFS_HH
#define CORE_TYPEDEFS_HH

#include "bootstrap/core/namespace-macros.hh"

BEGIN_NAMESPACE()

class gnarly_value {
public:
  virtual gnarly_value  () {}
  virtual ~gnarly_value () {}

  // No cv-modifier here. The call must be able to alter the closure state.
  virtual shared_ptr<gnarly_value> operator() (const shared_ptr<gnarly_value> v) = 0;
};

END_NAMESPACE()

#endif
