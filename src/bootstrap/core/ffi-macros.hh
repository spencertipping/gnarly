#ifndef CORE_FFI_MACROS_HH
#define CORE_FFI_MACROS_HH

#include "bootstrap/core/typedefs.hh"

#define gnarly_define(name, parameter) \
  static value* name (const value *parameter)

#define gnarly_forward_define(name, parameter) \
  gnarly_define(name, parameter);

#define gnarly_call(fn, v) \
  ((* static_cast<value_fn*>(fn)) (v))

#endif
