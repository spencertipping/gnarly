#ifndef CORE_EVALUATION_HH
#define CORE_EVALUATION_HH

#include "core/definitions.hh"

BEGIN_NAMESPACE()

gnarly_forward_define(eval)
gnarly_forward_define(eval_symbol)

gnarly_define(eval, form) {
  return gnarly_call(gnarly_call(form, &eval), &eval_symbol);
}

gnarly_define(eval_symbol, symbol) {
  return *symbol;
}

END_NAMESPACE()

#endif
