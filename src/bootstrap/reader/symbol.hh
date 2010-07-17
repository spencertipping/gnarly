#ifndef BOOTSTRAP_READER_SYMBOL_HH
#define BOOTSTRAP_READER_SYMBOL_HH

#include "bootstrap/core/definitions.hh"

BEGIN_NAMESPACE()

class reader_symbol : public gnarly_value {
  class reader_symbol_1 : public gnarly_value {
  public:
    reader_symbol_1 () {}
    virtual ~reader_symbol_1 () {}

    gnarly_ref operator() (const gnarly_ref a_function) const {
      return *a_function (symbol);
    }
  };

  const gnarly_ref symbol;
  const gnarly_ref aux (new reader_symbol_1 ());

public:
  reader_symbol (const gnarly_ref _symbol) : symbol (_symbol) {}
  virtual ~reader_symbol () {}

  gnarly_ref operator() (const gnarly_ref t_function) const {
    return aux;
  }
};

END_NAMESPACE()

#endif
