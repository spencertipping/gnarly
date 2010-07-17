#ifndef BOOTSTRAP_READER_CONS_HH
#define BOOTSTRAP_READER_CONS_HH

#include "bootstrap/core/definitions.hh"

BEGIN_NAMESPACE()

class reader_cons : public gnarly_value {
  class reader_cons_1 : public gnarly_value {
    const gnarly_ref t_function;

  public:
    reader_cons_1 (const gnarly_ref _t_function) : t_function (_t_function) {}
    virtual ~reader_cons_1 () {}

    gnarly_ref operator() (const gnarly_ref a_function) const {
      return *(*t_function (head)) (tail);
    }
  };

  const gnarly_ref head;
  const gnarly_ref tail;

public:
  reader_cons (const gnarly_ref _head, const gnarly_ref _tail) :
    head (_head), tail (_tail) {}
  virtual ~reader_cons () {}

  gnarly_ref operator() (const gnarly_ref t_function) const {
    return gnarly_ref (new reader_cons_1 (t_function));
  }
};

END_NAMESPACE()

#endif
