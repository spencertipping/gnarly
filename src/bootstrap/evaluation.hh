#ifndef CORE_EVALUATION_HH
#define CORE_EVALUATION_HH

#include "bootstrap/core/definitions.hh"

BEGIN_NAMESPACE()

class $colon$colon$dollar;

class $colon$colon : public gnarly_value {
public:
  $colon$colon () {}
  virtual ~$colon$colon () {}

  shared_ptr<gnarly_value> operator() (const shared_ptr<gnarly_value> v) const {
    return *((*v) (this)) ($colon$colon$dollar::singleton);
  }
};

static const shared_ptr<const $colon$colon>
  $colon$colon::singleton (new $colon$colon ());

class $colon$colon$dollar : public gnarly_value {
public:
  $colon$colon$dollar () {}
  virtual ~$colon$colon$dollar () {}

  shared_ptr<gnarly_value> operator() (const shared_ptr<gnarly_value> v) const {
    return reinterpret_cast<gnarly_value*> (*v);
  }
};

static const shared_ptr<const $colon$colon$dollar>
  $colon$colon$dollar::singleton (new $colon$colon$dollar ());

END_NAMESPACE()

#endif
