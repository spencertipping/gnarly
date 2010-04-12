#!/usr/bin/perl

use File::Temp  'tempfile';
use Carp        'carp';
use Digest::SHA 'sha256_base64';

$|++;

my %data;
my %transient;
my %externalized_functions;
my @data_types;
my @script_args;

sub meta::define_form {
  my ($namespace, $delegate) = @_;
  push @data_types, $namespace;
  *{"meta::${namespace}::implementation"} = $delegate;
  *{"meta::$namespace"} = sub {
    my ($name, $value) = @_;
    chomp $value;
    $data{"${namespace}::$name"} = $value;
    $delegate->($name, $value);
  };
}

meta::define_form 'meta', sub {
  my ($name, $value) = @_;
  eval $value;
  carp $@ if $@;
};

meta::meta('datatypes::attribute', <<'__PYmyPgg2sMijXksysuPrJU0k2SAh5zzFu2hvd7hDcoU');
meta::define_form 'attribute', sub {
  my ($name, undef) = @_;
  $externalized_functions{$name} = "attribute::$name";
  *{$name} = sub {
    associate("attribute::$name", $_[1] || join('', <STDIN>)) if @_ > 0 && $_[0] eq '=';
    retrieve("attribute::$name");
  };
};
__PYmyPgg2sMijXksysuPrJU0k2SAh5zzFu2hvd7hDcoU

meta::meta('datatypes::bootstrap', <<'__guYWiOv4zBmdrlI3k3sW7f/q/xsX38Xvzz0dwwLCIRM');
meta::define_form 'bootstrap', sub {};
__guYWiOv4zBmdrlI3k3sW7f/q/xsX38Xvzz0dwwLCIRM

meta::meta('datatypes::c', <<'__Ji6/U0j2lrrl1sb57Mpro3gDV/39Xhnx3oLs8zAhewk');
meta::define_form 'c', sub {
  my ($name, undef) = @_;
  $externalized_functions{$name} = "c::$name";
  *{$name} = sub {
    return edit("c::$name") if $_[0] eq 'edit';

    write_c_source($name)   if grep /^$_[0]$/, qw/run compile unlit/;
    compile_c_source($name) if grep /^$_[0]$/, qw/run compile/;
    run_c($name)            if grep /^$_[0]$/, qw/run/;
  };
};
__Ji6/U0j2lrrl1sb57Mpro3gDV/39Xhnx3oLs8zAhewk

meta::meta('datatypes::code', <<'__OEWZ93LGHT8yOiO/06UyIrAiKTXnAbI7tYl7+93uAT0');
meta::define_form 'code', sub {
  my ($name, $value) = @_;
  $externalized_functions{$name} = "code::$name";
  *{$name} = sub {
    edit("code::$name");
  };
};
__OEWZ93LGHT8yOiO/06UyIrAiKTXnAbI7tYl7+93uAT0

meta::meta('datatypes::configuration', <<'__my6exSOOfpxIJLIUfmSVVMUaquAgbayoNBj9guBwbYU');
meta::define_form 'configuration', sub {
  my ($name, $value) = @_;
  $externalized_functions{$name} = "configuration::$name";
  *{$name} = eval "sub {\n$value\n}";
  carp $@ if $@;
};
__my6exSOOfpxIJLIUfmSVVMUaquAgbayoNBj9guBwbYU

meta::meta('datatypes::data', <<'__j7lFraXGRfKk8ymj2mDJhNbCQMk9FSciN1hdDhzM99U');
meta::define_form 'data', sub {
  my ($name, undef) = @_;
  $externalized_functions{$name} = "data::$name";
  *{$name} = sub {
    associate("data::$name", $_[1] || join('', <STDIN>)) if @_ > 0 && $_[0] eq '=';
    retrieve("data::$name");
  };
};
__j7lFraXGRfKk8ymj2mDJhNbCQMk9FSciN1hdDhzM99U

meta::meta('datatypes::function', <<'__XSIHGGHv0Sh0JBj9KIrP/OzuuB2epyvn9pgtZyWE6t0');
meta::define_form 'function', sub {
  my ($name, $value) = @_;
  $externalized_functions{$name} = "function::$name";
  *{$name} = eval "sub {\n$value\n}";
  carp $@ if $@;
};
__XSIHGGHv0Sh0JBj9KIrP/OzuuB2epyvn9pgtZyWE6t0

meta::meta('datatypes::internal_function', <<'__heBxmlI7O84FgR+9+ULeiCTWJ4hqd079Z02rZnl9Ong');
meta::define_form 'internal_function', sub {
  my ($name, $value) = @_;
  *{$name} = eval "sub {\n$value\n}";
  carp $@ if $@;
};
__heBxmlI7O84FgR+9+ULeiCTWJ4hqd079Z02rZnl9Ong

meta::meta('datatypes::java', <<'__YwJ9aNyrqtevjVtmJgDlP6IdDiWzmLXRoqiofL/XKws');
meta::define_form 'java', sub {
  my ($name, undef) = @_;
  $externalized_functions{$name} = "java::$name";
  *{$name} = sub {
    write_java_source($name)   if grep /^$_[0]$/, qw/run compile unlit/;
    compile_java_source($name) if grep /^$_[0]$/, qw/run compile/;
    run_java($name)            if grep /^$_[0]$/, qw/run/;
  };
};
__YwJ9aNyrqtevjVtmJgDlP6IdDiWzmLXRoqiofL/XKws

meta::meta('datatypes::library', <<'__3RHc2q2OKjeHL1QRq6jhHHCeSrNLDPWTwSax7MclXRE');
meta::define_form 'library', sub {
  eval $_[1];
  warn $@ if $@;
};
__3RHc2q2OKjeHL1QRq6jhHHCeSrNLDPWTwSax7MclXRE

meta::meta('datatypes::list-type', <<'__OKczvJ+6wi8VPNFcZ9ohlXjw+ychodWCfcELdli9p+w');
meta::define_form '_list_type', sub {
  my ($outer_name, $outer_value) = @_;
  $externalized_functions{$outer_name} = "_list_type::$outer_name";
  
  *{$outer_name} = sub {
    associate("${outer_value}::$_", '') for @_;
  };

  meta::define_form $outer_value, sub {
    my ($name, $value) = @_;
    $externalized_functions{$name} = "${outer_value}::$name";
    *{$name} = sub {
      my ($command, @xs) = @_;
      my $xs = join "\n", @xs;
      return grep length, split /\n/, retrieve("${outer_value}::$name")               if $command eq 'items';
      associate("${outer_value}::$name", retrieve("${outer_value}::$name") . "\n$xs") if $command eq 'add' || $command eq '<<';
      edit("${outer_value}::$name")                                                   if $command eq 'edit';
      return retrieve("${outer_value}::$name");
    };
  };
};
__OKczvJ+6wi8VPNFcZ9ohlXjw+ychodWCfcELdli9p+w

meta::meta('datatypes::note', <<'__TGOjJwmj+QJp1giUQqg2bEaQe8RvqnrFEqyZhIpSC34');
meta::define_form 'note', sub {
  my ($name, undef) = @_;
  $externalized_functions{$name} = "note::$name";
  *{$name} = sub {edit("note::$name")};
};
__TGOjJwmj+QJp1giUQqg2bEaQe8RvqnrFEqyZhIpSC34

meta::meta('datatypes::ocaml', <<'__u5BPhNcm6QdRYIJltEUQ7misAeSFsh4+hB2GCg3BzKo');
meta::define_form 'ocaml', sub {
  my ($name, undef) = @_;
  $externalized_functions{$name} = "ocaml::$name";
  *{$name} = sub {
    return edit("ocaml::$name") if $_[0] eq 'edit';

    write_ocaml_source($name)   if grep /^$_[0]$/, qw/run compile unlit/;
    compile_ocaml_source($name) if grep /^$_[0]$/, qw/run compile/;
    run_ocaml($name)            if grep /^$_[0]$/, qw/run/;
  };
};
__u5BPhNcm6QdRYIJltEUQ7misAeSFsh4+hB2GCg3BzKo

meta::meta('datatypes::vim-highlighter', <<'__vsGBLVDC3S+pX/k/zl5CgXeAQz2QjpBkLgx0CJ4vcn0');
meta::define_form 'vim_highlighter', \&meta::bootstrap::implementation;
__vsGBLVDC3S+pX/k/zl5CgXeAQz2QjpBkLgx0CJ4vcn0

meta::meta('internal::runtime', <<'__Nd6Dp1A6nL7yAGeoRfeZETeaW8vnPN8HI9Diqo66vDA');
meta::define_form 'internal', \&meta::meta::implementation;
__Nd6Dp1A6nL7yAGeoRfeZETeaW8vnPN8HI9Diqo66vDA

meta::_list_type('issue', <<'__SlAoRtBw4giLcCWr6AYpgwvwPXq1Yk1ekfMyvJ0EnT8');
issue
__SlAoRtBw4giLcCWr6AYpgwvwPXq1Yk1ekfMyvJ0EnT8

meta::_list_type('list', <<'__ozA5XMClOtEgdzZUav/0c1lAk3Vku/dc4e2tQHgNkTk');
list
__ozA5XMClOtEgdzZUav/0c1lAk3Vku/dc4e2tQHgNkTk

meta::attribute('module-api-version', <<'__0P9ZdLaqUs9WK+pZIYQMAyqGCpGjUS9/6Pdo9rvgBfY');
1.0
__0P9ZdLaqUs9WK+pZIYQMAyqGCpGjUS9/6Pdo9rvgBfY

meta::attribute('module-name', <<'__Fs7fgK3gHGK90a6THQSSMwwLYr8pTAjAlc4vqyGpKY0');
js
__Fs7fgK3gHGK90a6THQSSMwwLYr8pTAjAlc4vqyGpKY0

meta::attribute('module-revision', <<'__X+zrZv/IbzjZUnhsbWlsecLbwjndTpG0ZynXOif7V+k');
0
__X+zrZv/IbzjZUnhsbWlsecLbwjndTpG0ZynXOif7V+k

meta::bootstrap('initialization', <<'__plktoDCjGQioE48vwfrH0xL3ulcYnTWp+fUvaFwRnnc');
#!/usr/bin/perl

use File::Temp  'tempfile';
use Carp        'carp';
use Digest::SHA 'sha256_base64';

$|++;

my %data;
my %transient;
my %externalized_functions;
my @data_types;
my @script_args;

sub meta::define_form {
  my ($namespace, $delegate) = @_;
  push @data_types, $namespace;
  *{"meta::${namespace}::implementation"} = $delegate;
  *{"meta::$namespace"} = sub {
    my ($name, $value) = @_;
    chomp $value;
    $data{"${namespace}::$name"} = $value;
    $delegate->($name, $value);
  };
}

meta::define_form 'meta', sub {
  my ($name, $value) = @_;
  eval $value;
  carp $@ if $@;
};

__plktoDCjGQioE48vwfrH0xL3ulcYnTWp+fUvaFwRnnc

meta::c('interpreter', <<'__GMfjWltod3iCFAG39/LVGQyNY4vpDTStCqE56TEeQwc');
C++ implementation of Gnarly | Spencer Tipping <spencer@spencertipping.com>
Licensed under the terms of the MIT source code license

Interpreter logic.

  > type value is [cons h t] or nil or [symbol n] or [expander x e] or [ref x] or [composition f g] or [application f x]
  > let m be mutable integer -> value

  > eval cons h t = call (eval h) t
  > eval symbol n = m n
  > eval ref x    = x
  > eval _        = _

  > call (expander x (cons h t)) y = cons (call (expander x h) y) (call (expander x t) y)
  > call (expander x x) y          = y
  > call (expander x _) y          = _
  > call (composition f g) x       = call f (call g x)
  > call nil x                     = eval x

  Builtin functions.

  > call n 'bind                = application (eval n) bind
  > call (application n bind) x = set m n to (eval x) in m n
  > call n 'unbind              = delete m n in n
  > call 'ref x                 = ref (eval x)
  > call 'beta x                = application beta x
  > call (application beta x) y = expander x y

  > call 'o' f                  = application o' (eval f)
  > call (application o' f) g   = composition f (eval g)

  > call 'cons x                = application cons (eval x)
  > call (application cons x) y = cons x (eval y)
  > call (cons x y) 'h          = x
  > call (cons x y) 't          = y

  Runtime.

  > output (eval input)

CPS conversion.
Ideally we want the interpreter to use only tail calls so that GOTO can be used to handle all of the branching. The simplest way to do this is to use CPS
conversion on the above program. Note, however, that this requires some encoding of pattern-matching. For now I'm going to gloss over it and save the encoding
of this decisional for the C++ implementation. Also note that the type constructors don't get CPS-converted.

  > eval (cons h t) k = eval h (x -> (call x t k)))
  > eval (symbol n) k = k (m n)
  > eval (ref x) k    = k x
  > eval _ k          = k _

  > call (expander x (cons h t)) y k = call (expander x h) y (c1 -> call (expander x t) y (c2 -> k (cons c1 c2)))
  > call (expander x x) y k          = k y
  > call (expander x _) y k          = k _
  > call (composition f g) x k       = call g x (c1 -> call f c1 k)
  > call nil x k                     = eval x k

  > call n 'bind k                   = eval n (c1 -> k (application c1 bind))
  > call (application n bind) x k    = eval x (c1 -> set m n to c1 (c2 -> k c1))
  > call n 'unbind k                 = delete m n (c1 -> k n)
  > call 'ref x k                    = eval x (c1 -> k (ref c1))
  > call 'beta x k                   = k (application beta x)
  > call (application beta x) y k    = k (expander x y)

  > call 'o' f k                     = eval f (c1 -> k (application o' c1))
  > call (application o' f) g k      = eval g (c1 -> k (composition f c1))

  > call 'cons x k                   = eval x (c1 -> k (application cons c1))
  > call (application cons x) y k    = eval y (c1 -> k (cons x c1))
  > call (cons x y) 'h k             = k x
  > call (cons x y) 't k             = k y

  > eval input output

Encoding in imperative form.
Probably the simplest way to encode the CPS-expanded forms above is to use a value stack with computed GOTO as the control flow mechanism. At this point,
control flow is forward-only (as would always be the case with a fully CPS-converted program).

Defining an expansion of one of the above CPS forms would involve popping the requisite parameters off of the stack, pushing parameters for the continuations,
and then invoking GOTO on those continuations with the new parameters in place. The only difficulty that arises is encoding the anonymous functions, which close
over other parameter values. I'm handling these by pushing the closure parameters manually and popping them as if they were explicit.

Types of arguments are encoded using the least-significant bits. (I'm relying on the fact that all pointers are to 64-bit-aligned integers.) The encoding is as
follows:

  | 000 cons cell (nil if the entire value is 0)
  | 001 symbol
  | 010 ref
  | 100 expander
  | 101 composition
  | 110 application
  | 111 continuation

  typedef unsigned long long value;

  #define table(name, size) static value name[size]; \
                            static value *name##p = name;

  #define stack_access(name) static inline value *name##c (value v) \
                               {*(++name##p) = v; return name##p;}

  #define binary_access(name) static inline value *name##c (value v1, value v2) \
                                {*(++name##p) = v1; *(++name##p) = v2; return name##p;}

  #define s_table(name, size) table(name, size) stack_access(name)
  #define b_table(name, size) table(name, size) binary_access(name)

  b_table(c, 10000000)
  b_table(a, 1000000)
  b_table(o, 1000000)
  b_table(e, 1000000)

  s_table(s, 1000000)
  s_table(m, 1000000)

  #define pop     (*(sp--))
  #define push(x) (*(++sp) = (x))
  #define peek    (*sp)
  #define peek1   (*(sp - 1))
  #define peek2   (*(sp - 2))
  #define poke(x) (*sp = (x))

  #define h(x)      (*((value*) (x)))
  #define t(x)      (*((value*) (x) + 1))
  #define ccp(x, y) ((value) cc(x, y))
  #define ecp(x, y) ((value) ec(x, y) | 0x4ull)
  #define ocp(x, y) ((value) oc(x, y) | 0x5ull)
  #define acp(x, y) ((value) ac(x, y) | 0x6ull)

  #define symbol(x)  (((x) << 3) | 0x1ull)
  #define bind(x, y) (m[(x) >> 3] = (y))
  #define unbind(x)  (m[(x) >> 3] = 0)

  #define unpack(x)       ((x) & ~0x7ull)

  #define continuation(x) ((value) (x) | 0x7ull)
  #define fn(x)           ((void*) unpack(x))

  static const value s_ref    = symbol(0);
  static const value s_beta   = symbol(1);
  static const value s_o      = symbol(2);
  static const value s_cons   = symbol(3);

  static const value s_h      = symbol(4);
  static const value s_t      = symbol(5);
  static const value s_bind   = symbol(6);
  static const value s_unbind = symbol(7);

  int main () {
    const void *eval_targets[3]   = {&&eval_cons, &&eval_symbol, &&eval_ref};
    const void *call_targets[8]   = {&&call_cons_or_nil, &&call_symbol, &&call_ref, &&call_undefined,
                                     &&call_application, &&call_composition, &&call_expander, &&call_continuation};
    const void *call_s_targets[3] = {&&call_s_ref, &&call_s_o, &&call_s_cons};

    value eval_x, eval_k, eval_c1_x, eval_c1_t, eval_c1_k;
    value call_x, call_y, call_k, call_x_type, call_x_h, call_x_t;

    eval:
      eval_x = peek1;
      if (eval_x & 0x4ull) goto *fn(pop);           // Inlined
      else                 goto *eval_targets[eval_x & 0x3ull];

      eval_cons:
        eval_k = pop;
        eval_x = peek;    // Linked (pop)
        poke(eval_k);     // Linked (push)
        push(t(eval_x));
        push(h(eval_x));
        push(continuation(&&eval_cons_c1));
        goto eval;

      eval_symbol:
        eval_k = pop;
        eval_x = pop;
        push(m[unpack(eval_x) >> 3]);
        goto *fn(eval_k);

      eval_ref:
        eval_k = pop;
        poke(*((value*) unpack(peek)));
        goto *fn(eval_k);

      eval_cons_c1:       // Fragile (implicit 'goto call' -- see end of block)
        eval_c1_x = pop;
        eval_c1_t = pop;
        eval_c1_k = pop;
        push(eval_c1_x);
        push(eval_c1_t);
        push(eval_c1_k);
        // goto call

    call:
      call_x_type = peek2 & 0x7ull;
      goto *call_targets[call_x_type];

      call_cons_or_nil:
        call_k = pop;
        call_y = pop;
        call_x = pop;

        if (call_x) {
          // cons
          if      (call_y == s_h) push(h(call_x));
          else if (call_y == s_t) push(h(call_y));
          else goto call_cons_on_invalid;
          goto *fn(call_k);
        } else {
          // nil
          push(call_y);
          push(call_k);
          goto eval;
        }

      call_symbol:
        call_k = pop;
        call_y = pop;
        call_x = pop;

        if (call_y == s_bind) {
          push(call_k);
          push(call_x);
          push(continuation(&&call_symbol_c1));
          goto eval;
        } else if (call_y == s_unbind) unbind(call_x), push(call_x);
        else goto call_symbol_on_invalid;

        goto *fn(call_k);

        call_symbol_c1:
          call_x = pop;
          call_k = pop;
          push(acp(call_x, s_bind));
          goto *fn(call_k);

      call_ref:       cerr << "Attempting to call a ref" << endl;              return 1;
      call_undefined: cerr << "Attempting to call an undefined value" << endl; return 1;

      call_application:
        call_k   = pop;
        call_y   = pop;
        call_x   = pop;
        call_x_h = h(call_x);
        call_x_t = t(call_x);

        if (call_x_h & 0x7ull == 0x7ull) {
          // Case 1. x_h is a continuation of some sort, so pass x_t and y to it.
          push(call_x_t);
          push(call_y);
          push(call_k);
          goto *fn(call_x_h);
        } else if (call_x_h & 0x7ull == 0x1ull) {
          // Case 2. It's a symbol, so create the binding. (Flattening the conditional by relying on the precondition that (application n x) implies that x ==
          // bind)
          push(call_k);
          push(call_x_h);
          push(call_y);
          push(&&call_application_symbol_bind);
          goto eval;
        }

        call_application_symbol_bind:
          call_y = pop;
          call_x = pop;
          push(m[unpack(call_x)] = call_y);
          goto *fn(pop);

      call_composition:
        call_k   = pop;
        call_y   = pop;
        call_x   = pop;
        push(call_k);
        push(h(call_x));
        push(t(call_x));
        push(call_y);
        push(&&call_composition_c1);
        goto call;

        call_composition_c1:
          call_x = pop;
          call_y = pop;
          call_k = pop;
          push(call_x);
          push(call_y);
          push(call_k);
          goto call;

      call_expander:
      call_continuation:
        goto *fn(pop);

      call_s_ref:
      call_s_o:
      call_s_cons:

    return 0;
  }
__GMfjWltod3iCFAG39/LVGQyNY4vpDTStCqE56TEeQwc

meta::code('cached-dependencies', <<'__zAONWIYddTlEUIbTHlpH/FJZmvmqdplRGS+yLUqP7Cs');

Gnarly Standard Library | Spencer Tipping <spencer@spencertipping.com>
Licensed under the terms of the MIT source code license.

Quoting.
Applying the identity beta expander consumes an evaluation step but does not carry it forward. Thus it can be useful for absorbing an evaluation as quoting
is commonly used to do. In particular, it can delay evaluation for parameters passed to functions; for instance, (o my-function ' stuff), or, more
concisely, ('> my-function stuff) will treat the /stuff/ as being quoted, just as if (my-function (' stuff)) had been typed. The double-quote operator, '',
is used in conjunction with o'. o' is the system composition function, which does not protect the results of the first function from the effects of the
second function's argument evaluation.  Also in (' ::> bind (:< (beta _ (o' _ ::)))) the mix is the :: operator, which double-evaluates an expression. This
is equivalent to Common Lisp's /eval/ function, which evaluates its argument implicitly (since it is not a macro), and then evaluates it again.

  (beta _ _ ' bind (beta _ _))
  (' ''       bind (beta _ (' _)))
  (' ::       bind (o' : :))

Composition.
These let you easily put evals or quotes onto existing functions.

  (' :<  bind (o'  :))
  (' ::< bind (o' ::))
  (' '<  bind (o'  '))
  (' :>  bind (:< (beta _ (o' _  :))))
  (' ::> bind (:< (beta _ (o' _ ::))))
  (' '>  bind (:< (beta _ (o' _ ''))))

Normal composition.
This doesn't double-evaluate stuff when there are two functions. This is by virtue of the fact that we quote the result of the first function. If the second
function doesn't evaluate, then it will just get a quoted value; however, it makes no sense to place a non-evaluating function on the left-hand side of a
composition.

  (' o bind (:< (beta _ (o' (o' _ ')))))

Functions and macros.
Definitions for creating functions and macros.

  tr = translate code. This is an unevaluative macro that essentially does beta-substitution and evaluates its output.
  fn = function. This behaves like lambda in most languages, except that you can compose the quote function (or any other beta) onto it to make it not evaluate
       its parameter.
  en = evaluative function. It evaluates the things you pass in as the /name/ and /expansion/.
  qn = quoted function. Just like function, except that its parameter is not evaluated.

  (' tr bind (:< (::> (beta ev (:< (beta name (:< (beta ev (:< (beta name ev)))))))) (' (gensym (' γ)))))
  (' fn bind (::> (tr ev (tr name (tr ev (o' (:< (beta name ev)) placeholder))))     (' (gensym (' λ)))))

  (' en bind     (fn name     (fn expansion (:> (:> fn name) expansion))))
  (' qn bind ('> (fn name ('> (fn expansion ('> (en name expansion)))))))

Shorthands.
let and def are two shorthands for common operations. let is used to define a local variable, like this:
|
| (let x 5 (x + 1))
|
and def is used to define an entry in the global symbol table, like this:
|
| (def foo (' bar))
|
Note that there aren't really any local variables -- they are all constants and get rewritten to placeholder expressions during function application.

  (' let bind (qn name (fn value (qn expansion ((en name expansion) value)))))
  (' def bind (tr name (' name bind)))

  (def _ (fn _))

Y combinator.
There are cases where using the global symbol table for recursion is suboptimal. One example is when you are trying to optimize a function using the
optimization library. In this case, you want to eliminate recursive symbol references and use the Y combinator instead. Since Gnarly uses call-by-value, I'm
encoding Z instead of Y.

  (def z  (fn f (fn x (f (qn y (:> (x x) y))) (fn x (f (qn y (:> (x x) y)))))))
  (def z> (qn name (qn body (z (en name body)))))

Booleans and relational operators.
Booleans are Church-encoded, and logical operators try to get the result as much evaluated as possible to maximize performance. The meanings of these functions
correspond to their names, with the first four ultimately rooted in the SK combinator calculus. K is a function that returns a constant-returning function; that
is, k x y = x. k' returns y; that is, it absorbs its parameter. Normally, k and k' are lazy in that they fail to evaluate their unused parameter. However,
sometimes it is necessary to evaluate both, so k: and k:' are provided for this purpose.

Relational operators operate on partially or fully ordered things. The only assumption that is made is that the things implement <=, the less-than-or-equal-to
operator. Deriving the other operators is trivial given this one, and it also provides an indication of whether something is not comparable to something else
(in this case, x <= y and y <= x are both false).

  (def k   (fn x (qn y x)))
  (def k'  (qn x (fn y y)))
  (def k:  (fn x (fn y x)))
  (def k:' (fn x (fn y y)))

  (def && (fn b1 (qn b2 (b1 (:: b2 k k') k'))))
  (def || (fn b1 (qn b2 (b1 k (:: b2 k k')))))
  (def !  (fn b (b k' k)))

  (def == (fn x (fn y (&& (x <= y) (y <= x)))))
  (def <  (fn x (fn y (&& (x <= y) (! (y <= x))))))
  (def >  (fn x (fn y (&& (y <= x) (! (x <= y))))))
  (def >= (fn x (fn y (y <= x))))
  (def <= (fn x (fn y (x <= y))))
  (def != (fn x (fn y (! (== x y)))))

Lists.
Here's what these functions are for.

  :? = a null check -- it behaves much like the ternary operator in C, but instead of zero/nonzero it does null/non-null.
  *  = map    -- usage is * function list. The mnemonic is that functions can distribute across lists via multiplication.
  %  = filter -- usage is % function list. The idea is that certain elements are inside some equivalence class, and others aren't. (Modulus is one example.)
  /  = reduce -- usage is / function list. For some reason I thought / felt like a fold operator, but I honestly don't know why.
  ++ = append -- usage is ++ list1 list2.

  reverse' is a helper function for reverse. It's a hack because right now I don't have letrec or a Y combinator.
  reverse reverses a list in linear time.

  list is a list constructor that behaves just as it does in Lisp or Scheme, except that it needs extra parens: (list (1 2 3)). It constructs the list in
  left-cons form, just like all lists in Gnarly: (((: . 1) . 2) . 3)

  (def :? (fn xs ('> == nil (type xs))))

  (def *  (z> r (fn f (fn xs (:? xs : (cons (r f (xs head)) (f (xs tail))))))))
  (def %  (z> r (fn f (fn xs (:? xs : (f (xs tail) (cons (r f (xs head)) (xs tail)) (r f (xs head))))))))
  (def /  (z> r (fn f (fn xs (:? (xs head) (xs tail) (f (r f (xs head)) (xs tail)))))))
  (def ++ (z> r (fn xs (fn ys (:? ys xs (cons (r xs (ys head)) (ys tail)))))))

  (def reverse' (z> r (fn xs (fn ys (:? ys xs (r (cons xs (ys tail)) (ys head)))))))
  (def reverse  (reverse' :))

  (def list ('> (* ::)))

Type detection.
These provide predicates that tell you whether a value is of a given type.

  (let type-predicate (qn fname (qn name (k:' (:> def fname (fn x (== name (type x)))) :)))
    ((type-predicate *? cons)
     (type-predicate /? symbol)
     (type-predicate #? number)
     (type-predicate @? expander)
     (type-predicate >? application)
     (type-predicate m? macro)
     (type-predicate p? placeholder)
     (type-predicate o? composition)))

Strings.
Sometimes you want to write a sentence. The easiest way to do this is to use lists of symbols separated out by spaces. Constructing one long symbol from a list
isn't too hard either; you can reduce the list under (x + (' " ") + y). The most common case is wanting to pass a string to a function; for this you want to
right-quote the function and combine it with the string generator. So there are two functions, $, which is a proper lambda that folds the list into a string,
and $'>, which right-quotes the result of right-composing the string generator onto a function.

Because I'm a softie, you can also use escaped symbol literals in the language. Note, however, that they aren't strings! The expression "foo bar bif" is a
symbol just as much as foo_bar_bif is, and both will be evaluated if given to a function. (Both will complain, too, if you haven't defined them.) So if you want
a "string" to behave like a proper string literal, it needs to be quoted, as in (' "foo bar").

  (def $   (o / _ (_ + (' " ") +)))
  (def $*  (o $ list))
  (def $'> (fn f (o f ('> $))))

  (def +' (qn s1 (qn s2 (s1 + s2))))
  (def =~ (fn p (:> perl (' "types::lambda('" + p +' "', sub {$_[0]->serialize() =~ /" + p +' "/ ? $types::true : $types::false})"))))

Cond.
This is cool. It behaves just like the Lisp cond form, so that you have a list of 2-tuples of conditions and values and the first true conditional has its value
returned. The internal workings below can be read as the following in English: "evaluate the tail of the first expression whose tail of whose head is true."

  (def cond (qn options (:: (o * _ (_ tail) (o % _ (:: (_ head tail)) (reverse options)) tail))))

Some Perl interfacing.
The core command set was designed to be able to interface with Perl. This is one example; we use interpreter internals to obtain reflection data about the value
being referenced. It's OK to do this in general, but just be aware that such code might conceivably break in the future.

  (def type (perl "types::lambda('[type]', sub {types::symbol(ref $_[0])})"))
  (def defperl (qn name (qn contents (name bind (:> perl (' "types::lambda('[" + name +' "]', sub {" + contents +' "})"))))))

Beta and composition disassembly.
You can take apart compiled functions. One reason for this is that they're never really compiled, just interpreted. So you can always inspect their definitions
at runtime and write new functions based on the ones that you have. The system provides two function constructors, beta and o'. These two operators look inside
those constructors; for a beta expander, !!< gives you the symbol being expanded and !!> gives you the expansion; for compositions, !!< gives you the outer, or
left, function, and !!> gives you the inner.

  (defperl !!< "${$_[0]}[0]")
  (defperl !!> "${$_[0]}[1]")

  (defperl r== "my $self = $_[0]; types::lambda('[' . $self->serialize() . ' r== ...]',
                  sub {$self eq $_[0] ? $types::true : $types::false})")

Assertions.
Basic things to assist testing.

  (defperl fail "terminal::message('fail', $transient{'last-failure'} = $_[0]->serialize()); types::nil()")

  (def fail$  ($'> fail))
  (def assert (qn predicate (fn e1 (fn e2
    (cond (((:: predicate e1 e2) :)
           (k                    (o ('> fail) $ ('> ++ ("Assertion failed:") (* serialize (list (predicate e1 e2))))))))))))

Function compilation.
Realistically, you shouldn't be using pure Gnarly functions for much. Not only are they tremendously slow, but their advantages are rarely needed for most
applications. Better is to compile the function, which results in a tremendous speed increase and makes certain assumptions about its behavior:

  1. All parameters to the function will evaluate their arguments.
  2. Macros defined in the global symbol table are constant. (If this is not the case, then you'll need to recompile the function.)

The compile function works on an existing beta-expander and assumes the form output by fn. If it is not of this form, then you will have a broken function or
Perl error.
__zAONWIYddTlEUIbTHlpH/FJZmvmqdplRGS+yLUqP7Cs

meta::code('integration-tests', <<'__Q4ZqcS3Gx3zaAHh8IKp8z9BT+IJxx4qAJ/5ZT2Y8kDU');
Integration tests. These are run after your code is loaded and the unit tests have been run.
__Q4ZqcS3Gx3zaAHh8IKp8z9BT+IJxx4qAJ/5ZT2Y8kDU

meta::code('source', <<'__hCXQ7E8pkw+lbmkTumknuIDjGphWebxCON1I9XTSL7w');
JavaScript generation | Spencer Tipping <spencer@spencertipping.com>
Licensed under the terms of the MIT source code license.

This library provides a Gnarly-like interface to define JavaScript functions. It's actually a DSL in Gnarly that generates JavaScript code, so it can be scripted and macro-driven in the same
ways that other Gnarly code can. The trigger is a constructor object called /js/, which is wrapped around some value and absorbs operators as proxies into JavaScript. It builds an expression
tree as you use it, so for example:

  js foo.bar + 6

returns a new js object with the expression tree (js-bop + (js-bop . foo bar) 6). This expression tree can then be serialized by invoking serialize:

  js foo.bar + 6 serialize      => (foo.bar+6)

  (def js-serialize (fn form (*? form (:: form) (o? form (form serialize) (serialize form)))))

  (def js-var  (fn name (fn v ('"var " + (js-serialize name) +' = + (js-serialize v) +' ;))))
  (def js-fn   (fn as (fn body ('"(function(){" + (js-var as (' arguments)) +' "return " + (js-serialize body) +' "})"))))
  (def js-bop  (qn op (fn v1 (fn v2 ('"(" + (js-serialize v1) + op + (js-serialize v2) +' ")")))))
  (def js-uop  (qn op (fn v ('"(" + (js-serialize v) +' ")"))))
  (def js-sub  (fn x (fn s (js-serialize x +' [ + (js-serialize s) +' ]))))
  (def js-call (fn f (fn xs (js-serialize f +' "(" + (/ (_ (_ +' , +)) (* js-serialize xs)) +' ")"))))

  (def jsc  (qn v (/? v (v bound? (v value) v) (:: v))))
  (def jsqn (qn name (qn body (qn v (:> (:> qn name) body (:> jsc v))))))

  (def js (qn v' (let v (jsc v') (qn action (cond (((== action (' serialize)) (js-serialize v))
                                                   ((== action (' extract))   v)
                                                   ((*? action)               (js (js-call v action)))
                                                   ((== action (' fn))        (qn as (js (js-fn v as))))
                                                   ((== action (' var))       (qn name (js (js-var name v))))
                                                   ((== action (' sub))       (jsqn _ (js (js-sub v _))))
                                                   (k:                        (jsqn _ (js (js-bop action v _))))))))))
__hCXQ7E8pkw+lbmkTumknuIDjGphWebxCON1I9XTSL7w

meta::code('tests', <<'__LyYaSZV1QHBrnAJWqdMoOVoeLahXQWFyeicpI7WKgqY');
JavaScript unit tests. These help make sure that things don't crash, though there isn't any validation.

  (def v1 (js 3 + 5))
  (def v2 (js 4 + 6))
  (v1 + v2 % (' bar) serialize)
__LyYaSZV1QHBrnAJWqdMoOVoeLahXQWFyeicpI7WKgqY

meta::configuration(':check', <<'__KvsssAe/QdPPsO6QGorfBgTAacCYF0TyU/r2trjV79o');
shell::initialize_symbol_table();
delete $transient{'last-failure'};

eval {shell::load_quietly('cached-dependencies', 'source', 'tests', 'integration-tests')};
terminal::message('error', $@) if $@;
'';     # Return silence on success.
__KvsssAe/QdPPsO6QGorfBgTAacCYF0TyU/r2trjV79o

meta::configuration(':export', <<'__btFu4SltjXa0eLKiqqrtZ+F3l4l/2XzUEaZXRE0jgyU');
&{':check'}();
return join("\n", retrieve('code::cached-dependencies', 'code::source')) . "\n\n" unless $transient{'last-failure'};
terminal::message('error', $transient{'last-failure'});
return '';
__btFu4SltjXa0eLKiqqrtZ+F3l4l/2XzUEaZXRE0jgyU

meta::configuration(':main', <<'__/vfwogrZnxJ3KpphPp94bXOA6L9YLCBkmxZkE/4LRUc');
shell::initialize_symbol_table();
run('cached-dependencies', 'source');
__/vfwogrZnxJ3KpphPp94bXOA6L9YLCBkmxZkE/4LRUc

meta::configuration(':test', <<'__LdPLavysWyPM5r4ze8pyazYiWAfGW+7BSHx7ItKQOfI');
&{':main'}();
run('tests', 'integration-tests');
__LdPLavysWyPM5r4ze8pyazYiWAfGW+7BSHx7ItKQOfI

meta::data('default-action', <<'__zmNcTqv/Xk9W26j7HjnKI1UwqitrGFM+7xrzhiAWxXc');
shell
__zmNcTqv/Xk9W26j7HjnKI1UwqitrGFM+7xrzhiAWxXc

meta::data('meta-associations', <<'__aEJIsIbfe/K4YicYlSn/uNNc4heo44dfrgLgKgcmkuE');
^function:: .pl
^library:: .pl
^internal_function:: .pl
^meta:: .pl
^bootstrap:: .pl
^configuration:: .pl
^code:: .gnarly
^vim_highlighter:: .vim
^c:: .lc
^ocaml:: .locaml

__aEJIsIbfe/K4YicYlSn/uNNc4heo44dfrgLgKgcmkuE

meta::function('add-to', <<'__KBgra0vG1gIsUI8CCVf4ZEdCatZDCdVO6HuUx+jOJ9Q');
my ($filename) = @_;
my @members = grep /^implementation::/, keys %data;

for (@members) {
  my $destination_name = basename($_);
  open my($handle), "| $filename import $destination_name" or messages::error("Attribute $_ could not be written.");
  print $handle retrieve($_);
  close $handle;
}
__KBgra0vG1gIsUI8CCVf4ZEdCatZDCdVO6HuUx+jOJ9Q

meta::function('api-name', <<'__qA8PQrovsL6EIw5iatzFboQXAdvl064VoxzO6b8D3oQ');
&{'module-name'}() . ':' . &{'module-api-version'}();
__qA8PQrovsL6EIw5iatzFboQXAdvl064VoxzO6b8D3oQ

meta::function('attributes', <<'__SHSGXpl5WufcHc7hGxs0XPxlKSgo0XQ752HpjI4rgRg');
grep /^attribute::/, sort keys %data;
__SHSGXpl5WufcHc7hGxs0XPxlKSgo0XQ752HpjI4rgRg

meta::function('cat', <<'__h2PeSpk/pPmrzLRTTofdLTbhj06IWUw5WWke6ggUsdk');
my ($name) = @_;
$data{$name};
__h2PeSpk/pPmrzLRTTofdLTbhj06IWUw5WWke6ggUsdk

meta::function('clone', <<'__qP6xPZE75s9g0XJIiC6FGw0vnj2j0glUzsAHxyA3lvY');
for (@_) {
  if ($_) {
    eval {
      file::write($_, serialize(), noclobber => 1);
      chmod(0700, $_);
      print "File $_ cloned successfully.\n";
    };

    print "$@\n" if $@;
  }
}
__qP6xPZE75s9g0XJIiC6FGw0vnj2j0glUzsAHxyA3lvY

meta::function('cp', <<'__yn1SQkcEk6o+gnuCy3QGVFtQb2piaCoUdJPGUkLjpD4');
my ($from, $to) = @_;
$data{$to} = $data{$from} if $data{$from};
messages::error("No such attribute $from") unless $data{$from};
$data{$from};
__yn1SQkcEk6o+gnuCy3QGVFtQb2piaCoUdJPGUkLjpD4

meta::function('create', <<'__YDNTuzkJSNUIk4tbdwxep6/rT8uGnceIj7rljM9gusc');
my ($name, $value) = @_;
messages::error("Attribute $name already exists.") if grep {$_ eq $name} keys %data;
if ($value) {
  associate($name, $value);
} else {
  associate($name,'');
  edit ($name);
}
__YDNTuzkJSNUIk4tbdwxep6/rT8uGnceIj7rljM9gusc

meta::function('edit', <<'__wmkQAgPhtPrNx2Sc88M1MNK4gG2sn/UGqsnAtRv6ksY');
my ($name, %options) = @_;

my $meta_extension = join '', grep {
  my $s = $_;
  $s =~ s/\s.*$//;
  $name =~ /$s/
} split /\n/, &{'meta-associations'}();

$meta_extension =~ s/^.*\s//;
chomp $meta_extension;

messages::error("Attribute $name does not exist.") unless grep {$_ eq $name} keys %data;
associate($name, invoke_editor_on($data{$name} || "# Attribute $name", %options, extension => $meta_extension),
          execute => $name !~ /^internal::/ && $name !~ /^bootstrap::/);
save();
__wmkQAgPhtPrNx2Sc88M1MNK4gG2sn/UGqsnAtRv6ksY

meta::function('exists', <<'__bxU1sDtIh3+P1x0HuuY0f7sKHr9qNZUEl64m2fvwmDk');
my $name = shift;
grep {$_ eq $name} keys %data;
__bxU1sDtIh3+P1x0HuuY0f7sKHr9qNZUEl64m2fvwmDk

meta::function('full-name', <<'__X5DqVkiY6nnaoUwhy1rWlslSD2vlNZ5zkbCZRfb1ITI');
&{'api-name'}() . ':' . &{'module-revision'}();
__X5DqVkiY6nnaoUwhy1rWlslSD2vlNZ5zkbCZRfb1ITI

meta::function('grab', <<'__sXs1aeJVBERH6nWE7ZpWiIO5Cg7fSBWcoscDg1DHzD8');
my ($filename, @attribute_names) = @_;
associate("implementation::$_", `$filename cat $_`) for @attribute_names;
__sXs1aeJVBERH6nWE7ZpWiIO5Cg7fSBWcoscDg1DHzD8

meta::function('import', <<'__oK2Kj5RYHcEUK0Iyiqu8w7zipbg+QNF4VO4hm7BkUNA');
my ($name) = @_;
associate($name, join('', <STDIN>));
__oK2Kj5RYHcEUK0Iyiqu8w7zipbg+QNF4VO4hm7BkUNA

meta::function('load-dependencies', <<'__oYAWdUSIg7K9xPssNpQhu7nUGr94qBGg5QJpTDAA3sA');
my $errors = '';
for (grep length, split /\n/, &{'dependencies'}()) {
  terminal::message('loader', "Running integration test checks on $_...");
  my $output = `$_ :check`;
  chomp $output;
  $errors .= $output ? "module $_\n$output\n" : '';
}

return terminal::message('error', "dependency loading failed:\n$errors") if $errors;
associate('code::cached-dependencies', join('', map `$_ :export`, grep length, split /\n/, &{'dependencies'}()));
terminal::message('loader', "imported dependencies");
__oYAWdUSIg7K9xPssNpQhu7nUGr94qBGg5QJpTDAA3sA

meta::function('lock', <<'__pqf/HijyN91BWpnS+uWYip/mFhHhcd+M9/YdlYsvv9Y');
my (undef, undef, $mode) = stat $0;
chmod $mode & 0555, $0;
__pqf/HijyN91BWpnS+uWYip/mFhHhcd+M9/YdlYsvv9Y

meta::function('ls', <<'__M3wGXSw8/xm3RiNq0uLWke1dHm2OWQbvJpHkngdPafg');
join("\n", sort keys %externalized_functions);
__M3wGXSw8/xm3RiNq0uLWke1dHm2OWQbvJpHkngdPafg

meta::function('ls-a', <<'__3Ou+kVmaLIe1oZKBOufKY7pSsksgTSAfNeXRniPFZPk');
join("\n", sort keys %data);
__3Ou+kVmaLIe1oZKBOufKY7pSsksgTSAfNeXRniPFZPk

meta::function('mv', <<'__ijyNZ8r34FVK0Ki9/Q0Irx5k9U0pZ+/frrdlu+qkEP4');
my ($from, $to) = @_;
messages::error("The '$from' attribute does not exist.") unless grep $from, keys %data;
associate($to, retrieve($from));
rm($from);
__ijyNZ8r34FVK0Ki9/Q0Irx5k9U0pZ+/frrdlu+qkEP4

meta::function('perl', <<'__Ojd593Fa9fx1Yx2XuPzK6WTUyxO70Nbmlbl9YRodUWA');
my $result = eval($_[0]);
$@ ? $@ : $result;
__Ojd593Fa9fx1Yx2XuPzK6WTUyxO70Nbmlbl9YRodUWA

meta::function('pop-state', <<'__eqnCLsMapvq2sYSx82KCxh25zmff+JIXFMYUan2kGKM');
%data = %{pop @{$transient{'states'}}} if @{$transient{'states'}};
reload();
__eqnCLsMapvq2sYSx82KCxh25zmff+JIXFMYUan2kGKM

meta::function('pull', <<'__ZU6uOu7dBdjjoNdEL/U7yrjicOQR5OLFQAacjrKqSCg');
my ($class_name) = @_;
my @attributes = grep /^implementation::/, split /\n/, `$class_name ls-a`;

for (@attributes) {
  s/^\s+//;
  s/\s+$//;
  print STDERR "Adding $_\n";
  associate(basename($_), `$class_name cat "$_"`);
}
__ZU6uOu7dBdjjoNdEL/U7yrjicOQR5OLFQAacjrKqSCg

meta::function('push-state', <<'__ik0ofu7R8gHAKSmMjek79V+yfgdjdK5Jmtwf7h8SpJk');
push @{$transient{'states'} = $transient{'states'} || []}, {%data};
my $state_count = scalar @{$transient{'states'}};
"There are now $state_count states on the stack.";
__ik0ofu7R8gHAKSmMjek79V+yfgdjdK5Jmtwf7h8SpJk

meta::function('reload', <<'__GwQjnnfuj0xQlervDJ9EVWzdmdz+XL3Gq0i9rdejvzM');
execute($_) for (grep {! (/^internal::/ || /^bootstrap::/)} keys %data);
__GwQjnnfuj0xQlervDJ9EVWzdmdz+XL3Gq0i9rdejvzM

meta::function('repl', <<'__0gcQcyYhbrhjjhxxlOrXqdYD4Scmtqtwj2I3HF/c1Xo');
my ($name, @options) = @_;

use Term::ReadLine;

my $term = new Term::ReadLine "$0 repl for $name";
$term->ornaments(0);
my $OUT    = $term->OUT || \*STDOUT;
my $prompt = &{'api-name'}() . "/$name> ";

terminal::message('repl', 'initializing symbol table');
types::clear_symbols();
types::load_symbols($name) if $name;
types::initialize_symbols();
types::initialize_debugging_symbols() if grep /^debug$/, @options;

terminal::message('repl', 'running session');
run('cached-dependencies') unless grep /^nodeps$/, @options;
run($name) if $name;

terminal::message('repl', 'ready');
while (defined ($_ = $term->readline($prompt))) {
  my $result = eval {reader::read_form($_)->eval()->serialize()};
  $@ ? chomp $@ && terminal::message('error', $@) : terminal::message('result', $result);
}

print "\n";
if ($name) {
  terminal::message('repl', 'storing symbol table');
  types::store_symbols($name);
}

terminal::message('repl', 'exiting');
__0gcQcyYhbrhjjhxxlOrXqdYD4Scmtqtwj2I3HF/c1Xo

meta::function('rm', <<'__7BVECTVo/mcT5+edC70WPc6S1xCbzAeyUCfCjkKWlww');
for my $to_be_deleted (@_) {
  messages::warning("$to_be_deleted does not exist") unless grep {$_ eq $to_be_deleted} keys %data;
}

delete @data{@_};
__7BVECTVo/mcT5+edC70WPc6S1xCbzAeyUCfCjkKWlww

meta::function('run', <<'__2RluS4s8uRjHD6Vtfi/a01P+Bo8jgpcLILqPCD4icpU');
shell::load_interactively(@_);
__2RluS4s8uRjHD6Vtfi/a01P+Bo8jgpcLILqPCD4icpU

meta::function('run-file', <<'__WceprRaNezbwObGfB9h6pwi0Ly281gdWz8CLtXL1ehY');
shell::initialize_symbol_table();
&{':main'}();
shell::load_from_file(@_);
__WceprRaNezbwObGfB9h6pwi0Ly281gdWz8CLtXL1ehY

meta::function('save', <<'__uWXGnrQr+A7Cl0zcsDuiokbWAw1XdMdjeq9gDcTMJIw');
my $serialized_data = serialize();
my $final_state     = state();

my (undef, $temporary_filename) = tempfile("$0." . 'X' x 32, OPEN => 0);
file::write($temporary_filename, $serialized_data);
chmod 0700, $temporary_filename;

my $observed_state = `perl $temporary_filename state`;
chomp $observed_state;
if ($observed_state ne $final_state) {
  messages::error("The state of this object ($final_state) is inconsistent with the state of $temporary_filename ($observed_state).\n" .
                  "$0 has not been updated.");
} else {
  eval {file::write($0, $serialized_data)};
  warn $@ if $@;
  my $observed_self_state = `perl $0 state`;
  chomp $observed_self_state;
  unlink $temporary_filename if $observed_self_state eq $final_state;
}
__uWXGnrQr+A7Cl0zcsDuiokbWAw1XdMdjeq9gDcTMJIw

meta::function('serialize', <<'__KGiI48MlyG6RAVW5QYRK8y97y8tx+jeAwPlY5eDtMTw');
my @keys_without_internals = grep(!/^internal::/, sort keys %data);
join "\n", $data{'bootstrap::initialization'},
           (grep {$_} (map {serialize::single(@_)} grep(/^meta::/,  @keys_without_internals),
                                                   grep(!/^meta::/, @keys_without_internals),
                                                   grep(/^internal::/, sort keys %data))),
           "__END__";
__KGiI48MlyG6RAVW5QYRK8y97y8tx+jeAwPlY5eDtMTw

meta::function('shell', <<'__y24Z0MyfVsS8IjvGqS/aSBwIs2L5yzllPmATpFIhCJg');
use Term::ReadLine;

my $term = new Term::ReadLine "$0 shell";
$term->ornaments(0);
my $prompt = &{'api-name'}() . '$ ';
my $OUT = $term->OUT || \*STDOUT;

$term->Attribs->{attempted_completion_function} = \&complete;

while (defined ($_ = $term->readline($prompt))) {
  my $command_line = $_;
  my @args = grep length, split /\s+|("[^"\\]*(?:\\.)?")/o;
  my $function_name = shift @args;

  return if $function_name eq 'exit';

  s/^"(.*)"$/\1/o, s/\\\\"/"/go for @args;

  if ($function_name) {
    if ($externalized_functions{$function_name}) {
      my $result = eval {&{$function_name}(@args)};
      messages::warning($@) if $@;
      chomp $result;
      print $OUT $result, "\n" unless $@;
    } else {
      messages::warning("Command not found: $function_name");
    }
  }

  for my $watch (@{$transient{'watch_list'}}) {
    print $OUT eval($watch), "\n";
    print $OUT "Error evaluating watched expression $watch: $@\n" if $@;
  }

  $prompt = &{'api-name'}() . '$ ';
}
__y24Z0MyfVsS8IjvGqS/aSBwIs2L5yzllPmATpFIhCJg

meta::function('size', <<'__lDGr6yVnDwcDWLkJH16MNukltjG2ypBSk/ktYb80h80');
length(serialize());
__lDGr6yVnDwcDWLkJH16MNukltjG2ypBSk/ktYb80h80

meta::function('snapshot', <<'__qjqsCy4CTt88dIi7IWM+Varpb3GcHsYrFTxW7EwpLW0');
my ($name) = @_;
file::write(my $finalname = state_based_filename($name), serialize(), noclobber => 1);
chmod 0700, $finalname;
__qjqsCy4CTt88dIi7IWM+Varpb3GcHsYrFTxW7EwpLW0

meta::function('state', <<'__1S8nzRSMoxJU/VEv2rx/NrAt1iRgXQ9ugxjUP3IFunI');
sha256_base64 serialize();
__1S8nzRSMoxJU/VEv2rx/NrAt1iRgXQ9ugxjUP3IFunI

meta::function('unlock', <<'__08PohCY8fcNe+pWCO6ic6XOOKv48NkrxpNMmTOUIFdA');
my (undef, undef, $mode) = stat $0;
chmod $mode | 0200, $0;
__08PohCY8fcNe+pWCO6ic6XOOKv48NkrxpNMmTOUIFdA

meta::function('update-from', <<'__/BG6eQgzKZ5JxbCXzs15unjydN/vRfpPrVepEyA7CYs');
# Upgrade all attributes that aren't customized. In this case, we want everything except for configuration::, code::, and attribute::.
return "That is a really bad idea." if $0 =~ /\.\/(.*)/ && $_[0] eq $1 || $_[0] eq $0;

&{'push-state'}();
print "Updating... Please be patient as this is an unjustifiably slow process.\n";
for my $attribute (grep length && ! (/^configuration::/ || /^code::/ || /^attribute::/ || /^function::pop-state$/ ||
                                     /^list::/ || /^issue::/ || /^data::/), split(/\n/, `$_[0] ls-a`)) {
  associate($attribute, join('', `$_[0] cat $attribute`));
  reload();     # Necessary to activate new datatypes.
  print '.';
}

print "\nReloading new attributes\n";
reload();
"Imported from $_[0]. Run pop-state to undo this change.\n(The value of pop-state wasn't changed during the update.)";
__/BG6eQgzKZ5JxbCXzs15unjydN/vRfpPrVepEyA7CYs

meta::function('usage', <<'__oHVev4RtZlF/82SSE87y4Bf7ran2afn/HDtukOQBf9I');
<<"EOD" . join '  ', split /\n/, ls ();
Usage: $0 [options] action [arguments]
Defined actions:
EOD
__oHVev4RtZlF/82SSE87y4Bf7ran2afn/HDtukOQBf9I

meta::function('vim', <<'__1EcCMR8Tks8HBoOg+zAKJ4LlrRIY8nvLs4M1VTr2Zec');
# Installs VIM highlighters.
file::write("$ENV{'HOME'}/.vim/syntax/$_.vim", retrieve("vim_highlighter::$_")) for map {s/^vim_highlighter:://o; $_} grep /^vim_highlighter::/, sort keys %data;
__1EcCMR8Tks8HBoOg+zAKJ4LlrRIY8nvLs4M1VTr2Zec

meta::internal_function('associate', <<'__D8BKmEFp/adiPPqPnXyMOzlsBMCmuZi62UpJWdoFg/0');
my ($name, $value, %options) = @_;
my $namespace = namespace($name);
messages::error("Namespace $namespace does not exist") unless grep {$_ eq $namespace} @data_types;
$data{$name} = $value;
execute($name) if $options{'execute'};
__D8BKmEFp/adiPPqPnXyMOzlsBMCmuZi62UpJWdoFg/0

meta::internal_function('basename', <<'__T4JEqOUYjMzssdVwV/rdgAhvr0Vz9TQUo0noTdeBLxw');
my ($name) = @_;
$name =~ s/^[^:]*:://;
$name;
__T4JEqOUYjMzssdVwV/rdgAhvr0Vz9TQUo0noTdeBLxw

meta::internal_function('compile_c_source', <<'__wpMpoal70gwcOkAF6fsip4jYBUSQ7Prp6oweFI40P3U');
write_c_source($_[0]);
my $source = $transient{"last_unlit_$_[0]"};
qx|g++ -O2 -g -o $source.out $source| if $source;
__wpMpoal70gwcOkAF6fsip4jYBUSQ7Prp6oweFI40P3U

meta::internal_function('compile_java_source', <<'__dkNCqD6IGlM2F2k+9vsSEZ868TxeVar5XgNbA3qpBks');
write_java_source($_[0]);
`javac -g $transient{"last_unlit_$_[0]"}` if $transient{"last_unlit_$_[0]"};
__dkNCqD6IGlM2F2k+9vsSEZ868TxeVar5XgNbA3qpBks

meta::internal_function('compile_ocaml_source', <<'__fYqhn2XlWxz8txxm4W1J34KcfOCv0isgVyGP++DFGYE');
write_ocaml_source($_[0]);
my $source = $transient{"last_ocaml_unlit_$_[0]"};
qx|ocamlopt -o $source.out $source| if $source;
__fYqhn2XlWxz8txxm4W1J34KcfOCv0isgVyGP++DFGYE

meta::internal_function('complete', <<'__lhlD80z2kvEUEeHPqLFw6JE8xUdXr6J5Q1gXHg4beHg');
my @functions  = sort keys %externalized_functions;
my @attributes = sort keys %data;

sub match {
  my ($text, @options) = @_;
  my @matches = sort grep /^$text/, @options;

  if    (@matches == 0) {return undef;}
  elsif (@matches == 1) {return $matches [0];}
  elsif (@matches >  1) {return ((longest ($matches [0], $matches [@matches - 1])), @matches);}
}

sub longest {
  my ($s1, $s2) = @_; 
  return substr ($s1, 0, length $1) if ($s1 ^ $s2) =~ /^(\0*)/;
  return ''; 
}

# This is another way to implement autocompletion.
#
# my $attribs = $term->Attribs;
# $attribs->{completion_entry_function} = $attribs->{list_completion_function};
# $attribs->{completion_word} = [sort keys %data, sort keys %externalized_functions];

my ($text, $line) = @_;
if ($line =~ / /) {
  # Start matching attribute names.
  match ($text, @attributes);
} else {
  # Start of line, so it's a function.
  match ($text, @functions);
}
__lhlD80z2kvEUEeHPqLFw6JE8xUdXr6J5Q1gXHg4beHg

meta::internal_function('execute', <<'__Ge94WTpmLuqsMDappj5G/G2BKILAE0GjeCqAeHLW6fQ');
my ($name, %options) = @_;
my $namespace = namespace($name);
eval {&{"meta::$namespace"}(basename($name), retrieve($name))};
warn $@ if $@ && $options{'carp'};
__Ge94WTpmLuqsMDappj5G/G2BKILAE0GjeCqAeHLW6fQ

meta::internal_function('file::read', <<'__ZxBqZsMZZRuLMQp8Sy//ZsoAvriDebjYLGAX7p7AxXg');
my $name = shift;
open my($handle), "<", $name;
my $result = join "", <$handle>;
close $handle;
$result;
__ZxBqZsMZZRuLMQp8Sy//ZsoAvriDebjYLGAX7p7AxXg

meta::internal_function('file::write', <<'__+NhpMabvNL+hHZaTZwBoFx2IFa79cjOZwGxEXX+xG0o');
my ($name, $contents, %options) = @_;
die "Choosing not to overwrite file $name" if $options{'noclobber'} && -f $name;
open my($handle), ">", $name or die "Can't open $name for writing";
print $handle $contents;
close $handle;
__+NhpMabvNL+hHZaTZwBoFx2IFa79cjOZwGxEXX+xG0o

meta::internal_function('invoke_editor_on', <<'__97Lgs5+qfyAu92Vv5GCVVSYgUgFhOKYkVYXlbWoUs6U');
my ($data, %options) = @_;
my $content_hash     = sha256_base64($data);
my $editor           = $options{'editor'} || $ENV{'VISUAL'} || $ENV{'EDITOR'} ||
                       messages::error('Either the $VISUAL or $EDITOR environment variable should be set to a valid editor.');
my $options          = $options{'options'} || $ENV{'VISUAL_OPTS'} || $ENV{'EDITOR_OPTS'} || '';
my $extension        = $options{'extension'} || '';

my (undef, $filename) = tempfile("$0." . ("X" x 32), OPEN => 0);
$filename .= $extension;

file::write($filename, $data);
system("$editor $options \"$filename\"");

my $result = file::read($filename);
unlink $filename;
$result;
__97Lgs5+qfyAu92Vv5GCVVSYgUgFhOKYkVYXlbWoUs6U

meta::internal_function('messages::error', <<'__200qXouilOAQNa4NkmIj6l+Rvb49Jpy8yxvIX29NcK4');
my ($message) = @_;
die "$message\n";
__200qXouilOAQNa4NkmIj6l+Rvb49Jpy8yxvIX29NcK4

meta::internal_function('messages::warning', <<'__DeU/1Klulk/y4fO+wtKt+liOmUKwCEYKM8BvtlXYXBc');
my ($message) = @_;
print "$message\n";
__DeU/1Klulk/y4fO+wtKt+liOmUKwCEYKM8BvtlXYXBc

meta::internal_function('namespace', <<'__D7UfKyyYZ1slZZyaS28hIt8a68jkI3ELBaddROXOHug');
my ($name) = @_;
$name =~ s/::.*$//;
$name;
__D7UfKyyYZ1slZZyaS28hIt8a68jkI3ELBaddROXOHug

meta::internal_function('retrieve', <<'__Erqqkp11FEHKsitr0DEJZ6OCGDYAs+U6BSu4UvLvsFM');
@data{@_};
__Erqqkp11FEHKsitr0DEJZ6OCGDYAs+U6BSu4UvLvsFM

meta::internal_function('run_c', <<'__2OLcajGatrqxSRYd/mjbxfnXRXElYKwTdFHDOi/on98');
qx|$_[0].out|;
__2OLcajGatrqxSRYd/mjbxfnXRXElYKwTdFHDOi/on98

meta::internal_function('run_java', <<'__STDNs4KOIo1nTEhGS9O5xBT6n4cV2eDkeL9+07u9UYw');
system("java -cp /tmp $_[0]");
__STDNs4KOIo1nTEhGS9O5xBT6n4cV2eDkeL9+07u9UYw

meta::internal_function('run_ocaml', <<'__Wh2aafSnjcntecRIveSYlNtu80e1Fv4gaw2B1bnTd7M');
qx|$transient{"last_ocaml_unlit_$_[0]"}.out|;
__Wh2aafSnjcntecRIveSYlNtu80e1Fv4gaw2B1bnTd7M

meta::internal_function('serialize::single', <<'__lDBHaXpbrfER2envI2Ipy77IcdjUnlZou+rggaxsAWE');
my $name               = shift || $_;
my $contents           = $data{$name};
my $delimiter          = "__" . sha256_base64 $contents;
my $meta_function_name = "meta::" . namespace($name);
my $invocation_name    = basename $name;
"$meta_function_name('$invocation_name', <<'$delimiter');\n$contents\n$delimiter\n";
__lDBHaXpbrfER2envI2Ipy77IcdjUnlZou+rggaxsAWE

meta::internal_function('state_based_filename', <<'__zNSrihAkMKJG5spRYgcFdoNArFKig1u12gIp6gJ8pZw');
my ($name) = @_;
my $noise  = $name || state();
$noise =~ s/\//-/g;
"$0.$noise";
__zNSrihAkMKJG5spRYgcFdoNArFKig1u12gIp6gJ8pZw

meta::internal_function('unlit_c', <<'__sy7R/rmrsF0Mxz/rPAqYJHVok29baW9BzZ/g5ahZhO4');
join "\n\n", map /^\s*[a-z@#]/ ? $_ : "/* $_ */", split /\n\s*\n+/, $_[0];
__sy7R/rmrsF0Mxz/rPAqYJHVok29baW9BzZ/g5ahZhO4

meta::internal_function('unlit_java', <<'__TxYYAPyN8PqFYA+9ZwxL/2Su3rmYdaXL/MG2t+4RjlQ');
join "\n\n", map /^\s*[a-z]/ ? $_ : "/* $_ */", split /\n\s*\n+/, $_[0];
__TxYYAPyN8PqFYA+9ZwxL/2Su3rmYdaXL/MG2t+4RjlQ

meta::internal_function('unlit_ocaml', <<'__7n0vtAVbAsrjYbpOYU5OVncwiZD7PCq7fjR71QrwT/0');
join "\n\n", map /^\s*[a-z@#]/ ? $_ : "(* $_ *)", split /\n\s*\n+/, $_[0];
__7n0vtAVbAsrjYbpOYU5OVncwiZD7PCq7fjR71QrwT/0

meta::internal_function('write_c_source', <<'__+3eSSJPtzpvyQdOXsoWFX900DK9jqfkLu2f+cinBBVc');
file::write($transient{"last_unlit_$_[0]"} = '/tmp/' . state_based_filename . '.cpp', unlit_c(retrieve("c::$_[0]")));
__+3eSSJPtzpvyQdOXsoWFX900DK9jqfkLu2f+cinBBVc

meta::internal_function('write_java_source', <<'__3u037kPBFgaxUiYMupt5G6nEMZRkjFLTlGyHmdAnJp4');
file::write($transient{"last_unlit_$_[0]"} = '/tmp/' . state_based_filename . '.java', unlit_java(retrieve("java::$_[0]")));
__3u037kPBFgaxUiYMupt5G6nEMZRkjFLTlGyHmdAnJp4

meta::internal_function('write_ocaml_source', <<'__aJJakvlD5inz2QSkM9oxfQMUvxQruHLEk9tf2CDEz+Q');
file::write($transient{"last_ocaml_unlit_$_[0]"} = "/tmp/$0.ml", unlit_ocaml(retrieve("ocaml::$_[0]")));
__aJJakvlD5inz2QSkM9oxfQMUvxQruHLEk9tf2CDEz+Q

meta::issue('bugs', <<'__47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU');

__47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU

meta::issue('test-cases', <<'__KtdLV8oE00qsOR/lprnhBJF7moFLLNHXYD+k6Ih9sDs');
Static importing
__KtdLV8oE00qsOR/lprnhBJF7moFLLNHXYD+k6Ih9sDs

meta::issue('todos', <<'__47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU');

__47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU

meta::library('datatypes', <<'__q1LSLuHGLAt+1XUTuRfyzIH3/XWigXMQ1PScnZNIjJI');
# Data type definitions for the interpreter.

package types;

my %symbol_table;

sub clear_symbols {%symbol_table = ()}
sub store_symbols {my %h = %symbol_table; $transient{$_[0]} = \%h}
sub load_symbols  {%symbol_table = %{$transient{$_[0]}} if ref $transient{$_[0]} eq 'HASH'}

sub initialize_debugging_symbols {
  symbol('counter')->bind(counter(0));
  symbol('trace')->bind(lambda('[trace]', sub {
    my $value = $_[0];
    terminal::message('trace', $_[0]->serialize());
    $value;
  }));
}

sub initialize_symbols {
  symbol('gensym')->bind(lambda('[gensym]', \&gensym));
  symbol('placeholder')->bind(lambda('[placeholder]', sub {placeholder($_[0])}));

  symbol('beta')->bind(macro('[beta]', sub {
    my $symbol = $_[0];
    die 'Cannot create beta-expander for non-symbol value ' . $symbol->serialize() unless ref $symbol eq 'symbol';
    application(symbol('beta')->eval(), $symbol, macro('', sub {
      expander($symbol, $_[0]);
    }));
  }));

  symbol('cons')->bind(lambda('[cons]', sub {
    my $head = $_[0];
    application(symbol('cons')->eval(), $head, lambda('', sub {
      cons($head, $_[0]);
    }));
  }));

  symbol('o\'')->bind(lambda('[o\']', sub {
    my $f = $_[0];
    application(symbol('o\'')->eval(), $f, lambda('', sub {
      composition($f, $_[0]);
    }));
  }));

  symbol(':')->bind(nil());

  symbol('serialize')->bind(lambda('[serialize]', sub {symbol($_[0]->serialize())}));

  symbol('perl')->bind(macro('[perl]', sub {
    my $result = eval $_[0]->serialize();
    die $@ if $@;
    $result;
  }));
}

sub trace {
  my ($f, $name) = @_;
  sub {
    my $result = eval {$f->(@_)};
    terminal::message($name, $_[0]->serialize() . ($_[1] ? ' invoked on ' . $_[1]->serialize() : '')) and die $@ if $@;
    $result;
  }
}

sub deftype {
  my ($name, %members) = @_;
  *{"types::$name"} = sub {bless $members{'new'}->(@_), $name};
  *{"${name}::$_"} = $members{$_} for keys %members;

  # Treat eval and call specially. We want to make sure that we have a backtrace of values at the very least.
  *{"${name}::$_"} = trace($members{$_}, $_) for qw/eval call/;
}

our $id = sub {$_[0]};

sub method {
  my %table = @_;
  sub {
    my $self = $_[0];
    my $lambda_wrap = sub {application($self, $_[0], macro('', $_[1]))};
    if (ref $_[1] eq 'symbol' && $table{${$_[1]}}) {
      # Default behavior -- return the value directly, wrapping it in a serializable lambda
      # closure only if it's a function.
      my $v = $table{${$_[1]}}->($self, $value);
      return $lambda_wrap->($_[1], $v) if ref $v eq 'CODE';
      return $v;
    } else {
      $table{''} ? $table{''}->($self, $_[1]) :
                   die 'Cannot apply ' . $self->serialize() . ' to ' . $_[1]->serialize();
    }
  };
}

my $nil = '';
deftype 'nil', new       => sub {\$nil},
               eval      => $id,
               call      => sub {$_[1]->eval()},
               serialize => sub {':'};

deftype 'composition', new       => sub {[@_]},
                       eval      => $id,
                       call      => sub {$_[0][0]->call($_[0][1]->call($_[1]))},
                       serialize => sub {'[' . $_[0][0]->serialize() . ' o\' ' . $_[0][1]->serialize() . ']'};

deftype 'macro',  new       => sub {[@_]},
                  eval      => $id,
                  call      => sub {$_[0][1]->($_[1])},
                  serialize => sub {ref $_[0][0] eq 'CODE' ? $_[0][0]->() : $_[0][0]};

deftype 'application', new       => sub {[@_]},
                       eval      => $id,
                       call      => sub {$_[0][2]->call($_[1])},
                       serialize => sub {'(' . $_[0][0]->serialize() . ' ' . $_[0][1]->serialize() . ' ...)'};

sub lambda {composition(macro(@_), nil())}

our $true  = lambda('true',  sub {my $x = $_[0]; application($true, $x, macro('', sub {$x}))});
our $false =  macro('false', sub {application($false, $_[0], lambda('', sub {$_[0]}))});

deftype 'symbol',
  new       => sub {\$_[0]},
  eval      => sub {$symbol_table{${$_[0]}} ||
                    die 'Cannot evaluate symbol ' . $_[0]->serialize() . ' because it is undefined.'},

  bind      => sub {$symbol_table{${$_[0]}} = $_[1]; $_[0]},
  unbind    => sub {delete $symbol_table{${$_[0]}}; $_[0]},
  equals    => sub {ref $_[1] eq 'symbol' ? ${$_[0]} eq ${$_[1]} : ${$_[0]} eq $_[1]},
  call      => method(bind     => sub {my $self = $_[0]; sub {$self->bind($_[0]->eval())}},
                      'bound?' => sub {$symbol_table{${$_[0]}} ? $true : $false},
                      unbind   => sub {my $self = $_[0]; sub {$self->unbind(); $_[0]->eval()}},
                      value    => sub {$_[0]->eval()},
                      '<='     => sub {my $self = $_[0]; sub {$$self le ${$_[0]->eval()} ? $true : $false}},
                      '+'      => sub {my $self = $_[0]; sub {symbol($$self . ${$_[0]->eval()})}},
                      split    => sub {cons_from_array(map {symbol($_)} split //, ${$_[0]})},
                      ''       => sub {$_[1]->eval()->call($_[0])}),
  serialize => sub {${$_[0]}};

my $gensym_count = 0;
sub gensym {
  my $value = $_[0];
  die 'Cannot call gensym on non-symbol value: ' . $value->serialize() unless ref $value eq 'symbol';
  ++$gensym_count;
  symbol("__$$value${gensym_count}__");
}

deftype 'placeholder',
  new       => sub {\$_[0]},
  eval      => sub {${$_[0]}},
  call      => sub {$_[0]->eval()->call($_[1])},
  serialize => sub {'[' . ${$_[0]}->serialize() . ']'};

sub lazy_rewrite {
  my ($form, $find, $replace) = @_;
  return cons(lazy_rewrite($form->head(), $find, $replace), lazy_rewrite($form->tail(), $find, $replace)) if ref $form eq 'cons' && $form->contents()->{$$find};
  return $replace                                                                                         if ref $form eq 'symbol' && $$form eq $$find;
  return $form;
}

deftype 'expander', new       => sub {\@_},
                    eval      => $id,
                    head      => sub {$_[0][0]},
                    tail      => sub {$_[0][1]},
                    call      => sub {lazy_rewrite($_[0]->tail(), $_[0]->head(), $_[1])},
                    serialize => sub {'[' . $_[0]->head()->serialize() . ' -> ' . $_[0]->tail()->serialize() . ']'};

deftype 'counter', new       => sub {\$_[0]},
                   eval      => sub {counter(${$_[0]} + 1)},
                   call      => sub {die 'Cannot call ' . $_[0]->serialize() . ' on ' . $_[1]->serialize()},
                   serialize => sub {"[counter +${$_[0]}]"};

deftype 'cons', new       => sub {
                               my %contents;
                               if (ref $_[0] eq 'cons') {$contents{$_}++ for keys %{$_[0]->contents()}}
                               if (ref $_[1] eq 'cons') {$contents{$_}++ for keys %{$_[1]->contents()}}
                               $contents{${$_[0]}}++ if ref $_[0] eq 'symbol';
                               $contents{${$_[1]}}++ if ref $_[1] eq 'symbol';
                               [$_[0], $_[1], \%contents];
                             },
                contents  => sub {$_[0][2]},
                eval      => sub {$_[0]->head()->eval()->call($_[0]->tail())},
                head      => sub {$_[0][0]},
                tail      => sub {$_[0][1]},
                call      => method(head => sub {$_[0]->head()},
                                    tail => sub {$_[0]->tail()},
                                    call => sub {my $self = $_[0]; sub {$_[0]->eval()->call($self->head())->call($self->tail())}},
                                    '<=' => sub {my $self = $_[0]; sub {$self eq $_[0]->eval() ? $true : $false}},
                                    ''   => sub {$_[1]->eval()->call($_[0])}),
                serialize => sub {'(' . $_[0]->head()->serialize() . ' . ' . $_[0]->tail()->serialize() . ')'};

sub cons_from_array {
  return nil() unless @_;
  my $tail = pop @_;
  return cons(cons_from_array(@_), $tail);
}

sub number_op(&) {
  my $implementation = $_[0];
  sub {my $self = $_[0]; sub {number($implementation->($$self, ${$_[0]->eval()}))}}
}

deftype 'number', new       => sub {\$_[0]},
                  eval      => $id,
                  call      => method('<=' => sub {my $self = $_[0]; sub {$$self <= ${$_[0]->eval()} ? $true : $false}},
                                      '$'  => sub {symbol(chr ${$_[0]})},
                                      '+'  => number_op {$_[0]  + $_[1]},
                                      '-'  => number_op {$_[0]  - $_[1]},
                                      '*'  => number_op {$_[0]  * $_[1]},
                                      '/'  => number_op {$_[0]  / $_[1]},
                                      '%'  => number_op {$_[0]  % $_[1]},
                                      '&'  => number_op {$_[0]  & $_[1]},
                                      '|'  => number_op {$_[0]  | $_[1]},
                                      '^'  => number_op {$_[0]  ^ $_[1]},
                                      '<<' => number_op {$_[0] << $_[1]},
                                      '>>' => number_op {$_[0] >> $_[1]},
                                      ''   => sub {return number(${$_[0]} * ${$_[1]}) if ref $_[1] eq 'number';
                                                   return $_[1]->eval()->call($_[0])}),
                  serialize => sub {${$_[0]}};
__q1LSLuHGLAt+1XUTuRfyzIH3/XWigXMQ1PScnZNIjJI

meta::library('reader', <<'__FNIGiO/4HpdvKl1LG4oDoOJ1jQgaaKgwGIBLzZKw+uo');
package reader;

sub read_form {
  sub lex {
    grep length, split /([()])|\s+|("(?:[^"\\]|\\.)*")/so, $_[0];
  }

  sub unquote {
    map {if (s/^"(.*)"$/\1/so) {
           s/\\n/\n/go;
           s/\\r/\r/go;
           s/\\(.)/\1/sgo;
         }
         $_} @_;
  }

  sub parse {
    my ($value, $xs) = @_;
    my $x = shift @$xs;

    return $value                                                    if $x eq ')' || $x eq '';
    return parse(types::cons($value, parse(types::nil(), $xs)), $xs) if $x eq '(';
    return parse(types::cons($value, types::number($x)), $xs)        if $x =~ /^-?[\d.]+$/o;
    return parse(types::cons($value, types::symbol(unquote $x)), $xs);
  }

  parse types::nil(), [lex $_[0]];
}

sub read_paragraph {
  sub flatten_cons_tree {
    return () if ref $_[0] eq 'nil';
    return (flatten_cons_tree($_[0]->head()), $_[0]->tail());
  }

  return () if $_[0] =~ /^\s*[^( ]/o;
  return flatten_cons_tree(read_form $_[0]);
}

sub read_document {
  map {read_paragraph $_} split /\n(?:\s*\n)+/o, $_[0];
}
__FNIGiO/4HpdvKl1LG4oDoOJ1jQgaaKgwGIBLzZKw+uo

meta::library('shell', <<'__sbRiGMNXhqgt65vHRHrGFknAxvnRwNlctXwzHgwFybo');
package shell;

sub definitions           {map reader::read_document(::retrieve("code::$_")), @_}
sub definitions_from_file {map reader::read_document(file::read($_)), @_}

sub initialize_symbol_table {
  types::clear_symbols;
  types::initialize_symbols;
  types::initialize_debugging_symbols;
}

sub make_loader {
  my ($loader) = @_;
  sub {
    for my $document (@_) {
      terminal::message('shell', "loading $document");
      for ($loader->($document)) {
        my $result = eval {$_->eval()};
        chomp $@ and terminal::message('error', "$@ in " . $_->serialize()) if $@;
        terminal::message('loader', $result->serialize()) unless $@;
      }
    }
  };
}

*{'load_interactively'} = make_loader(\&definitions);
*{'load_from_file'}     = make_loader(\&definitions_from_file);

sub load_quietly {map $_->eval()->serialize(), definitions @_}
__sbRiGMNXhqgt65vHRHrGFknAxvnRwNlctXwzHgwFybo

meta::library('terminal', <<'__UqcWaKvw20yAGxzo+r5ZwEssO3nQjhzUp8hYS5n6iIs');
# Functions for nice-looking terminal output.

package terminal;

use constant black  => "0;0";
use constant red    => "1;31";
use constant yellow => "1;33";
use constant green  => "1;32";
use constant blue   => "1;34";
use constant purple => "1;35";
use constant cyan   => "1;36";

my %default_colors = (repl    => green, result  => blue,   error   => red,
                      fail    => red,   warning => yellow, loader  => green,
                      shell   => blue,  trace   => cyan,   eval    => yellow,
                      call    => yellow);

my $longest_prefix = 0;
$longest_prefix = $longest_prefix < $_ ? $_ : $longest_prefix for map length, keys %default_colors;

sub message {
  my ($prefix, $message) = @_;
  my $color = $default_colors{$prefix};
  my $padding = ' ' x ($longest_prefix - length $prefix);
  print "${padding}[\033[${color}m$prefix\033[0;0m] $message\n";
}
__UqcWaKvw20yAGxzo+r5ZwEssO3nQjhzUp8hYS5n6iIs

meta::list('dependencies', <<'__T71KPTYRVSLUowcTp8bm1FsrNDegZe6m3I7uOMxqEnM');
./core
__T71KPTYRVSLUowcTp8bm1FsrNDegZe6m3I7uOMxqEnM

meta::note('code-as-deltas', <<'__TpzZQtxCNif1pAmnxjVcs7qoZiGg5rdl2TNFfz35n3g');
Objects represented as static quantities have fewer composable properties than those represented as generated values. More particularly, if you have the option
of either (1) defining objects statically, or (2) defining objects in terms of a series of differences from some fundamental 'zero' object, the first method
will be less flexible than the second.

As a real-world example of this, using a version control system gives you much more flexibility to manipulate your files than using snapshots, because often the
patches that are generated to carry the source code from one version to another can be split into pieces with partial meaning. The number of distinct types of
patches is quite low, and patterns can be recognized based on parameterized combinations of those fundamental patch types.
__TpzZQtxCNif1pAmnxjVcs7qoZiGg5rdl2TNFfz35n3g

meta::ocaml('prototype', <<'__Lr6ttIXkR6jrNfQTvH8WOop5mW3aBB7JbVdWCr6HFPQ');
OCaml prototype implementation of Gnarly | Spencer Tipping <spencer@spencertipping.com>
Licensed under the terms of the MIT source code license

Interpreter logic. (See the C++ interpreter implementation for the basis of these definitions.)

  open Hashtbl

  type value = Cons of value * value
             | Nil
             | Symbol of int
             | Expander of value * value
             | Ref of value
             | Composition of value * value
             | Application of value * value
             | Continuation of int

  let s_ref    = Symbol 0
  let s_beta   = Symbol 1
  let s_o      = Symbol 2
  let s_cons   = Symbol 3
  let s_h      = Symbol 4
  let s_t      = Symbol 5
  let s_bind   = Symbol 6
  let s_unbind = Symbol 7

  let c_ref    = Continuation 0
  let c_beta   = Continuation 1
  let c_o      = Continuation 2
  let c_cons   = Continuation 3

  let m = Hashtbl.create 100

  let _ = Hashtbl.add m 0 c_ref;
          Hashtbl.add m 1 c_beta;
          Hashtbl.add m 2 c_o;
          Hashtbl.add m 3 c_cons

  let rec eval x = match x with
    | Cons (h, t) -> call (eval h) t
    | Symbol s    -> Hashtbl.find m s
    | Ref x       -> x
    | x           -> x
  and call x y = match (x, y) with
    | (Expander (x', (Cons (h, t))), y')                  -> Cons (call (Expander (x', h)) y', call (Expander (x', t)) y')
    | (Expander (x', x''), y) when x' == x''              -> y
    | (Expander (x', q), y)                               -> q
    | (Composition (f, g), x')                            -> call f (call g x')
    | (Nil, x')                                           -> eval x'
    | (Symbol x', s) when s == s_bind                     -> Application (eval (Symbol x'), s_bind)
    | (Symbol x', s) when s == s_unbind                   -> Hashtbl.remove m x'; Symbol x'
    | (Application ((Symbol x'), s), y') when s == s_bind -> Hashtbl.replace m x' y'; y'
    | (c, x') when c == c_ref                             -> Ref x'
    | (c, x') when c == c_beta                            -> Application (c_beta, x')
    | (c, f)  when c == c_o                               -> Application (c_o, eval f)
    | (c, x') when c == c_cons                            -> Application (c_cons, eval x')
    | (Application (c, x'), y') when c == c_beta          -> Expander (x', y')
    | (Application (c, f), g)   when c == c_o             -> Composition (f, eval g)
    | (Application (c, x'), y') when c == c_cons          -> Cons (x', (eval y'))
    | (Cons (h, t), s) when s == s_h                      -> h
    | (Cons (h, t), s) when s == s_t                      -> t

  let int_to_string x = Int64.to_string (Int64.of_int x)
  let rec serialize x = match x with
    | Cons (h, t)        -> "(" ^ (serialize h) ^ " . " ^ (serialize t) ^ ")"
    | Nil                -> ":"
    | Symbol s           -> "[sym " ^ (int_to_string s) ^ "]"
    | Ref x              -> "[ref " ^ (serialize x) ^ "]"
    | Application (x, y) -> "(" ^ (serialize x) ^ " " ^ (serialize y) ^ " ...)"
    | Expander (x, y)    -> "[" ^ (serialize x) ^ " -> " ^ (serialize y) ^ "]"
    | Composition (x, y) -> "[" ^ (serialize x) ^ " o' " ^ (serialize y) ^ "]"
    | Continuation x     -> "[cont " ^ (int_to_string x) ^ "]"

Perl interop.
Because Perl is pathologically slow at allocating references, a standard interop layer is used to shell out REPL expressions to other programs. The interop layer communicates information both
ways; Perl needs to send the expression to be evaluated along with an image of the environment, and it must receive the result of that expression and a new environment image.

There are a few significant communication barriers here. One is that other environments generally represent symbols as numbers, whereas they should be displayed as strings to the user. Thus
some bidirectional mapping must be maintained to convert back and forth. Luckily, all type constructors are immutable in Gnarly, so referential equality/inequality need not be considered when
converting. (And, in fact, it's safe to referentially unify when equal, potentially yielding some interesting optimizations.)
__Lr6ttIXkR6jrNfQTvH8WOop5mW3aBB7JbVdWCr6HFPQ

meta::vim_highlighter('gnarly', <<'__Hr/Ry8cigp+dlQ8IoAikdVLpXjy0uKgv2jaGcN/Vs0g');
" Gnarly
" Maintainer: Spencer Tipping <spencer@spencertipping.com>
" Language:   A minimalistic combinatory language with rewriting

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

set iskeyword=33-39,42-126,128-255

syn keyword gnarlyDangerous        perl defperl !!< !!> contained
syn keyword gnarlyBuiltins         o' beta placeholder contained
syn keyword gnarlyMethods          bind unbind value head tail contained
syn keyword gnarlyFunctions        gensym counter trace type cons fail serialize : z z> contained
syn keyword gnarlyUserFunctions    ' '' :: o tr fn :< :> :<' :>' ::< ::> # '< '> '>' let en qn def fail$ assert contained
syn keyword gnarlyBooleans         k k' k: k:' contained
syn match   gnarlyNumbers          /\<-\?[0-9.]\+\>/ contained
syn match   gnarlyParens           /[()]/ contained

syn region  gnarlyQuoted           start=/"/ end=/"/ skip=/\\.\|\n/ contained

syn keyword gnarlyBooleanFunctions <= == < > >= != && \|\| ! r== contained
syn keyword gnarlyListFunctions    * / % ++ :? reverse reverse' list cons' $ $'> $* contained
syn keyword gnarlyTypeFunctions    *? /? @? o? #? >? m? p? contained
syn keyword gnarlyConditionals     cond contained

syn cluster gnarlySyntax add=gnarlyBuiltins,gnarlyMethods,gnarlyFunctions,gnarlyUserFunctions,gnarlyNumbers,gnarlyParens,gnarlyBooleans
syn cluster gnarlySyntax add=gnarlyBooleanFunctions,gnarlyListFunctions,gnarlyConditionals,gnarlyQuoted,gnarlyDangerous,gnarlyTypeFunctions

syn region  gnarlyCodeRegion    start=/^\s*(/     end=/^$/ contains=@gnarlySyntax transparent fold
syn region  gnarlyCommentRegion start=/^\s*[^( ]/ end=/^$/ fold

hi link gnarlyBooleans         Boolean
hi link gnarlyBooleanFunctions Operator
hi link gnarlyListFunctions    Operator
hi link gnarlyTypeFunctions    Operator
hi link gnarlyCommentRegion    Comment
hi link gnarlyBuiltins         Keyword
hi link gnarlyFunctions        Keyword
hi link gnarlyConditionals     Keyword
hi link gnarlyMethods          Keyword
hi link gnarlyUserFunctions    Keyword
hi link gnarlyNumbers          Number
hi link gnarlyParens           Special
hi link gnarlyQuoted           String

hi link gnarlyDangerous        Special

set foldmethod=syntax

let b:current_syntax = "gnarly"
__Hr/Ry8cigp+dlQ8IoAikdVLpXjy0uKgv2jaGcN/Vs0g

meta::vim_highlighter('lc', <<'__Og4NtvEdzxsAANLD72RF5q6r0NegrGBtSmHkZV0xp0E');
" Literate C++
" Maintainer: Spencer Tipping <spencer@spencertipping.com>
" Language:   C++ with literate paragraph detection

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn include @cpp syntax/cpp.vim

syn region  lcCodeRegion    start=/^\s*[a-z@#]/   end=/^$/ contains=@cpp fold
syn region  lcCommentRegion start=/^\s*[^a-z@# ]/ end=/^$/ fold

hi link lcCommentRegion Comment

set foldmethod=syntax

let b:current_syntax = "lc"
__Og4NtvEdzxsAANLD72RF5q6r0NegrGBtSmHkZV0xp0E

meta::vim_highlighter('ljava', <<'__yF4ujZVYAP7IPHn10yiwYWEgCcOSjnD1XxAwKp9Nk2U');
" Literate Java
" Maintainer: Spencer Tipping <spencer@spencertipping.com>
" Language:   Java with literate paragraph detection

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn include @java syntax/java.vim

syn region  ljavaCodeRegion    start=/^\s*[a-z]/   end=/^$/ contains=@java fold
syn region  ljavaCommentRegion start=/^\s*[^a-z ]/ end=/^$/ fold

hi link ljavaCommentRegion Comment

set foldmethod=syntax

let b:current_syntax = "ljava"
__yF4ujZVYAP7IPHn10yiwYWEgCcOSjnD1XxAwKp9Nk2U

meta::vim_highlighter('locaml', <<'__9p1ExW6kepCXqYZ/g5F0EQds8SrfJ+gja9SX4dsNZng');
" Literate OCaml
" Maintainer: Spencer Tipping <spencer@spencertipping.com>
" Language:   OCaml with literate paragraph detection

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn include @ocaml syntax/ocaml.vim

syn region  lcCodeRegion    start=/^\s*[a-z@#]/   end=/^$/ contains=@ocaml fold
syn region  lcCommentRegion start=/^\s*[^a-z@# ]/ end=/^$/ fold

hi link lcCommentRegion Comment

set foldmethod=syntax

let b:current_syntax = "locaml"
__9p1ExW6kepCXqYZ/g5F0EQds8SrfJ+gja9SX4dsNZng

meta::internal('runtime', <<'__YPmIzwZkTg8URmPfjiwGRG4VDUF2ZCJqTEz+gjETYLQ');
my $initial_state = sha256_base64 serialize();

push @script_args, shift @ARGV while @ARGV && $ARGV[0] =~ /^-/;

my $default_action = retrieve('data::default-action');
chomp $default_action;
my $function_name = shift(@ARGV) || $default_action || 'usage';
$function_name = 'usage' unless $externalized_functions{$function_name};
my $result = &{$function_name}(@ARGV);
chomp $result;
print "$result\n" if $result;

END {
  my $serialized_data = serialize();
  my $final_state     = sha256_base64 $serialized_data;
  save() unless $initial_state eq $final_state;
}

__YPmIzwZkTg8URmPfjiwGRG4VDUF2ZCJqTEz+gjETYLQ

__END__