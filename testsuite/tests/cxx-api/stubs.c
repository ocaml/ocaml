#ifndef __cplusplus
#error "A C++ compiler was expected!"
#endif

// Generate all-includes.h with:
//
//   find runtime/caml otherlibs/*/caml       \
//     -name '*.h' -not -name 'jumptbl.h'     \
//     -execdir echo '#include <caml/{}>' ';' \
//   > testsuite/tests/cxx-api/all-includes.h
//
//   FIXME: Could the list be generated automatically?

#include "all-includes.h"

extern "C" {
value test_cxx(value);
}

value test_cxx(value vunit) {
  CAMLparam0();
  CAMLlocal1(pair);
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, Val_int(42));
  Store_field(pair, 1, Val_int(1337));
  CAMLreturn(pair);
}
