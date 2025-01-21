// Generate all-includes.h with:
//
//   find runtime/caml otherlibs/*/caml       \
//     -name '*.h' -not -name 'jumptbl.h'     \
//     -execdir echo '#include <caml/{}>' ';' \
//   > testsuite/tests/cxx-api/all-includes.h
//
//   FIXME: Could the list be generated automatically?

#ifndef __cplusplus
#error "A C++ compiler is required!"
#endif

#include "all-includes.h"

extern "C" {
  CAMLprim value test_cxx(value);
}

value test_cxx(value vunit)
{
  CAMLparam0();
  CAMLlocal1(str);
  str = caml_copy_string("ok\n");
  CAMLreturn(str);
}
