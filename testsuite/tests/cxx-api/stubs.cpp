#ifndef __cplusplus
#error "A C++ compiler is required!"
#endif

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <ctime>

extern "C" {
  CAMLprim value test_cxx(value);
}

value test_cxx(value vunit)
{
  CAMLparam1(vunit);
  CAMLlocal1(str);
  time_t t;

  caml_release_runtime_system();
  t = time(NULL);
  caml_acquire_runtime_system();

  str = caml_copy_string(t != (time_t)-1 ? "ok\n" : "ko\n");

  CAMLreturn(str);
}
