/* C stubs for pseudo-terminal operations */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#if defined(__APPLE__) || defined(__linux__) || defined(__unix__)
#include <util.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

CAMLprim value caml_openpty(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  int master_fd, slave_fd;

  if (openpty(&master_fd, &slave_fd, NULL, NULL, NULL) < 0) {
    caml_failwith(strerror(errno));
  }

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(master_fd));
  Store_field(result, 1, Val_int(slave_fd));

  CAMLreturn(result);
}

#else

CAMLprim value caml_openpty(value unit) {
  caml_failwith("openpty not supported on this platform");
}

#endif
