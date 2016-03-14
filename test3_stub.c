#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <stdio.h>
#include <caml/callback.h>

CAMLprim value test3_stub(value v)
{
  static value* closure = NULL;
  if (closure == NULL) closure = caml_named_value("called_from_c");
  printf("C about to call OCaml\n");
  fflush(stdout);
  printf("C allocating a string\n");
  (void) caml_copy_string("foobar");
  return caml_callback(*closure, v);
}
