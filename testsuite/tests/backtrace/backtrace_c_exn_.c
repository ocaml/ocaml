#include <caml/callback.h>
#include <caml/memory.h>

void caml_498_raise(void) {
    CAMLparam0 ();
    const value *cl;

    cl = caml_named_value("test_raise_exn");

    if (cl != NULL)
      caml_callback(*cl, Val_unit);

    CAMLreturn0;
}
