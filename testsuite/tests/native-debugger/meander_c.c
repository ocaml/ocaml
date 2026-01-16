#include <caml/mlvalues.h>
#include <caml/callback.h>

value ocaml_to_c (value unit) {
    caml_callback(*caml_named_value
                  ("c_to_ocaml"), Val_unit);
    return Val_int(0);
}
