#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include <string.h>

typedef uintnat unativeint_t;
#define caml_copy_int16 Val_int
#define caml_copy_int8 Val_int

#define LOADPRIM(type) \
  do {\
    CAMLparam1(ptr);\
    u##type##_t res;\
    memcpy(&res, (void*)Nativeint_val(ptr), sizeof res);\
    CAMLreturn(caml_copy_##type(res));\
  } while (0)

CAMLprim value caml_load_int8(value ptr) {
  LOADPRIM(int8);
}

CAMLprim value caml_load_int16(value ptr) {
  LOADPRIM(int16);
}

CAMLprim value caml_load_int32(value ptr) {
  LOADPRIM(int32);
}

CAMLprim value caml_load_int64(value ptr) {
  LOADPRIM(int64);
}

CAMLprim value caml_load_nativeint(value ptr) {
  LOADPRIM(nativeint);
}

#undef LOADPATH
