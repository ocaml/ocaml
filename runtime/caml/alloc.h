/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_ALLOC_H
#define CAML_ALLOC_H


#include "misc.h"
#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

/* It is guaranteed that these allocation functions will not trigger
   any OCaml callback such as finalizers or signal handlers. */

CAMLextern value caml_alloc (mlsize_t, tag_t);
CAMLextern value caml_alloc_N(mlsize_t, tag_t, ...);
CAMLextern value caml_alloc_1(tag_t, value);
CAMLextern value caml_alloc_2(tag_t, value, value);
CAMLextern value caml_alloc_3(tag_t, value, value, value);
CAMLextern value caml_alloc_4(tag_t, value, value, value, value);
CAMLextern value caml_alloc_5(tag_t, value, value, value, value,
                              value);
CAMLextern value caml_alloc_6(tag_t, value, value, value, value,
                              value, value);
CAMLextern value caml_alloc_7(tag_t, value, value, value, value,
                              value, value, value);
CAMLextern value caml_alloc_8(tag_t, value, value, value, value,
                              value, value, value, value);
CAMLextern value caml_alloc_9(tag_t, value, value, value, value,
                              value, value, value, value, value);
CAMLextern value caml_alloc_small (mlsize_t, tag_t);
CAMLextern value caml_alloc_shr_check_gc (mlsize_t, tag_t);
CAMLextern value caml_alloc_tuple (mlsize_t);
CAMLextern value caml_alloc_float_array (mlsize_t len);
CAMLextern value caml_alloc_string (mlsize_t len);  /* len in bytes (chars) */
CAMLextern value caml_alloc_initialized_string (mlsize_t len, const char *);
CAMLextern value caml_copy_string (char const *);
CAMLextern value caml_copy_string_array (char const * const*);
CAMLextern value caml_copy_double (double);
CAMLextern value caml_copy_int32 (int32_t);       /* defined in [ints.c] */
CAMLextern value caml_copy_int64 (int64_t);       /* defined in [ints.c] */
CAMLextern value caml_copy_nativeint (intnat);  /* defined in [ints.c] */
CAMLextern value caml_alloc_array (value (*funct) (char const *),
                                   char const * const * array);
CAMLextern value caml_alloc_sprintf(const char * format, ...)
#ifdef __GNUC__
  __attribute__ ((format (printf, 1, 2)))
#endif
;
CAMLextern value caml_alloc_some(value);

typedef void (*final_fun)(value);
CAMLextern value caml_alloc_final (mlsize_t, /*size in words*/
                                   final_fun, /*finalization function*/
                                   mlsize_t, /*resources consumed*/
                                   mlsize_t  /*max resources*/);

CAMLextern int caml_convert_flag_list (value, const int *);

/* Convenience functions to deal with unboxable types. */
Caml_inline value caml_alloc_unboxed (value arg) { return arg; }
Caml_inline value caml_alloc_boxed (value arg) {
  value result = caml_alloc_small (1, 0);
  Field (result, 0) = arg;
  return result;
}
Caml_inline value caml_field_unboxed (value arg) { return arg; }
Caml_inline value caml_field_boxed (value arg) { return Field (arg, 0); }

/* Unannotated unboxable types are boxed by default. (may change in the
   future) */
#define caml_alloc_unboxable caml_alloc_boxed
#define caml_field_unboxable caml_field_boxed

#ifdef __cplusplus
}
#endif

#endif /* CAML_ALLOC_H */
