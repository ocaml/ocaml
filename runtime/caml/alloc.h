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


#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern caml_value caml_alloc (mlsize_t wosize, tag_t);
CAMLextern caml_value caml_alloc_small (mlsize_t wosize, tag_t);
CAMLextern caml_value caml_alloc_tuple (mlsize_t wosize);
CAMLextern caml_value caml_alloc_float_array (mlsize_t len);
CAMLextern caml_value caml_alloc_string (mlsize_t len);  /* len in bytes (chars) */
CAMLextern caml_value caml_alloc_initialized_string (mlsize_t len, const char *);
CAMLextern caml_value caml_copy_string (char const *);
CAMLextern caml_value caml_copy_string_array (char const **);
CAMLextern caml_value caml_copy_double (double);
CAMLextern caml_value caml_copy_int32 (int32_t);       /* defined in [ints.c] */
CAMLextern caml_value caml_copy_int64 (int64_t);       /* defined in [ints.c] */
CAMLextern caml_value caml_copy_nativeint (intnat);  /* defined in [ints.c] */
CAMLextern caml_value caml_alloc_array (caml_value (*funct) (char const *),
                                   char const ** array);
CAMLextern caml_value caml_alloc_sprintf(const char * format, ...)
#ifdef __GNUC__
  __attribute__ ((format (printf, 1, 2)))
#endif
;

CAMLextern caml_value caml_alloc_with_profinfo (mlsize_t, tag_t, intnat);
CAMLextern caml_value caml_alloc_small_with_my_or_given_profinfo (
  mlsize_t, tag_t, uintnat);
CAMLextern caml_value caml_alloc_small_with_profinfo (mlsize_t, tag_t, intnat);

typedef void (*final_fun)(caml_value);
CAMLextern caml_value caml_alloc_final (mlsize_t wosize,
                                   final_fun, /*finalization function*/
                                   mlsize_t, /*resources consumed*/
                                   mlsize_t  /*max resources*/);

CAMLextern int caml_convert_flag_list (caml_value, int *);

/* Convenience functions to deal with unboxable types. */
static inline caml_value caml_alloc_unboxed (caml_value arg) { return arg; }
static inline caml_value caml_alloc_boxed (caml_value arg) {
  caml_value result = caml_alloc_small (1, 0);
  Field (result, 0) = arg;
  return result;
}
static inline caml_value caml_field_unboxed (caml_value arg) { return arg; }
static inline caml_value caml_field_boxed (caml_value arg) { return Field (arg, 0); }

/* Unannotated unboxable types are boxed by default. (may change in the
   future) */
#define caml_alloc_unboxable caml_alloc_boxed
#define caml_field_unboxable caml_field_boxed

#ifdef __cplusplus
}
#endif

#endif /* CAML_ALLOC_H */
