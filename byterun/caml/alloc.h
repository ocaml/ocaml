/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

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
CAMLextern value caml_alloc_small_with_my_or_given_profinfo (mlsize_t wosize,
  tag_t tag, uintnat profinfo);
CAMLextern value caml_alloc_tuple (mlsize_t);
CAMLextern value caml_alloc_string (mlsize_t);  /* size in bytes */
CAMLextern value caml_copy_string (char const *);
CAMLextern value caml_copy_string_array (char const * const*);
CAMLextern value caml_copy_double (double);
CAMLextern value caml_copy_int32 (int32_t);       /* defined in [ints.c] */
CAMLextern value caml_copy_int64 (int64_t);       /* defined in [ints.c] */
CAMLextern value caml_copy_nativeint (intnat);  /* defined in [ints.c] */
CAMLextern value caml_alloc_array (value (*funct) (char const *),
                                   char const * const* array);
CAMLextern value caml_alloc_sprintf(const char * format, ...);

typedef void (*final_fun)(value);
CAMLextern value caml_alloc_final (mlsize_t, /*size in words*/
                                   final_fun, /*finalization function*/
                                   mlsize_t, /*resources consumed*/
                                   mlsize_t  /*max resources*/);

CAMLextern int caml_convert_flag_list (value, const int *);

#ifdef __cplusplus
}
#endif

#endif /* CAML_ALLOC_H */
