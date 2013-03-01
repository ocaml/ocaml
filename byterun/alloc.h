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

/* $Id$ */

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
CAMLextern value caml_alloc_small (mlsize_t, tag_t);
CAMLextern value caml_alloc_tuple (mlsize_t);
CAMLextern value caml_alloc_string (mlsize_t);  /* size in bytes */
CAMLextern value caml_alloc_string_loc (mlsize_t, profiling_t);  /* size in bytes */
CAMLextern value caml_copy_string (char const *);
CAMLextern value caml_copy_string_loc (char const *, profiling_t);
CAMLextern value caml_copy_string_array (char const **);
CAMLextern value caml_copy_double (double);
CAMLextern value caml_copy_int32 (int32);       /* defined in [ints.c] */
CAMLextern value caml_copy_int64 (int64);       /* defined in [ints.c] */
CAMLextern value caml_copy_nativeint (intnat);  /* defined in [ints.c] */
CAMLextern value caml_alloc_array (value (*funct) (char const *),
                                   char const ** array);

typedef void (*final_fun)(value);
CAMLextern value caml_alloc_final (mlsize_t, /*size in words*/
                                   final_fun, /*finalization function*/
                                   mlsize_t, /*resources consumed*/
                                   mlsize_t  /*max resources*/);

CAMLextern int caml_convert_flag_list (value, int *);

/* CAGO: add extra arg for unique identifier in header */
CAMLextern value caml_alloc_loc (mlsize_t, tag_t, profiling_t);
CAMLexport value caml_alloc_small_loc (mlsize_t, tag_t, profiling_t);
CAMLextern value caml_alloc_tuple_loc (mlsize_t, profiling_t);
CAMLextern value caml_alloc_final_loc (mlsize_t, /*size in words*/
                                   final_fun, /*finalization function*/
                                   mlsize_t, /*resources consumed*/
				   mlsize_t  /*max resources*/,
				   profiling_t);


#ifdef __cplusplus
}
#endif

#endif /* CAML_ALLOC_H */
