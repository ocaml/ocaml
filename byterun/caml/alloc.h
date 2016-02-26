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

CAMLextern value caml_alloc (mlsize_t wosize, tag_t);
CAMLextern value caml_alloc_small (mlsize_t wosize, tag_t);
CAMLextern value caml_alloc_tuple (mlsize_t wosize);
CAMLextern value caml_alloc_string (mlsize_t len);  /* len in bytes (chars) */
CAMLextern value caml_copy_string (char const *);
CAMLextern value caml_copy_string_array (char const **);
CAMLextern value caml_copy_double (double);
CAMLextern value caml_copy_int32 (int32_t);       /* defined in [ints.c] */
CAMLextern value caml_copy_int64 (int64_t);       /* defined in [ints.c] */
CAMLextern value caml_copy_nativeint (intnat);  /* defined in [ints.c] */
CAMLextern value caml_alloc_array (value (*funct) (char const *),
                                   char const ** array);
CAMLextern value caml_alloc_sprintf(const char * format, ...);

typedef void (*final_fun)(value);
CAMLextern value caml_alloc_final (mlsize_t wosize,
                                   final_fun, /*finalization function*/
                                   mlsize_t, /*resources consumed*/
                                   mlsize_t  /*max resources*/);

CAMLextern int caml_convert_flag_list (value, int *);

#ifdef __cplusplus
}
#endif

#endif /* CAML_ALLOC_H */
