/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Manuel Serrano and Xavier Leroy, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_CUSTOM_H
#define CAML_CUSTOM_H


#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "mlvalues.h"

struct custom_operations {
  char *identifier;
  void (*finalize)(value v);
  int (*compare)(value v1, value v2);
  intnat (*hash)(value v);
  void (*serialize)(value v,
                    /*out*/ uintnat * bsize_32 /*size in bytes*/,
                    /*out*/ uintnat * bsize_64 /*size in bytes*/);
  uintnat (*deserialize)(void * dst);
  int (*compare_ext)(value v1, value v2);
};

#define custom_finalize_default NULL
#define custom_compare_default NULL
#define custom_hash_default NULL
#define custom_serialize_default NULL
#define custom_deserialize_default NULL
#define custom_compare_ext_default NULL

#define Custom_ops_val(v) (*((struct custom_operations **) (v)))

/* Given a pointer [p] to the data part of a custom block
   (as returned by [Data_custom_val]), return the size of this data part
   (in words or in bytes). */
#define Wsize_custom_data(p) (Wosize_hp((header_t *)(p) - 2) - 1)
#define Bsize_custom_data(p) (Bsize_wsize(Wsize_custom_data(p)))

#ifdef __cplusplus
extern "C" {
#endif


CAMLextern value caml_alloc_custom(struct custom_operations * ops,
                                   uintnat size, /*size in bytes*/
                                   mlsize_t mem, /*resources consumed*/
                                   mlsize_t max  /*max resources*/);

CAMLextern void caml_register_custom_operations(struct custom_operations * ops);

CAMLextern int caml_compare_unordered;
  /* Used by custom comparison to report unordered NaN-like cases. */

#ifdef CAML_INTERNALS
extern struct custom_operations * caml_find_custom_operations(char * ident);
extern struct custom_operations *
          caml_final_custom_operations(void (*fn)(value));

extern void caml_init_custom_operations(void);
#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_CUSTOM_H */
