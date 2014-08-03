/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#ifndef CAML_MINOR_GC_H
#define CAML_MINOR_GC_H


#include "misc.h"
#include "minor_heap.h"

CAMLextern __thread char *caml_young_ptr;
extern __thread asize_t caml_minor_heap_size;

struct caml_ref_table {
  value **base;
  value **end;
  value **threshold;
  value **ptr;
  value **limit;
  asize_t size;
  asize_t reserve;
};
CAMLextern __thread struct caml_ref_table caml_ref_table, caml_weak_ref_table;
CAMLextern __thread struct addrmap caml_promotion_table, caml_promotion_rev_table;

extern void caml_set_minor_heap_size (asize_t); /* size in bytes */
extern void caml_empty_minor_heap (void);
CAMLextern void caml_minor_collection (void);
CAMLextern void garbage_collection (void); /* def in asmrun/signals.c */
extern void caml_realloc_ref_table (struct caml_ref_table *);
extern void caml_alloc_table (struct caml_ref_table *, asize_t, asize_t);
CAMLextern value caml_promote(struct domain*, value root);

#define Oldify(p) do{ \
    value __oldify__v__ = *p; \
    if (Is_block (__oldify__v__) && Is_young (__oldify__v__)){ \
      caml_oldify_one (__oldify__v__, (p)); \
    } \
  }while(0)

#endif /* CAML_MINOR_GC_H */
