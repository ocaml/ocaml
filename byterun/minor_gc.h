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
#include "addrmap.h"

CAMLextern __thread char *caml_young_ptr;
extern __thread asize_t caml_minor_heap_size;

struct caml_ref_entry {
  value obj;
  intnat field;
};

struct caml_ref_table {
  struct caml_ref_entry *base;
  struct caml_ref_entry *end;
  struct caml_ref_entry *threshold;
  struct caml_ref_entry *ptr;
  struct caml_ref_entry *limit;
  asize_t size;
  asize_t reserve;
};

struct caml_remembered_set {
  struct caml_ref_table ref, fiber_ref;
  struct addrmap promotion, promotion_rev;
};
CAMLextern __thread struct caml_remembered_set caml_remembered_set;

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

#define Ref_table_add(ref_table, x, f) do {                             \
    struct caml_ref_table* ref = (ref_table);                           \
    if (ref->ptr >= ref->limit) {                                       \
      CAMLassert (ref->ptr == ref->limit);                              \
      caml_realloc_ref_table (ref);                                     \
    }                                                                   \
    ref->ptr->obj = (x);                                                \
    ref->ptr->field = (f);                                              \
    ref->ptr++;                                                         \
  } while (0)


#endif /* CAML_MINOR_GC_H */
