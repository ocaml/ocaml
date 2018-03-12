/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_MINOR_GC_H
#define CAML_MINOR_GC_H

#include "misc.h"
#include "addrmap.h"

struct caml_ref_table {
  value **base;
  value **end;
  value **threshold;
  value **ptr;
  value **limit;
  asize_t size;
  asize_t reserve;
};

struct caml_remembered_set {
  struct caml_ref_table major_ref, minor_ref, fiber_ref;
};

extern void caml_set_minor_heap_size (asize_t); /* size in bytes */
extern void caml_empty_minor_heap (void);
CAMLextern void caml_minor_collection (void);
CAMLextern void forward_pointer (void* domain, value v, value* p);
CAMLextern void garbage_collection (void); /* def in asmrun/signals.c */
extern void caml_realloc_ref_table (struct caml_ref_table *);
struct caml_remembered_set* caml_alloc_remembered_set();
void caml_free_remembered_set(struct caml_remembered_set*);
struct domain;
CAMLextern value caml_promote(struct domain*, value root);
int caml_stack_is_saved (void);

#define Ref_table_add(ref_table, x) do {                                \
    struct caml_ref_table* ref = (ref_table);                           \
    if (ref->ptr >= ref->limit) {                                       \
      CAMLassert (ref->ptr == ref->limit);                              \
      caml_realloc_ref_table (ref);                                     \
    }                                                                   \
    *ref->ptr++ = (x);                                                  \
  } while (0)

#endif /* CAML_MINOR_GC_H */
