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
#include "config.h"

#define CAML_TABLE_STRUCT(t) { \
  t *base;                     \
  t *end;                      \
  t *threshold;                \
  t *ptr;                      \
  t *limit;                    \
  asize_t size;                \
  asize_t reserve;             \
}

struct caml_ref_table CAML_TABLE_STRUCT(value *);

struct caml_ephe_ref_elt {
  value ephe;      /* an ephemeron in major heap */
  mlsize_t offset; /* the offset that points in the minor heap  */
};
struct caml_ephe_ref_table CAML_TABLE_STRUCT(struct caml_ephe_ref_elt);

struct caml_custom_elt {
  value block;     /* The finalized block in the minor heap. */
  mlsize_t mem;    /* The parameters for adjusting GC speed. */
  mlsize_t max;
};
struct caml_custom_table CAML_TABLE_STRUCT(struct caml_custom_elt);

struct caml_minor_tables {
  struct caml_ref_table major_ref;
  struct caml_ephe_ref_table ephe_ref;
  struct caml_custom_table custom;
};

struct domain;

extern void caml_set_minor_heap_size (asize_t); /* size in bytes */
extern void caml_empty_minor_heap_no_major_slice_from_stw (struct domain* domain, void* unused, int participating_count, struct domain** participating); /* in STW */
extern int caml_try_stw_empty_minor_heap_on_all_domains(); /* out STW */
extern void caml_empty_minor_heaps_once(); /* out STW */
CAMLextern void caml_minor_collection (void);
CAMLextern void garbage_collection (void); /* def in asmrun/signals.c */
header_t caml_get_header_val(value v);
void caml_alloc_table (struct caml_ref_table *tbl, asize_t sz, asize_t rsv);
extern void caml_realloc_ref_table (struct caml_ref_table *);
extern void caml_realloc_ephe_ref_table (struct caml_ephe_ref_table *);
extern void caml_realloc_custom_table (struct caml_custom_table *);
struct caml_minor_tables* caml_alloc_minor_tables();
void caml_free_minor_tables(struct caml_minor_tables*);
void caml_empty_minor_heap_setup(struct domain* domain);

#ifdef DEBUG
extern int caml_debug_is_minor(value val);
extern int caml_debug_is_major(value val);
#endif

#define Ref_table_add(ref_table, x) do {                                \
    struct caml_ref_table* ref = (ref_table);                           \
    if (ref->ptr >= ref->limit) {                                       \
      CAMLassert (ref->ptr == ref->limit);                              \
      caml_realloc_ref_table (ref);                                     \
    }                                                                   \
    *ref->ptr++ = (x);                                                  \
  } while (0)

static inline void add_to_ephe_ref_table (struct caml_ephe_ref_table *tbl,
                                          value ar, mlsize_t offset)
{
  struct caml_ephe_ref_elt *ephe_ref;
  if (tbl->ptr >= tbl->limit){
    CAMLassert (tbl->ptr == tbl->limit);
    caml_realloc_ephe_ref_table (tbl);
  }
  ephe_ref = tbl->ptr++;
  ephe_ref->ephe = ar;
  ephe_ref->offset = offset;
  CAMLassert(ephe_ref->offset < Wosize_val(ephe_ref->ephe));
}

static inline void add_to_custom_table (struct caml_custom_table *tbl, value v,
                                        mlsize_t mem, mlsize_t max)
{
  struct caml_custom_elt *elt;
  if (tbl->ptr >= tbl->limit){
    CAMLassert (tbl->ptr == tbl->limit);
    caml_realloc_custom_table (tbl);
  }
  elt = tbl->ptr++;
  elt->block = v;
  elt->mem = mem;
  elt->max = max;
}

#endif /* CAML_MINOR_GC_H */
