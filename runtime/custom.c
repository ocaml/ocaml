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

#define CAML_INTERNALS

#include <string.h>

#include "caml/alloc.h"
#include "caml/camlatomic.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/gc_ctrl.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/shared_heap.h"
#include "caml/signals.h"
#include "caml/memprof.h"

_Atomic uintnat caml_custom_minor_ratio = Custom_minor_ratio_def;
_Atomic uintnat caml_custom_minor_max_bsz = Custom_minor_max_bsz_def;

/* [mem] is the amount (in words) of off-heap memory held by the custom
   block. It is used for two purposes:
   1. to trigger a minor GC when the amount held by the minor heap gets
      over max_minor
   2. to inform the major GC if/when the block gets promoted.
*/
CAMLexport value caml_alloc_custom (const struct custom_operations * ops,
                                    uintnat bsz,
                                    mlsize_t mem,
                                    mlsize_t unused)
{
  mlsize_t wosize;
  CAMLparam0();
  CAMLlocal1(result);

  wosize = 1 + (bsz + sizeof(value) - 1) / sizeof(value);
  if (wosize <= Max_young_wosize
      && mem <= atomic_load_relaxed(&caml_custom_minor_max_bsz)) {
    result = caml_alloc_small(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
    if (ops->finalize != NULL || mem != 0) {
      /* Record the off-heap size in case the block gets promoted. */
      add_to_custom_table (&Caml_state->minor_tables->custom, result, mem);
      /* Keep track of off-heap memory held by custom block in minor heap. */
      if (mem != 0) {
        caml_adjust_minor_gc_speed (mem, 0);
      }
    }
  } else {
    result = caml_alloc_shr(wosize, Custom_tag);
    caml_domain_state *d = Caml_state;
    Caml_update_major_allocated_words(off_heap, d, mem, 1);
    Custom_ops_val(result) = ops;
    result = caml_check_urgent_gc(result);
  }
  CAMLreturn(result);
}

CAMLexport value caml_alloc_custom_mem(const struct custom_operations * ops,
                                       uintnat bsz,
                                       mlsize_t mem_bytes)
{
  size_t mem_words = (mem_bytes + sizeof(value) - 1) / sizeof(value);
  value v = caml_alloc_custom (ops, bsz, mem_words, 0);
  caml_memprof_sample_block(v, mem_words, mem_words, CAML_MEMPROF_SRC_CUSTOM);
  return v;
}

struct custom_operations_list {
  const struct custom_operations * ops;
  struct custom_operations_list * next;
};

typedef _Atomic(struct custom_operations_list *) custom_operations_table;

/* Thread-safety: the tables are append-only lists, hence we only need
   a CAS loop update them. */
static void push_custom_ops(custom_operations_table * table,
                            const struct custom_operations * ops)
{
  struct custom_operations_list * l =
    caml_stat_alloc(sizeof(struct custom_operations_list));
  l->ops = ops;
  struct custom_operations_list * prev = atomic_load(table);
  do {
    l->next = prev;
  } while (!atomic_compare_exchange_weak(table, &prev, l));
}

static custom_operations_table custom_ops_table = NULL;

CAMLexport void
caml_register_custom_operations(const struct custom_operations * ops)
{
  CAMLassert(ops->identifier != NULL);
  CAMLassert(ops->deserialize != NULL);
  push_custom_ops(&custom_ops_table, ops);
}

struct custom_operations * caml_find_custom_operations(const char * ident)
{
  for (struct custom_operations_list *l = atomic_load(&custom_ops_table);
       l != NULL;
       l = l->next)
    if (strcmp(l->ops->identifier, ident) == 0)
      return (struct custom_operations*)l->ops;
  return NULL;
}

static custom_operations_table custom_ops_final_table = NULL;

struct custom_operations * caml_final_custom_operations(final_fun fn)
{
  struct custom_operations * ops;
  for (struct custom_operations_list *l = atomic_load(&custom_ops_final_table);
       l != NULL;
       l = l->next)
    if (l->ops->finalize == fn) return (struct custom_operations*)l->ops;
  ops = caml_stat_alloc(sizeof(struct custom_operations));
  ops->identifier = "_final";
  ops->finalize = fn;
  ops->compare = custom_compare_default;
  ops->hash = custom_hash_default;
  ops->serialize = custom_serialize_default;
  ops->deserialize = custom_deserialize_default;
  ops->compare_ext = custom_compare_ext_default;
  ops->fixed_length = custom_fixed_length_default;
  push_custom_ops(&custom_ops_final_table, ops);
  return ops;
}

void caml_init_custom_operations(void)
{
  caml_register_custom_operations(&caml_int32_ops);
  caml_register_custom_operations(&caml_nativeint_ops);
  caml_register_custom_operations(&caml_int64_ops);
  caml_register_custom_operations(&caml_ba_ops);
}
