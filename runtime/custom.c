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

_Atomic uintnat caml_custom_major_ratio = Custom_major_ratio_def;
_Atomic uintnat caml_custom_minor_ratio = Custom_minor_ratio_def;
_Atomic uintnat caml_custom_minor_max_bsz = Custom_minor_max_bsz_def;

mlsize_t caml_custom_get_max_major (void)
{
  /* The major ratio is a percentage relative to the major heap size.
     A complete GC cycle will be done every time 2/3 of that much
     memory is allocated for blocks in the major heap.  Assuming
     constant allocation and deallocation rates, this means there are
     at most [M/100 * major-heap-size] bytes of floating garbage at
     any time.  The reason for a factor of 2/3 (or 1.5) is, roughly
     speaking, because the major GC takes 1.5 cycles (previous cycle +
     marking phase) before it starts to deallocate dead blocks
     allocated during the previous cycle.  [heap_size / 150] is really
     [heap_size * (2/3) / 100] (but faster). */
  return caml_heap_size(Caml_state->shared_heap) / 150
         * atomic_load_relaxed(&caml_custom_major_ratio);
}

/* [mem] is an amount of out-of-heap resources, in the same units as
   [max_major] and [max_minor]. When the cumulated amount of such
   resources reaches [max_minor] (for resources held by the minor
   heap) we do a minor collection; when it reaches [max_major] (for
   resources held by the major heap), we guarantee that a major cycle
   is done.

   If [max_major] is 0, then [mem] is a number of bytes and the actual
   limit is [caml_custom_get_max_major ()] computed at the
   time when the custom block is promoted to the major heap.
*/
static value alloc_custom_gen (const struct custom_operations * ops,
                               uintnat bsz,
                               mlsize_t mem,
                               mlsize_t max_major,
                               mlsize_t max_minor)
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
      /* Record the extra resources in case the block gets promoted. */
      add_to_custom_table (&Caml_state->minor_tables->custom, result,
                           mem, max_major);
      /* Keep track of extra resources held by custom block in
         minor heap. */
      if (mem != 0) {
        caml_adjust_minor_gc_speed (mem, max_minor);
      }
    }
  } else {
    result = caml_alloc_shr(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
    caml_adjust_gc_speed(mem, max_major);
    result = caml_check_urgent_gc(result);
  }
  CAMLreturn(result);
}

Caml_inline mlsize_t get_max_minor (void)
{
  return
    Bsize_wsize (Caml_state->minor_heap_wsz) / 100
                * atomic_load_relaxed(&caml_custom_minor_ratio);
}

CAMLexport value caml_alloc_custom(const struct custom_operations * ops,
                                   uintnat bsz,
                                   mlsize_t mem,
                                   mlsize_t max)
{
  mlsize_t max_major = max;
  mlsize_t max_minor = max == 0 ? get_max_minor() : max;
  return alloc_custom_gen (ops, bsz, mem, max_major, max_minor);
}

CAMLexport value caml_alloc_custom_mem(const struct custom_operations * ops,
                                       uintnat bsz,
                                       mlsize_t mem)
{
  value v = alloc_custom_gen (ops, bsz, mem, 0, get_max_minor());
  size_t mem_words = (mem + sizeof(value) - 1) / sizeof(value);
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
