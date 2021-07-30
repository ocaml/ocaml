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
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/gc_ctrl.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"
#include "caml/memprof.h"

uintnat caml_custom_major_ratio = Custom_major_ratio_def;
uintnat caml_custom_minor_ratio = Custom_minor_ratio_def;
uintnat caml_custom_minor_max_bsz = Custom_minor_max_bsz_def;

static value alloc_custom_gen (struct custom_operations * ops,
                               uintnat bsz,
                               mlsize_t mem,
                               mlsize_t max_major,
                               mlsize_t mem_minor,
                               mlsize_t max_minor)
{
  mlsize_t wosize;
  CAMLparam0();
  CAMLlocal1(result);

  /* [mem] is the total amount of out-of-heap memory, [mem_minor] is how much
     of it should be counted against [max_minor]. */
  CAMLassert (mem_minor <= mem);

  wosize = 1 + (bsz + sizeof(value) - 1) / sizeof(value);
  if (wosize <= Max_young_wosize) {
    result = caml_alloc_small(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
    if (ops->finalize != NULL || mem != 0) {
      if (mem > mem_minor) {
        caml_adjust_gc_speed (mem - mem_minor, max_major);
      }
      /* The remaining [mem_minor] will be counted if the block survives a
         minor GC */
      add_to_custom_table (Caml_state->custom_table, result,
                           mem_minor, max_major);
      /* Keep track of extra resources held by custom block in
         minor heap. */
      if (mem_minor != 0) {
        if (max_minor == 0) max_minor = 1;
        Caml_state->extra_heap_resources_minor +=
          (double) mem_minor / (double) max_minor;
        if (Caml_state->extra_heap_resources_minor > 1.0)
          caml_minor_collection ();
      }
    }
  } else {
    result = caml_alloc_shr(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
    caml_adjust_gc_speed(mem, max_major);
    caml_check_urgent_gc(Val_unit);
  }
  CAMLreturn(result);
}

CAMLexport value caml_alloc_custom(struct custom_operations * ops,
                                   uintnat bsz,
                                   mlsize_t mem,
                                   mlsize_t max)
{
  return alloc_custom_gen (ops, bsz, mem, max, mem, max);
}

CAMLexport value caml_alloc_custom_mem(struct custom_operations * ops,
                                       uintnat bsz,
                                       mlsize_t mem)
{
  mlsize_t mem_minor =
    mem < caml_custom_minor_max_bsz ? mem : caml_custom_minor_max_bsz;
  mlsize_t max_major =
    /* The major ratio is a percentage relative to the major heap size.
       A complete GC cycle will be done every time 2/3 of that much memory
       is allocated for blocks in the major heap.  Assuming constant
       allocation and deallocation rates, this means there are at most
       [M/100 * major-heap-size] bytes of floating garbage at any time.
       The reason for a factor of 2/3 (or 1.5) is, roughly speaking, because
       the major GC takes 1.5 cycles (previous cycle + marking phase) before
       it starts to deallocate dead blocks allocated during the previous cycle.
       [heap_size / 150] is really [heap_size * (2/3) / 100] (but faster). */
    Bsize_wsize (Caml_state->stat_heap_wsz) / 150 * caml_custom_major_ratio;
  mlsize_t max_minor =
    Bsize_wsize (Caml_state->minor_heap_wsz) / 100 * caml_custom_minor_ratio;
  value v = alloc_custom_gen (ops, bsz, mem, max_major, mem_minor, max_minor);
  caml_memprof_track_custom(v, mem);
  return v;
}

struct custom_operations_list {
  struct custom_operations * ops;
  struct custom_operations_list * next;
};

static struct custom_operations_list * custom_ops_table = NULL;

CAMLexport void caml_register_custom_operations(struct custom_operations * ops)
{
  struct custom_operations_list * l =
    caml_stat_alloc(sizeof(struct custom_operations_list));
  CAMLassert(ops->identifier != NULL);
  CAMLassert(ops->deserialize != NULL);
  l->ops = ops;
  l->next = custom_ops_table;
  custom_ops_table = l;
}

struct custom_operations * caml_find_custom_operations(char * ident)
{
  struct custom_operations_list * l;
  for (l = custom_ops_table; l != NULL; l = l->next)
    if (strcmp(l->ops->identifier, ident) == 0) return l->ops;
  return NULL;
}

static struct custom_operations_list * custom_ops_final_table = NULL;

struct custom_operations * caml_final_custom_operations(final_fun fn)
{
  struct custom_operations_list * l;
  struct custom_operations * ops;
  for (l = custom_ops_final_table; l != NULL; l = l->next)
    if (l->ops->finalize == fn) return l->ops;
  ops = caml_stat_alloc(sizeof(struct custom_operations));
  ops->identifier = "_final";
  ops->finalize = fn;
  ops->compare = custom_compare_default;
  ops->hash = custom_hash_default;
  ops->serialize = custom_serialize_default;
  ops->deserialize = custom_deserialize_default;
  ops->compare_ext = custom_compare_ext_default;
  ops->fixed_length = custom_fixed_length_default;
  l = caml_stat_alloc(sizeof(struct custom_operations_list));
  l->ops = ops;
  l->next = custom_ops_final_table;
  custom_ops_final_table = l;
  return ops;
}

void caml_init_custom_operations(void)
{
  caml_register_custom_operations(&caml_int32_ops);
  caml_register_custom_operations(&caml_nativeint_ops);
  caml_register_custom_operations(&caml_int64_ops);
  caml_register_custom_operations(&caml_ba_ops);
}
