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

uintnat caml_custom_major_ratio = 30;
uintnat caml_custom_minor_ratio = 100;
uintnat caml_custom_minor_max_bsz = 8192;

static value alloc_custom_gen (struct custom_operations * ops,
                               uintnat bsz,
                               mlsize_t mem,
                               mlsize_t max_major,
                               mlsize_t max_minor,
                               int long_lived)
{
  mlsize_t wosize;
  CAMLparam0();
  CAMLlocal1(result);

  wosize = 1 + (bsz + sizeof(value) - 1) / sizeof(value);
  if (wosize <= Max_young_wosize) {
    result = caml_alloc_small(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
    if (ops->finalize != NULL || mem != 0) {
      if (long_lived){
        caml_adjust_gc_speed (mem, max_major);
      }else{
        /* Remember that the block needs processing after minor GC. */
        add_to_custom_table (&caml_custom_table, result, mem, max_major);
        /* Keep track of extra resources held by custom block in
           minor heap. */
        if (mem != 0) {
          if (max_minor == 0) max_minor = 1;
          caml_extra_heap_resources_minor += (double) mem / (double) max_minor;
          if (caml_extra_heap_resources_minor > 1.0) {
            caml_request_minor_gc ();
            caml_gc_dispatch ();
          }
        }
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

CAMLexport value caml_alloc_custom(struct custom_operations * ops,
                                   uintnat bsz,
                                   mlsize_t mem,
                                   mlsize_t max)
{
  return alloc_custom_gen (ops, bsz, mem, max, max, 0);
}

CAMLexport value caml_alloc_custom_mem(struct custom_operations * ops,
                                       uintnat bsz,
                                       mlsize_t mem)
{
  return alloc_custom_gen (ops, bsz, mem,
                           Bsize_wsize (caml_stat_heap_wsz) / 150
                           * caml_custom_major_ratio,
                           Bsize_wsize (caml_minor_heap_wsz) / 100
                           * caml_custom_major_ratio,
                           mem >= caml_custom_minor_max_bsz);
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

extern struct custom_operations caml_int32_ops,
                                caml_nativeint_ops,
                                caml_int64_ops,
                                caml_ba_ops;

void caml_init_custom_operations(void)
{
  caml_register_custom_operations(&caml_int32_ops);
  caml_register_custom_operations(&caml_nativeint_ops);
  caml_register_custom_operations(&caml_int64_ops);
  caml_register_custom_operations(&caml_ba_ops);
}
