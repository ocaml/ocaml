/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* To walk the memory roots for garbage collection */

#include "finalise.h"
#include "globroots.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "stacks.h"
#include "major_gc.h"
#include "shared_heap.h"

CAMLexport __thread struct caml__roots_block *caml_local_roots = NULL;

void caml_sample_local_roots(struct caml_sampled_roots* r)
{
  r->stack_low = caml_extern_sp;
  r->stack_high = caml_stack_high;
  r->local_roots = caml_local_roots;
  r->young_ptr = (value*)caml_young_ptr;
  r->young_end = (value*)caml_young_end;
  r->mark_stack = caml_mark_stack;
  r->mark_stack_count = caml_mark_stack_count;
  r->promotion_table = &caml_promotion_table;
  r->promotion_rev_table = &caml_promotion_rev_table;
  r->shared_heap = caml_shared_heap;
}

CAMLexport void caml_do_local_roots (scanning_action f, struct caml_sampled_roots* r)
{
  register value * sp;
  struct caml__roots_block *lr;
  int i, j;

  for (sp = r->stack_low; sp < r->stack_high; sp++) {
    f (*sp, sp);
  }
  for (lr = r->local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        if (*sp != 0) {
          f (*sp, sp);
        }
      }
    }
  }
}

void caml_do_sampled_roots(scanning_action f, struct caml_sampled_roots* r)
{
  /* look for roots on the minor heap */
  value* p = r->young_ptr;
  while (p < r->young_end) {
    value v = Val_hp(p);
    Assert (Is_block(v) && Wosize_val(v) <= Max_young_wosize);
    if (Tag_val(v) < No_scan_tag) {
      int i;
      value* fields = Op_val(v);
      for (i = 0; i < Wosize_val(v); i++) {
        if (Is_block(fields[i]) && !Is_minor(fields[i])) f(fields[i], &fields[i]);
      }
    }
    p += Whsize_wosize(Wosize_val(v));
  }
  Assert(p == r->young_end);

  /* look for gray values in the mark stack */
  for (p = r->mark_stack; p < r->mark_stack + r->mark_stack_count; p++) {
    value v = *p;
    Assert (Is_block(v));
    if (Tag_val(v) < No_scan_tag) {
      int i;
      value* fields = Op_val(v);
      for (i = 0; i < Wosize_val(v); i++) {
        if (Is_block(fields[i]) && !Is_minor(fields[i])) f(fields[i], &fields[i]);
      }
    }
  }

  /* look for local C roots */
  caml_do_local_roots(f, r);
}
