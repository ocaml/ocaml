/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* To walk the memory roots for garbage collection */

#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/major_gc.h"
#include "caml/shared_heap.h"
#include "caml/fiber.h"

CAMLexport _Atomic scan_roots_hook caml_scan_roots_hook =
  (scan_roots_hook)NULL;

void caml_do_roots (
  scanning_action f, scanning_action_flags fflags, void* fdata,
  caml_domain_state* d,
  int do_final_val)
{
  scan_roots_hook hook;
  caml_do_local_roots(f, fflags, fdata,
                      d->local_roots, d->current_stack, d->gc_regs);
  hook = atomic_load(&caml_scan_roots_hook);
  if (hook != NULL) (*hook)(f, fflags, fdata, d);
  caml_final_do_roots(f, fflags, fdata, d, do_final_val);

}

CAMLexport void caml_do_local_roots (
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct caml__roots_block *local_roots,
  struct stack_info *current_stack,
  value * v_gc_regs)
{
  struct caml__roots_block *lr;
  int i, j;
  value* sp;

  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        if (*sp != 0) {
          f (fdata, *sp, sp);
        }
      }
    }
  }
  caml_scan_stack(f, fflags, fdata, current_stack, v_gc_regs);
}
