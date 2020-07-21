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

#ifdef NATIVE_CODE
#include "caml/stack.h"
/* Communication with [caml_start_program] and [caml_call_gc]. */

/* The global roots.
   FIXME: These should be promoted, and not scanned here.
   FIXME: caml_globals_inited makes assumptions about store ordering.
   XXX KC : What to do here?
*/

intnat caml_globals_inited = 0;
#endif

CAMLexport void (*caml_scan_roots_hook)(scanning_action, void* fdata, struct domain*) = NULL;

void caml_do_roots (scanning_action f, void* fdata, struct domain* d, int do_final_val)
{
  f (fdata, d->state->dls_root, &d->state->dls_root);
  caml_do_local_roots(f, fdata, d, do_final_val);
  caml_scan_stack(f, fdata, d->state->current_stack);
  caml_scan_global_roots(f, fdata);
  caml_final_do_roots(f, fdata, d, do_final_val);
}

CAMLexport void caml_do_local_roots (scanning_action f, void* fdata,
                                     struct domain* domain, int do_final_val)
{
  struct caml__roots_block *lr;
  int i, j;
  value* sp;

  for (lr = domain->state->local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        if (*sp != 0) {
          f (fdata, *sp, sp);
        }
      }
    }
  }
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(f, fdata, domain);
}
