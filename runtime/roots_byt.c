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

#include "caml/codefrag.h"
#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/stacks.h"
#include "caml/memprof.h"
#include "caml/eventlog.h"

CAMLexport void (*caml_scan_roots_hook) (scanning_action f) = NULL;

/* FIXME should rename to [caml_oldify_minor_roots] and synchronise with
   roots_nat.c */
/* Call [caml_oldify_one] on (at least) all the roots that point to the minor
   heap. */
void caml_oldify_local_roots (void)
{
  register value * sp;
  struct caml__roots_block *lr;
  intnat i, j;

  /* The stack */
  /* [caml_oldify_one] acts only on pointers into the minor heap.
     So, it is safe to pass code pointers to [caml_oldify_one],
     even in no-naked-pointers mode */
  for (sp = Caml_state->extern_sp; sp < Caml_state->stack_high; sp++) {
    caml_oldify_one (*sp, sp);
  }
  /* Local C roots */  /* FIXME do the old-frame trick ? */
  for (lr = Caml_state->local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        caml_oldify_one (*sp, sp);
      }
    }
  }
  /* Global C roots */
  caml_scan_global_young_roots(&caml_oldify_one);
  /* Finalised values */
  caml_final_oldify_young_roots ();
  /* Memprof */
  caml_memprof_oldify_young_roots ();
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(&caml_oldify_one);
}

/* Call [caml_darken] on all roots */

void caml_darken_all_roots_start (void)
{
  caml_do_roots (caml_darken, 1);
}

uintnat caml_incremental_roots_count = 1;

intnat caml_darken_all_roots_slice (intnat work)
{
  return work;
}

/* Note, in byte-code there is only one global root, so [do_globals] is
   ignored and [caml_darken_all_roots_slice] does nothing. */
void caml_do_roots (scanning_action f, int do_globals)
{
  /* Global variables */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_GLOBAL);
  f(caml_global_data, &caml_global_data);
  CAML_EV_END(EV_MAJOR_ROOTS_GLOBAL);
  /* The stack and the local C roots */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_LOCAL);
  caml_do_local_roots_byt(f, Caml_state->extern_sp, Caml_state->stack_high,
                          Caml_state->local_roots);
  CAML_EV_END(EV_MAJOR_ROOTS_LOCAL);
  /* Global C roots */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_C);
  caml_scan_global_roots(f);
  CAML_EV_END(EV_MAJOR_ROOTS_C);
  /* Finalised values */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_FINALISED);
  caml_final_do_roots (f);
  CAML_EV_END(EV_MAJOR_ROOTS_FINALISED);
  /* Memprof */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_MEMPROF);
  caml_memprof_do_roots (f);
  CAML_EV_END(EV_MAJOR_ROOTS_MEMPROF);
  /* Hook */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_HOOK);
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(f);
  CAML_EV_END(EV_MAJOR_ROOTS_HOOK);
}

CAMLexport void caml_do_local_roots_byt (scanning_action f, value *stack_low,
                                         value *stack_high,
                                         struct caml__roots_block *local_roots)
{
  register value * sp;
  struct caml__roots_block *lr;
  int i, j;

  for (sp = stack_low; sp < stack_high; sp++) {
#ifdef NO_NAKED_POINTERS
    /* Code pointers inside the stack are naked pointers.
       We must avoid passing them to function [f]. */
    value v = *sp;
    if (Is_block(v) && caml_find_code_fragment_by_pc((char *) v) == NULL) {
      f(v, sp);
    }
#else
    f (*sp, sp);
#endif
  }
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        f (*sp, sp);
      }
    }
  }
}
