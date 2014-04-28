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

CAMLexport struct caml__roots_block *caml_local_roots = NULL;

/* FIXME should rename to [caml_oldify_young_roots] and synchronise with
   asmrun/roots.c */
/* Call [caml_oldify_one] on (at least) all the roots that point to the minor
   heap. */
void caml_oldify_local_roots (void)
{
  register value * sp;
  struct caml__roots_block *lr;
  intnat i, j;

  /* The stack */
  for (sp = caml_extern_sp; sp < caml_stack_high; sp++) {
    caml_oldify_one (*sp, sp);
  }
  /* Local C roots */  /* FIXME do the old-frame trick ? */
  for (lr = caml_local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        if (*sp != 0) {
          caml_oldify_one (*sp, sp);
        }
      }
    }
  }
}

void caml_do_roots (scanning_action f)
{
  /* The stack and the local C roots */
  caml_do_local_roots(f, caml_extern_sp, caml_stack_high, caml_local_roots);
  /* Global C roots */
  caml_scan_global_roots(f);
}

CAMLexport void caml_do_local_roots (scanning_action f, value *stack_low,
                                     value *stack_high,
                                     struct caml__roots_block *local_roots)
{
  register value * sp;
  struct caml__roots_block *lr;
  int i, j;

  for (sp = stack_low; sp < stack_high; sp++) {
    f (*sp, sp);
  }
  for (lr = local_roots; lr != NULL; lr = lr->next) {
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
