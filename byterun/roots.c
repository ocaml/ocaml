/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

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

CAMLexport struct caml__roots_block *local_roots = NULL;

void (*scan_roots_hook) (scanning_action f) = NULL;

/* FIXME rename to [oldify_young_roots] and synchronise with asmrun/roots.c */
/* Call [oldify] on (at least) all the roots that point to the minor heap. */
void oldify_local_roots (void)
{
  register value * sp;
  struct global_root * gr;
  struct caml__roots_block *lr;
  long i, j;

  /* The stack */
  for (sp = extern_sp; sp < stack_high; sp++) {
    oldify (*sp, sp);
  }
  /* Local C roots */  /* FIXME do the old-frame trick ? */
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        oldify (*sp, sp);
      }
    }
  }
  /* Global C roots */
  for (gr = caml_global_roots.forward[0]; gr != NULL; gr = gr->forward[0]) {
    oldify(*(gr->root), gr->root);
  }
  /* Finalised values */
  final_do_young_roots (&oldify);
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(&oldify);
}

/* Call [darken] on all roots */

void darken_all_roots (void)
{
  do_roots (darken);
}

void do_roots (scanning_action f)
{
  struct global_root * gr;

  /* Global variables */
  f(global_data, &global_data);

  /* The stack and the local C roots */
  do_local_roots(f, extern_sp, stack_high, local_roots);

  /* Global C roots */
  for (gr = caml_global_roots.forward[0]; gr != NULL; gr = gr->forward[0]) {
    f(*(gr->root), gr->root);
  }
  /* Finalised values */
  final_do_strong_roots (f);
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(f);
}

void do_local_roots (scanning_action f, value *stack_low, value *stack_high,
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
        f (*sp, sp);
      }
    }
  }
}

