/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* To walk the memory roots for garbage collection */

#include "memory.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "stacks.h"

struct caml__roots_block *local_roots = NULL;

struct global_root {
  value * root;
  struct global_root * next;
};

static struct global_root * global_roots = NULL;

void (*scan_roots_hook) (scanning_action f) = NULL;

/* Register a global C root */

void register_global_root(value *r)
{
  struct global_root * gr;
  gr = (struct global_root *) stat_alloc(sizeof(struct global_root));
  gr->root = r;
  gr->next = global_roots;
  global_roots = gr;
}

/* Un-register a global C root */

void remove_global_root(value *r)
{
  struct global_root ** gp, * gr;
  for (gp = &global_roots; *gp != NULL; gp = &(*gp)->next) {
    gr = *gp;
    if (gr->root == r) {
      *gp = gr->next;
      stat_free((char *) gr);
      return;
    }
  }
}

/* Call [oldify] on all roots except [global_data] */

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
  /* Local C roots */
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        oldify (*sp, sp);
      }
    }
  }
  /* Global C roots */
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    oldify(*(gr->root), gr->root);
  }
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(oldify);
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
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    f (*(gr->root), gr->root);
  }
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(f);
}

void do_local_roots (scanning_action f, value *stack_low, value *stack_high, struct caml__roots_block *local_roots)
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

