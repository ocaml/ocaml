/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
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

value * local_roots = NULL;

struct global_root {
  value * root;
  struct global_root * next;
};
  
static struct global_root * global_roots = NULL;

void (*scan_roots_hook) P((scanning_action)) = NULL;

/* Register a global C root */

void register_global_root(r)
     value * r;
{
  struct global_root * gr;
  gr = (struct global_root *) stat_alloc(sizeof(struct global_root));
  gr->root = r;
  gr->next = global_roots;
  global_roots = gr;
}

/* Call [oldify] on all stack roots and C roots */

void oldify_local_roots ()
{
  register value * sp;
  value * block;
  struct global_root * gr;

  /* The stack */
  for (sp = extern_sp; sp < stack_high; sp++) {
    oldify (*sp, sp);
  }
  /* Local C roots */
  for (block = local_roots; block != NULL; block = (value *) block [1]){
    for (sp = block - (long) block [0]; sp < block; sp++){
      oldify (*sp, sp);
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

void darken_all_roots ()
{
  register value * sp;
  value * block;
  struct global_root * gr;

  /* Global variables */
  darken(global_data);

  /* The stack */
  for (sp = extern_sp; sp < stack_high; sp++) {
    darken (*sp);
  }
  /* Local C roots */
  for (block = local_roots; block != NULL; block = (value *) block [1]){
    for (sp = block - (long) block [0]; sp < block; sp++){
      darken (*sp);
    }
  }
  /* Global C roots */
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    darken (*(gr->root));
  }
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(darken);
}



