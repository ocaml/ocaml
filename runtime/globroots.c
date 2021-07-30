/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Registration of global memory roots */

#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/globroots.h"
#include "caml/skiplist.h"

/* The three global root lists.
   Each is represented by a skip list with the key being the address
   of the root.  (The associated data field is unused.) */

struct skiplist caml_global_roots = SKIPLIST_STATIC_INITIALIZER;
                  /* mutable roots, don't know whether old or young */
struct skiplist caml_global_roots_young = SKIPLIST_STATIC_INITIALIZER;
                  /* generational roots pointing to minor or major heap */
struct skiplist caml_global_roots_old = SKIPLIST_STATIC_INITIALIZER;
                  /* generational roots pointing to major heap */

/* The invariant of the generational roots is the following:
   - If the global root contains a pointer to the minor heap, then the root is
     in [caml_global_roots_young];
   - If the global root contains a pointer to the major heap, then the root is
     in [caml_global_roots_old] or in [caml_global_roots_young];
   - Otherwise (the root contains a pointer outside of the heap or an integer),
     then neither [caml_global_roots_young] nor [caml_global_roots_old] contain
     it.
*/

/* Insertion and deletion */

Caml_inline void caml_insert_global_root(struct skiplist * list, value * r)
{
  caml_skiplist_insert(list, (uintnat) r, 0);
}

Caml_inline void caml_delete_global_root(struct skiplist * list, value * r)
{
  caml_skiplist_remove(list, (uintnat) r);
}

/* Iterate a GC scanning action over a global root list */

static void caml_iterate_global_roots(scanning_action f,
                                      struct skiplist * rootlist)
{
  FOREACH_SKIPLIST_ELEMENT(e, rootlist, {
      value * r = (value *) (e->key);
      f(*r, r);
    })
}

/* Register a global C root of the mutable kind */

CAMLexport void caml_register_global_root(value *r)
{
  CAMLassert (((intnat) r & 3) == 0);  /* compact.c demands this (for now) */
  caml_insert_global_root(&caml_global_roots, r);
}

/* Un-register a global C root of the mutable kind */

CAMLexport void caml_remove_global_root(value *r)
{
  caml_delete_global_root(&caml_global_roots, r);
}

enum gc_root_class {
  YOUNG,
  OLD,
  UNTRACKED
};

static enum gc_root_class classify_gc_root(value v)
{
  if(!Is_block(v)) return UNTRACKED;
  if(Is_young(v)) return YOUNG;
#ifndef NO_NAKED_POINTERS
  if(!Is_in_heap(v)) return UNTRACKED;
#endif
  return OLD;
}

/* Register a global C root of the generational kind */

CAMLexport void caml_register_generational_global_root(value *r)
{
  CAMLassert (((intnat) r & 3) == 0);  /* compact.c demands this (for now) */

  switch(classify_gc_root(*r)) {
    case YOUNG:
      caml_insert_global_root(&caml_global_roots_young, r);
      break;
    case OLD:
      caml_insert_global_root(&caml_global_roots_old, r);
      break;
    case UNTRACKED: break;
  }
}

/* Un-register a global C root of the generational kind */

CAMLexport void caml_remove_generational_global_root(value *r)
{
  switch(classify_gc_root(*r)) {
    case OLD:
      caml_delete_global_root(&caml_global_roots_old, r);
      /* Fallthrough: the root can be in the young list while actually
         being in the major heap. */
    case YOUNG:
      caml_delete_global_root(&caml_global_roots_young, r);
      break;
    case UNTRACKED: break;
  }
}

/* Modify the value of a global C root of the generational kind */

CAMLexport void caml_modify_generational_global_root(value *r, value newval)
{
  enum gc_root_class c;
  /* See PRs #4704, #607 and #8656 */
  switch(classify_gc_root(newval)) {
    case YOUNG:
      c = classify_gc_root(*r);
      if(c == OLD)
        caml_delete_global_root(&caml_global_roots_old, r);
      if(c != YOUNG)
        caml_insert_global_root(&caml_global_roots_young, r);
      break;

    case OLD:
      /* If the old class is YOUNG, then we do not need to do
         anything: It is OK to have a root in roots_young that
         suddenly points to the old generation -- the next minor GC
         will take care of that. */
      if(classify_gc_root(*r) == UNTRACKED)
        caml_insert_global_root(&caml_global_roots_old, r);
      break;

    case UNTRACKED:
      caml_remove_generational_global_root(r);
      break;
  }

  *r = newval;
}

/* Scan all global roots */

void caml_scan_global_roots(scanning_action f)
{
  caml_iterate_global_roots(f, &caml_global_roots);
  caml_iterate_global_roots(f, &caml_global_roots_young);
  caml_iterate_global_roots(f, &caml_global_roots_old);
}

/* Scan global roots for a minor collection */

void caml_scan_global_young_roots(scanning_action f)
{

  caml_iterate_global_roots(f, &caml_global_roots);
  caml_iterate_global_roots(f, &caml_global_roots_young);
  /* Move young roots to old roots */
  FOREACH_SKIPLIST_ELEMENT(e, &caml_global_roots_young, {
      value * r = (value *) (e->key);
      caml_insert_global_root(&caml_global_roots_old, r);
    });
  caml_skiplist_empty(&caml_global_roots_young);
}
