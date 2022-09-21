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
#include "caml/memory.h"
#include "caml/roots.h"
#include "caml/globroots.h"
#include "caml/skiplist.h"
#include "caml/stack.h"

static caml_plat_mutex roots_mutex = CAML_PLAT_MUTEX_INITIALIZER;

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
     it. */

/* Insertion and deletion */

Caml_inline void caml_insert_global_root(struct skiplist * list, value * r)
{
  caml_plat_lock(&roots_mutex);
  caml_skiplist_insert(list, (uintnat) r, 0);
  caml_plat_unlock(&roots_mutex);
}

Caml_inline void caml_delete_global_root(struct skiplist * list, value * r)
{
  caml_plat_lock(&roots_mutex);
  caml_skiplist_remove(list, (uintnat) r);
  caml_plat_unlock(&roots_mutex);
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
  return OLD;
}

/* Register a global C root of the generational kind */

CAMLexport void caml_register_generational_global_root(value *r)
{
  Caml_check_caml_state();
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

#ifdef NATIVE_CODE

/* Linked-list of natdynlink'd globals */

typedef struct link {
  void *data;
  struct link *next;
} link;

static link *cons(void *data, link *tl) {
  link *lnk = caml_stat_alloc(sizeof(link));
  lnk->data = data;
  lnk->next = tl;
  return lnk;
}

#define iter_list(list,lnk) \
  for (lnk = list; lnk != NULL; lnk = lnk->next)


/* protected by roots_mutex */
static link * caml_dyn_globals = NULL;

void caml_register_dyn_global(void *v) {
  caml_plat_lock(&roots_mutex);
  caml_dyn_globals = cons((void*) v,caml_dyn_globals);
  caml_plat_unlock(&roots_mutex);
}

static void scan_native_globals(scanning_action f, void* fdata)
{
  int i, j;
  static link* dyn_globals;
  value* glob;
  link* lnk;

  caml_plat_lock(&roots_mutex);
  dyn_globals = caml_dyn_globals;
  caml_plat_unlock(&roots_mutex);

  /* The global roots */
  for (i = 0; caml_globals[i] != 0; i++) {
    for(glob = caml_globals[i]; *glob != 0; glob++) {
      for (j = 0; j < Wosize_val(*glob); j++){
        f(fdata, Field(*glob, j), &Field(*glob, j));
      }
    }
  }

  /* Dynamic (natdynlink) global roots */
  iter_list(dyn_globals, lnk) {
    for(glob = (value *) lnk->data; *glob != 0; glob++) {
      for (j = 0; j < Wosize_val(*glob); j++){
        f(fdata, Field(*glob, j), &Field(*glob, j));
      }
    }
  }
}

#endif

/* Iterate a GC scanning action over a global root list */
Caml_inline void caml_iterate_global_roots(scanning_action f,
                                      struct skiplist * rootlist, void* fdata)
{
  FOREACH_SKIPLIST_ELEMENT(e, rootlist, {
      value * r = (value *) (e->key);
      f(fdata, *r, r);
    })
}

/* Scan all global roots */
void caml_scan_global_roots(scanning_action f, void* fdata) {
  caml_plat_lock(&roots_mutex);
  caml_iterate_global_roots(f, &caml_global_roots, fdata);
  caml_iterate_global_roots(f, &caml_global_roots_young, fdata);
  caml_iterate_global_roots(f, &caml_global_roots_old, fdata);
  caml_plat_unlock(&roots_mutex);

  #ifdef NATIVE_CODE
  scan_native_globals(f, fdata);
  #endif
}

/* Scan global roots for a minor collection */
void caml_scan_global_young_roots(scanning_action f, void* fdata)
{
  caml_plat_lock(&roots_mutex);

  caml_iterate_global_roots(f, &caml_global_roots, fdata);
  caml_iterate_global_roots(f, &caml_global_roots_young, fdata);

  /* Move young roots to old roots */
  FOREACH_SKIPLIST_ELEMENT(e, &caml_global_roots_young, {
      value * r = (value *) (e->key);
      caml_skiplist_insert(&caml_global_roots_old, (uintnat) r, 0);
    });
  caml_skiplist_empty(&caml_global_roots_young);

  caml_plat_unlock(&roots_mutex);
}
