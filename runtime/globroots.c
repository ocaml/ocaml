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
#include "caml/stack.h"

static caml_plat_mutex roots_mutex = CAML_PLAT_MUTEX_INITIALIZER;

/* legacy multicore API that we need to fix */
CAMLexport caml_root caml_create_root(value init)
{
  CAMLparam1(init);
  
  value* v = (value*)malloc(sizeof(value));

  *v = init;

  caml_register_global_root(v);

  CAMLreturnT(caml_root, (caml_root)v);
}

CAMLexport caml_root caml_create_root_noexc(value init)
{
  CAMLparam1(init);

  caml_root r = caml_create_root(init);

  CAMLreturnT(caml_root, r);
}

CAMLexport void caml_delete_root(caml_root root)
{
  value* v = (value*)root;
  Assert(root);
  /* the root will be removed from roots_all and freed at the next GC */
  caml_remove_global_root(v);
  free(v);
}

CAMLexport value caml_read_root(caml_root root)
{
  return *((value*)root);
}

CAMLexport void caml_modify_root(caml_root root, value newv)
{
  *((value*)root) = newv;
}

/* The three global root lists.
   Each is represented by a skip list with the key being the address
   of the root.  (The associated data field is unused.) */

struct skiplist caml_global_roots = SKIPLIST_STATIC_INITIALIZER;
                  /* mutable roots, don't know whether old or young */

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

/* Iterate a GC scanning action over a global root list */

static void caml_iterate_global_roots(scanning_action f,
                                      struct skiplist * rootlist, void* fdata)
{
  caml_plat_lock(&roots_mutex);
  FOREACH_SKIPLIST_ELEMENT(e, rootlist, {
      value * r = (value *) (e->key);
      f(fdata, *r, r);
    })
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

/* Register a global C root of the generational kind */

CAMLexport void caml_register_generational_global_root(value *r)
{
  caml_register_global_root(r);
}

/* Un-register a global C root of the generational kind */

CAMLexport void caml_remove_generational_global_root(value *r)
{
  caml_remove_global_root(r);
}

/* Modify the value of a global C root of the generational kind */

CAMLexport void caml_modify_generational_global_root(value *r, value newval)
{
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
  for (i = 0; i <= caml_globals_inited && caml_globals[i] != 0; i++) {
    for(glob = caml_globals[i]; *glob != 0; glob++) {
      for (j = 0; j < Wosize_val(*glob); j++){
        f(fdata, Op_val(*glob)[j], &Op_val(*glob)[j]);
      }
    }
  }

  /* Dynamic (natdynlink) global roots */
  iter_list(dyn_globals, lnk) {
    for(glob = (value *) lnk->data; *glob != 0; glob++) {
      for (j = 0; j < Wosize_val(*glob); j++){
        f(fdata, Op_val(*glob)[j], &Op_val(*glob)[j]);
      }
    }
  }
}

#endif

/* Scan all global roots */
void caml_scan_global_roots(scanning_action f, void* fdata) {
  caml_iterate_global_roots(f, &caml_global_roots, fdata);

  #ifdef NATIVE_CODE
  scan_native_globals(f, fdata);
  #endif
}

/* Scan global roots for a minor collection */

void caml_scan_global_young_roots(scanning_action f, void* fdata)
{
  caml_iterate_global_roots(f, &caml_global_roots, fdata);
}
