/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*               Sadiq Jaffer, OCaml Labs Consultancy Ltd                 */
/*               Xavier Leroy, projet Cambium, INRIA Paris                */
/*                                                                        */
/*   Copyright 2021 OCaml Labs Consultancy Ltd                            */
/*   Copyright 2020 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* A concurrent dictionary data structure implemented as skip lists. See
   implementation for much more detail. */

/* Keys and associated data are natural-width integers (type [uintnat]).
   Key values 0 and uintnat_max are reserved for internal use; do not use.
   Pointers can be used too, modulo conversion to [uintnat]. */

#ifndef CAML_SKIPLIST_H
#define CAML_SKIPLIST_H

#ifdef CAML_INTERNALS

#include "config.h"

#define NUM_LEVELS 17

/* The head of a skip list */

struct lf_skiplist {
  struct lf_skipcell *head;
  struct lf_skipcell *tail;
  uintnat _Atomic search_level; /* racy level to start searches at */
  struct lf_skipcell *_Atomic garbage_head;
};

/* The cells of a skip list */

struct lf_skipcell {
  uintnat key;
  uintnat data;
  uintnat top_level;
  void *stat_block;
  struct lf_skipcell *_Atomic garbage_next;
  struct lf_skipcell *_Atomic forward[]; /* flexible array member */
};

/* Initialize a skip list */
extern void caml_lf_skiplist_init(struct lf_skiplist *sk);

/* Search a skip list.
   If [key] is found, store associated data in [*data] and return 1.
   If [key] is not found, return 0 and leave [*data] unchanged. */
extern int caml_lf_skiplist_find(struct lf_skiplist *sk, uintnat key,
                                 /*out*/ uintnat *data);

/* Search the entry of the skip list that has the largest key less than
   or equal to [k].
   If such an entry exists, store its key in [*key], the associated data in
   [*data], and return 1.
   If no such entry exists (all keys in the skip list are strictly greater
   than [k]), return 0 and leave [*key] and [*data] unchanged. */
extern int caml_lf_skiplist_find_below(struct lf_skiplist *sk, uintnat k,
                                       /*out*/ uintnat *key,
                                       /*out*/ uintnat *data);
/* Insertion in a skip list. [key] must be between 1 and CAML_UINTNAT_MAX-1.
   If [key] was already there, change the associated data and return 1.
   If [key] was not there, insert new [key, data] binding and return 0. */
extern int caml_lf_skiplist_insert(struct lf_skiplist *sk, uintnat key,
                                   uintnat data);

/* Deletion in a skip list.
   If [key] was there, remove it and return 1.
   If [key] was not there, leave the skip list unchanged and return 0. */
extern int caml_lf_skiplist_remove(struct lf_skiplist *sk, uintnat key);

/* This must only be called by a single domain during a stop-the world
    protected by global barriers. */
extern void caml_lf_skiplist_free_garbage(struct lf_skiplist *sk);

/* Macros used for marking pointers and that are unfortunately necessary
  in the header for FOREACH_LF_SKIPLIST_ELEMENT to work */
#define LF_SK_IS_MARKED(p) ((p)&1)
#define LF_SK_MARKED(p) ((struct lf_skipcell *)(((uintptr_t)(p)) | 1))
#define LF_SK_UNMARK(p) ((struct lf_skipcell *)(((uintptr_t)(p)) & ~1))
#define LF_SK_EXTRACT(from, mark_to, ptr_to)                                   \
  {                                                                            \
    uintptr_t tmp = (uintptr_t)atomic_load_acquire(&(from));                   \
    mark_to = LF_SK_IS_MARKED(tmp);                                            \
    ptr_to = LF_SK_UNMARK(tmp);                          \
  }

/* Iterate over a skip list, in increasing order of keys.
   [var] designates the current element.
   [action] can refer to [var->key] and [var->data].
   [action] can safely remove the current element, i.e. call
   [caml_lf_skiplist_remove] on [var->key].  The traversal will
   continue with the skiplist element following the removed element.
   Other operations performed over the skiplist during its traversal have
   unspecified effects on the traversal. */

#define FOREACH_LF_SKIPLIST_ELEMENT(var, sk, action)                           \
  {                                                                            \
    struct lf_skipcell *var, *caml__next;                                      \
    int caml__marked;                                                          \
    var = (sk)->head->forward[0];                                              \
    while (var != (sk)->tail) {                                                \
      LF_SK_EXTRACT(var->forward[0], caml__marked, caml__next);                \
      if (!caml__marked) {                                                     \
        action;                                                                \
      }                                                                        \
      var = caml__next;                                                        \
    }                                                                          \
  }
#endif /* CAML_INTERNALS */

#endif /* CAML_SKIPLIST_H */
