/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cambium, INRIA Paris                  */
/*                                                                        */
/*   Copyright 2020 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* A dictionary data structure implemented as skip lists */

/* Keys and associated data are natural-width integers (type [uintnat]).
   Pointers can be used too, modulo conversion to [uintnat]. */

#ifndef CAML_SKIPLIST_H
#define CAML_SKIPLIST_H

#ifdef CAML_INTERNALS

#include "config.h"

#define NUM_LEVELS 17

/* The head of a skip list */

struct skiplist {
  struct skipcell * forward[NUM_LEVELS]; /* forward chaining */
  int level;                    /* max level used */
};

/* The cells of a skip list */

struct skipcell {
  uintnat key;
  uintnat data;
#if (__STDC_VERSION__ >= 199901L)
  struct skipcell * forward[];  /* variable-length array */
#else
  struct skipcell * forward[1]; /* variable-length array */
#endif
};

/* Initialize a skip list, statically */
#define SKIPLIST_STATIC_INITIALIZER { {0, }, 0 }

/* Initialize a skip list, dynamically */
extern void caml_skiplist_init(struct skiplist * sk);

/* Search a skip list.
   If [key] is found, store associated data in [*data] and return 1.
   If [key] is not found, return 0 and leave [*data] unchanged. */
extern int caml_skiplist_find(struct skiplist * sk, uintnat key,
                              /*out*/ uintnat * data);

/* Search the entry of the skip list that has the largest key less than
   or equal to [k].
   If such an entry exists, store its key in [*key], the associated data in
   [*data], and return 1.
   If no such entry exists (all keys in the skip list are strictly greater
   than [k]), return 0 and leave [*key] and [*data] unchanged. */
extern int caml_skiplist_find_below(struct skiplist * sk, uintnat k,
                                    /*out*/ uintnat * key,
                                    /*out*/ uintnat * data);

/* Insertion in a skip list.
   If [key] was already there, change the associated data and return 1.
   If [key] was not there, insert new [key, data] binding and return 0. */
extern int caml_skiplist_insert(struct skiplist * sk,
                                uintnat key, uintnat data);

/* Deletion in a skip list.
   If [key] was there, remove it and return 1.
   If [key] was not there, leave the skip list unchanged and return 0. */
extern int caml_skiplist_remove(struct skiplist * sk, uintnat key);

/* Empty an already initialized skip list. */
extern void caml_skiplist_empty(struct skiplist * sk);

/* Iterate over a skip list, in increasing order of keys.
   [var] designates the current element.
   [action] can refer to [var->key] and [var->data].
   [action] can safely remove the current element, i.e. call
   [caml_skiplist_remove] on [var->key].  The traversal will
   continue with the skiplist element following the removed element.
   Other operations performed over the skiplist during its traversal have
   unspecified effects on the traversal. */

#define FOREACH_SKIPLIST_ELEMENT(var,sk,action) \
  { struct skipcell * var, * caml__next; \
    for (var = (sk)->forward[0]; var != NULL; var = caml__next) \
    { caml__next = (var)->forward[0]; action; } \
  }

#endif

#endif
