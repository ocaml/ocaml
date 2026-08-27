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
#ifdef DEBUG
  uintnat check;                /* key, obfuscated: see caml_skiplist_check */
#endif
  struct skipcell * forward[]; /* flexible array member */
};

#ifdef DEBUG
/* Arbitrary. It has to be non-zero, so that a cell zeroed wholesale fails the
   check, and it is xored so that the key a cell was inserted with can be had
   back from the stamp. */
#define SKIPCELL_STAMP ((uintnat) 0x9E3779B97F4A7C15ULL)
#define Check_of(k) ((k) ^ SKIPCELL_STAMP)
#define Skipcell_ok(e) ((e)->check == Check_of((e)->key))
#define Skipcell_key_of_check(e) Check_of((e)->check)

/* The stamp, as a symbol. */
extern const uintnat caml_skipcell_stamp;

/* Fail if any cell's key no longer matches what it was inserted with. A key
   is read as an address by some users, so a write landing on one is found by
   dereferencing it. */
extern void caml_skiplist_check(struct skiplist * sk, const char * what);
#endif

/* Initialize a skip list, statically */
#define SKIPLIST_STATIC_INITIALIZER { {0, }, 0 }

/* Initialize a skip list, dynamically */
extern void caml_skiplist_init(struct skiplist * sk);

/* Search a skip list.
   If [key] is found, store associated data in [*data] and return 1.
   If [key] is not found, return 0 and leave [*data] unchanged. */
extern int caml_skiplist_find(struct skiplist * sk, uintnat key,
                              /*out*/ uintnat * data);

/* Search a skip list.
   If [key] is found, return a pointer to its associated data.
   If [key] is not found, return NULL. */
extern uintnat* caml_skiplist_find_ptr(struct skiplist * sk, uintnat key);


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

#define FOREACH_SKIPLIST_ELEMENT(var,sk,action) {               \
    for (struct skipcell *var = (sk)->forward[0], *caml__next;  \
         var != NULL;                                           \
         var = caml__next) {                                    \
      caml__next = (var)->forward[0];                           \
      action;                                                   \
    }                                                           \
  }

#endif /* CAML_INTERNALS */

#endif /* CAML_SKIPLIST_H */
