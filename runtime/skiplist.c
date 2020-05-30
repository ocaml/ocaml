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

#define CAML_INTERNALS

/* A dictionary data structure implemented as skip lists
   (see William Pugh, "Skip lists: a probabilistic alternative to
   balanced binary trees", Comm. ACM 33(6), 1990). */

#include <stddef.h>
#include "caml/config.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/skiplist.h"

/* Size of struct skipcell, in bytes, without the forward array */
#if (__STDC_VERSION__ >= 199901L)
#define SIZEOF_SKIPCELL sizeof(struct skipcell)
#else
#define SIZEOF_SKIPCELL (sizeof(struct skipcell) - sizeof(struct skipcell *))
#endif

/* Generate a random level for a new node: 0 with probability 3/4,
   1 with probability 3/16, 2 with probability 3/64, etc.
   We use a simple linear congruential PRNG (see Knuth vol 2) instead
   of random(), because we need exactly 32 bits of pseudo-random data
   (i.e. 2 * (NUM_LEVELS - 1)).  Moreover, the congruential PRNG
   is faster and guaranteed to be deterministic (to reproduce bugs). */

static uint32_t random_seed = 0;

static int random_level(void)
{
  uint32_t r;
  int level = 0;

  /* Linear congruence with modulus = 2^32, multiplier = 69069
     (Knuth vol 2 p. 106, line 15 of table 1), additive = 25173. */
  r = random_seed = random_seed * 69069 + 25173;
  /* Knuth (vol 2 p. 13) shows that the least significant bits are
     "less random" than the most significant bits with a modulus of 2^m,
     so consume most significant bits first */
  while ((r & 0xC0000000U) == 0xC0000000U) { level++; r = r << 2; }
  CAMLassert(level < NUM_LEVELS);
  return level;
}

/* Initialize a skip list */

void caml_skiplist_init(struct skiplist * sk)
{
  int i;
  for (i = 0; i < NUM_LEVELS; i++) sk->forward[i] = NULL;
  sk->level = 0;
}

/* Search a skip list */

int caml_skiplist_find(struct skiplist * sk, uintnat key, uintnat * data)
{
  int i;
  struct skipcell * e, * f;

  e = (struct skipcell *) sk;
  for (i = sk->level; i >= 0; i--) {
    while (1) {
      f = e->forward[i];
      if (f == NULL || f->key >= key) break;
      e = f;
    }
  }
  e = e->forward[0];
  if (e != NULL && e->key == key) {
    *data = e->data;
    return 1;
  } else {
    return 0;
  }
}

int caml_skiplist_find_below(struct skiplist * sk, uintnat k,
                             uintnat * key, uintnat * data)
{
  int i;
  struct skipcell * e, * f;

  e = (struct skipcell *) sk;
  for (i = sk->level; i >= 0; i--) {
    while (1) {
      f = e->forward[i];
      if (f == NULL || f->key > k) break;
      e = f;
    }
  }
  if (e == (struct skipcell *) sk) {
    return 0;
  } else {
    *key = e-> key; *data = e->data; return 1;
  }
}

/* Insertion in a skip list */

int caml_skiplist_insert(struct skiplist * sk,
                         uintnat key, uintnat data)
{
  struct skipcell * update[NUM_LEVELS];
  struct skipcell * e, * f;
  int i, new_level;

  /* Init "cursor" to list head */
  e = (struct skipcell *) sk;
  /* Find place to insert new node */
  for (i = sk->level; i >= 0; i--) {
    while (1) {
      f = e->forward[i];
      if (f == NULL || f->key >= key) break;
      e = f;
    }
    update[i] = e;
  }
  e = e->forward[0];
  /* If already present, update data */
  if (e != NULL && e->key == key) {
    e->data = data;
    return 1;
  }
  /* Insert additional element, updating list level if necessary */
  new_level = random_level();
  if (new_level > sk->level) {
    for (i = sk->level + 1; i <= new_level; i++)
      update[i] = (struct skipcell *) sk;
    sk->level = new_level;
  }
  e = caml_stat_alloc(SIZEOF_SKIPCELL +
                      (new_level + 1) * sizeof(struct skipcell *));
  e->key = key;
  e->data = data;
  for (i = 0; i <= new_level; i++) {
    e->forward[i] = update[i]->forward[i];
    update[i]->forward[i] = e;
  }
  return 0;
}

/* Deletion in a skip list */

int caml_skiplist_remove(struct skiplist * sk, uintnat key)
{
  struct skipcell * update[NUM_LEVELS];
  struct skipcell * e, * f;
  int i;

  /* Init "cursor" to list head */
  e = (struct skipcell *) sk;
  /* Find element in list */
  for (i = sk->level; i >= 0; i--) {
    while (1) {
      f = e->forward[i];
      if (f == NULL || f->key >= key) break;
      e = f;
    }
    update[i] = e;
  }
  e = e->forward[0];
  /* If not found, nothing to do */
  if (e == NULL || e->key != key) return 0;
  /* Rebuild list without node */
  for (i = 0; i <= sk->level; i++) {
    if (update[i]->forward[i] == e)
      update[i]->forward[i] = e->forward[i];
  }
  /* Reclaim list element */
  caml_stat_free(e);
  /* Down-correct list level */
  while (sk->level > 0 &&
         sk->forward[sk->level] == NULL)
    sk->level--;
  return 1;
}

/* Empty a skip list */

void caml_skiplist_empty(struct skiplist * sk)
{
  struct skipcell * e, * next;
  int i;

  for (e = sk->forward[0]; e != NULL; e = next) {
    next = e->forward[0];
    caml_stat_free(e);
  }
  for (i = 0; i <= sk->level; i++) sk->forward[i] = NULL;
  sk->level = 0;
}
