/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Registration of global memory roots */

#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "globroots.h"

/* The set of global memory roots is represented as a skip list
   (see William Pugh, "Skip lists: a probabilistic alternative to
   balanced binary trees", Comm. ACM 33(6), 1990). */

/* Generate a random level for a new node: 0 with probability 3/4,
   1 with probability 3/16, 2 with probability 3/64, etc.
   We use a simple linear congruential PRNG (see Knuth vol 2) instead
   of random(), because we need exactly 32 bits of pseudo-random data
   (i.e. 2 * (NUM_LEVELS - 1)).  Moreover, the congruential PRNG
   is faster and guaranteed to be deterministic (to reproduce bugs). */

static uint32 random_seed = 0;

static int random_level(void)
{
  uint32 r;
  int level = 0;

  /* Linear congruence with modulus = 2^32, multiplier = 69069
     (Knuth vol 2 p. 106, line 15 of table 1), additive = 25173. */
  r = random_seed = random_seed * 69069 + 25173;
  /* Knuth (vol 2 p. 13) shows that the least significant bits are
     "less random" than the most significant bits with a modulus of 2^m,
     so consume most significant bits first */
  while ((r & 0xC0000000U) == 0xC0000000U) { level++; r = r << 2; }
  Assert(level < NUM_LEVELS);
  return level;
}

/* The initial global root list */

struct global_root_list caml_global_roots = { NULL, { NULL, }, 0 };

/* Register a global C root */

CAMLexport void caml_register_global_root(value *r)
{
  struct global_root * update[NUM_LEVELS];
  struct global_root * e, * f;
  int i, new_level;

  Assert (((intnat) r & 3) == 0);  /* compact.c demands this (for now) */

  /* Init "cursor" to list head */
  e = (struct global_root *) &caml_global_roots;
  /* Find place to insert new node */
  for (i = caml_global_roots.level; i >= 0; i--) {
    while (1) {
      f = e->forward[i];
      if (f == NULL || f->root >= r) break;
      e = f;
    }
    update[i] = e;
  }
  e = e->forward[0];
  /* If already present, don't do anything */
  if (e != NULL && e->root == r) return;
  /* Insert additional element, updating list level if necessary */
  new_level = random_level();
  if (new_level > caml_global_roots.level) {
    for (i = caml_global_roots.level + 1; i <= new_level; i++)
      update[i] = (struct global_root *) &caml_global_roots;
    caml_global_roots.level = new_level;
  }
  e = caml_stat_alloc(sizeof(struct global_root) +
                      new_level * sizeof(struct global_root *));
  e->root = r;
  for (i = 0; i <= new_level; i++) {
    e->forward[i] = update[i]->forward[i];
    update[i]->forward[i] = e;
  }
}

/* Un-register a global C root */

CAMLexport void caml_remove_global_root(value *r)
{
  struct global_root * update[NUM_LEVELS];
  struct global_root * e, * f;
  int i;

  /* Init "cursor" to list head */
  e = (struct global_root *) &caml_global_roots;
  /* Find element in list */
  for (i = caml_global_roots.level; i >= 0; i--) {
    while (1) {
      f = e->forward[i];
      if (f == NULL || f->root >= r) break;
      e = f;
    }
    update[i] = e;
  }
  e = e->forward[0];
  /* If not found, nothing to do */
  if (e == NULL || e->root != r) return;
  /* Rebuild list without node */
  for (i = 0; i <= caml_global_roots.level; i++) {
    if (update[i]->forward[i] == e)
      update[i]->forward[i] = e->forward[i];
  }
  /* Reclaim list element */
  caml_stat_free(e);
  /* Down-correct list level */
  while (caml_global_roots.level > 0 && 
         caml_global_roots.forward[caml_global_roots.level] == NULL)
    caml_global_roots.level--;
}
