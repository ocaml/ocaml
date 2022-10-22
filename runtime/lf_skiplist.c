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

#define CAML_INTERNALS

/* A concurrent dictionary data structure implemented as skip lists. This
    implementation is based on the sequential skip list implementation in
    the runtime by Xavier Leroy but extends it to be safe under concurrent
    modification. It has the property that insert/remove are lock-free and
    contains is further wait-free. It is literally a textbook implementation
    and can be found in Herlihy et al's "The Art of Multiprocessor
    Programming" 2nd Edition, section 14.4. It only differs from the
    textbook implementation to fix errors in the pseudocode in [contains],
    to add a [search_level] optimisation to the data structure, replacing Java's
    volatile with atomics and to keep a list of removed nodes in order to do a
    deferred free.

    You _must_ call [caml_lf_skiplist_free_garbage] "every so often" in order
    for the data structure to free removed nodes. This must be done by only
    one thread at a time when no other thread can be accessing the structure.

    It is roughly half the speed of the sequential skip list so only use
    where concurrent access is necessary. For use-cases where there is
    only infrequent contention and where acquiring a lock during find is
    allowed then a sequential skip list guarded by a mutex may perform
    better.

    A sequential implementation of skip lists is in file skiplist.c and is based
    on the paper by William Pugh, "Skip lists: a probabilistic alternative to
    balanced binary trees", Comm. ACM 33(6), 1990). */

#include "caml/lf_skiplist.h"
#include "caml/config.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include <stddef.h>

/* Size of struct lf_skipcell, in bytes, without the forward array */
#if (__STDC_VERSION__ >= 199901L)
#define SIZEOF_LF_SKIPCELL sizeof(struct lf_skipcell)
#else
#define SIZEOF_LF_SKIPCELL                                                     \
  (sizeof(struct lf_skipcell) - sizeof(struct lf_skipcell *))
#endif

/* Generate a random level for a new node: 0 with probability 3/4,
   1 with probability 3/16, 2 with probability 3/64, etc.
   We use a simple linear congruential PRNG (see Knuth vol 2) instead
   of random(), because we need exactly 32 bits of pseudo-random data
   (i.e. 2 * (NUM_LEVELS - 1)).  Moreover, the congruential PRNG
   is faster and guaranteed to be deterministic (to reproduce bugs). */

static uint32_t _Atomic random_seed = 0;

static int random_level(void) {
  uint32_t r;
  int level = 0;

  /* Linear congruence with modulus = 2^32, multiplier = 69069
   (Knuth vol 2 p. 106, line 15 of table 1), additive = 25173. */

  while( 1 ) {
    uint32_t curr =
      atomic_load_explicit(&random_seed, memory_order_relaxed);

    r = curr * 69069 + 25173;

    if( atomic_compare_exchange_strong(&random_seed, &curr, r) ) {
        break;
    }
  }
  /* Knuth (vol 2 p. 13) shows that the least significant bits are
   "less random" than the most significant bits with a modulus of 2^m,
   so consume most significant bits first */
  while ((r & 0xC0000000U) == 0xC0000000U) {
    level++;
    r = r << 2;
  }
  CAMLassert(level < NUM_LEVELS);
  return level;
}

/* Initialize a skip list */

void caml_lf_skiplist_init(struct lf_skiplist *sk) {
  atomic_store_explicit(&sk->search_level, 0, memory_order_relaxed);

  /* This concurrent skip list has two sentinel nodes, the first [head] is
  less than any possible key in the data structure and the second [tail] is
  greater than any key. */
  sk->head = caml_stat_alloc(SIZEOF_LF_SKIPCELL +
                             NUM_LEVELS * sizeof(struct lf_skipcell *));
  sk->head->key = 0;
  sk->head->data = 0;
  sk->head->garbage_next = NULL;
  sk->head->top_level = NUM_LEVELS - 1;

  sk->tail = caml_stat_alloc(SIZEOF_LF_SKIPCELL +
                             NUM_LEVELS * sizeof(struct lf_skipcell *));
  sk->tail->key = UINTNAT_MAX;
  sk->tail->data = 0;
  sk->tail->garbage_next = NULL;
  sk->tail->top_level = NUM_LEVELS - 1;

  /* We do this so that later in find when we try to CAS a cell's
     `garbage_next` in `skiplist_find` we can disambiguate between a cell with
     an uninitialised `garbage_next` (that we may take ownership of) and one
     that is already in the garbage list. If we instead used NULL then this
     would not be possible.  */
  sk->garbage_head = sk->head;

  /* each level in the skip list starts of being just head pointing to tail */
  for (int j = 0; j < NUM_LEVELS; j++) {
    atomic_store_explicit
      (&sk->head->forward[j], sk->tail, memory_order_release);

    atomic_store_explicit
      (&sk->tail->forward[j], NULL, memory_order_release);
  }
}

/* [skiplist_find] is used for insert/remove and attempts to find a node in the
   skiplist. It populates the [preds] and [succs] arrays at each level. These
   arrays are later used for inserting or removing the node (by either CASing
   the new link or marking it). Additional [skiplist_find] will snip out nodes
   that have been marked for deletion if it finds during the search. The
   function is lock-free. */
static int skiplist_find(struct lf_skiplist *sk, uintnat key,
                         struct lf_skipcell **preds,
                         struct lf_skipcell **succs) {
  /* [pred] is a node that precedes the node we are looking for */
  struct lf_skipcell *pred = NULL;
  /* [curr] is the current node we are examining. If it is less
  than our key */
  struct lf_skipcell *curr = NULL;
  /* [succ] is the next node to examine at our current level */
  struct lf_skipcell *succ = NULL;

retry:
  while (1) {
    /* start at the the head of the skiplist. This node has a key less than any
       key we could be searching for */
    pred = sk->head;
    /*
    The algorithm itself is fairly simple, we start at the highest level (i.e
    the top, the level with the fewest nodes) of the skiplist and keep walking
    nodes along the level until [curr] is greater than the key we are looking
    for. When that happens we drop down to the next level and start the whole
    thing again from [pred]. If we could visualise searching for an element near
    the end of the list it would look something like a staircase with wide steps
    at the beginning and shorter ones as we descend down.

    The only complexity is that we need to make sure that we don't examine any
    nodes that are 'marked', that is the lowest bit of their forward pointer to
    the next node is set to 1. When we encounter one of those it means [curr]
    has been deleted and we need to snip it out. We might need to retry this
    several times if there's concention with other threads and we fail the
    compare-and-swap.
    */
    for (int level = NUM_LEVELS - 1; level >= 0; level--) {
      curr = LF_SK_UNMARK(
          atomic_load_explicit(&pred->forward[level], memory_order_acquire));
      while (1) {
        int is_marked;

        LF_SK_EXTRACT(curr->forward[level], is_marked, succ);
        while (is_marked) {
          struct lf_skipcell *null_cell = NULL;
          int snip = atomic_compare_exchange_strong(&pred->forward[level],
                                                    &curr, succ);
          if (!snip) {
            goto retry;
          }

          /*
          If we are at this point then we have successfully snipped out a
          removed node. What we need to try to do now is add the node to the
          skiplist's garbage list.

          There's a bit of complexity here. While we use a compare-and-swap to
          snip the node out of skiplist, it's possible that it can be removed by
          two threads at the same time from different levels of the skiplist. To
          avoid this we reuse the garbage_next field and make sure only one
          thread can ever add the node to the garbage list. This is what the
          compare-and-swap below ensures by swapping garbage_next to a value
          of 1. We don't need to worry about anyone accidentally following this
          bogus pointer, it is only dereferenced in the cleanup function and
          this is called when no thread can be concurrently modifying the
          skiplist.
          */
          if (atomic_compare_exchange_strong(&curr->garbage_next, &null_cell,
                                             (struct lf_skipcell *)1)) {
            /* Despite now having exclusivity of the current node's
               garbage_next, having won the CAS, we might be racing another
               thread to add a different node to the skiplist's garbage_head.
               This is why we need to a retry loop and yet another CAS. */
            while (1) {
              struct lf_skipcell *_Atomic current_garbage_head =
                  atomic_load_explicit(&sk->garbage_head, memory_order_acquire);

              atomic_store_explicit(&curr->garbage_next, current_garbage_head,
                                    memory_order_release);

              if (atomic_compare_exchange_strong(
                      &sk->garbage_head,
                      (struct lf_skipcell **)&current_garbage_head, curr)) {
                break;
              }
            }
          }

          /* Now try to load the current node again. We need to check it too
             hasn't been marked. If it has we repeat the process */
          curr = LF_SK_UNMARK(atomic_load_explicit(&pred->forward[level],
                                                   memory_order_acquire));
          LF_SK_EXTRACT(curr->forward[level], is_marked, succ);
        }

        if (curr->key < key) {
          pred = curr;
          curr = succ;
        } else {
          break;
        }
      }

      preds[level] = pred;
      succs[level] = curr;
    }

    return curr->key == key;
  }
}

/* [lf_skiplist_lookup] will return a skipcell or node that is greater than or
   equal to the key provided, along with the node that directly proceeds it. It
   is a much simplified version of [lf_skiplist_find] as it simply ignores
   marked nodes and does not snip them out. As a consequence, it is wait-free.

   This implementation differs from of the 'contains' in "The Art of
   Multiprocessor Programming" to fix the erroneous swap of pred and curr inside
   the while(marked) loop. It also uses [search_level] to avoid scanning the
   sentinels unnecessarily.
 */
static struct lf_skipcell *lf_skiplist_lookup(struct lf_skiplist *sk,
                                              uintnat key,
                                              struct lf_skipcell **pred_out) {
  struct lf_skipcell *pred = sk->head;
  struct lf_skipcell *curr = NULL;
  struct lf_skipcell *succ = NULL;
  int marked = 0;

  /* We start our search from the search_level of the skiplist - this is in
     contrast to the find function above where we start at NUM_LEVELS. This is
     intentional. Since every search has to eventually end up at the bottom-most
     level (even those of an empty list), if we accidentally start at the wrong
     level then our only cost is an increased number of nodes searched. If we
     did the same thing in the find function above then we'd also fail to snip
     out marked nodes. If we did that for long enough we might leak memory. */
  for (int level =
           atomic_load_explicit(&sk->search_level, memory_order_relaxed);
       level >= 0; level--) {
    curr = LF_SK_UNMARK(
        atomic_load_explicit(&pred->forward[level], memory_order_acquire));
    while (1) {
      LF_SK_EXTRACT(curr->forward[level], marked, succ);
      while (marked) {
        curr = succ;
        LF_SK_EXTRACT(curr->forward[level], marked, succ);
      }
      if (curr->key < key) {
        pred = curr;
        curr = succ;
      } else {
        break;
      }
    }
  }

  if (pred_out) {
    *pred_out = pred;
  }

  return curr;
}

/* Search a skip list */

int caml_lf_skiplist_find(struct lf_skiplist *sk, uintnat key, uintnat *data) {
  struct lf_skipcell *found_cell = lf_skiplist_lookup(sk, key, NULL);

  if (found_cell->key == key) {
    if (data) {
      *data = found_cell->data;
    }
    return 1;
  } else {
    return 0;
  }
}

int caml_lf_skiplist_find_below(struct lf_skiplist *sk, uintnat k, uintnat *key,
                                uintnat *data) {
  struct lf_skipcell *pred;
  struct lf_skipcell *curr = lf_skiplist_lookup(sk, k, &pred);
  struct lf_skipcell *found_cell;

  if (curr->key == k) {
    found_cell = curr;
  } else if (pred != sk->head) {
    found_cell = pred;
  } else {
    return 0;
  }

  if (data) {
    *data = found_cell->data;
  }
  if (key) {
    *key = found_cell->key;
  }
  return 1;
}

/* Insertion in a skip list */

int caml_lf_skiplist_insert(struct lf_skiplist *sk, uintnat key, uintnat data) {
  struct lf_skipcell *preds[NUM_LEVELS];
  struct lf_skipcell *succs[NUM_LEVELS];

  CAMLassert(key > 0 && key < UINTNAT_MAX);

  while (1) {
    /* We first try to find a node with [key] in the skip list. If it exists
       then we don't need to add it. The [skiplist_find] method will also
       populate the predecessors and successors arrays, which gives us the nodes
       between which we could add the new node. */
    int found = skiplist_find(sk, key, preds, succs);
    struct lf_skipcell *pred;
    struct lf_skipcell *succ;

    if (found) {
      /* Already present; update data */
      atomic_store_explicit((atomic_uintnat*)&succs[0]->data, data,
                            memory_order_relaxed);
      return 1;
    } else {
      /* node does not exist. We need to generate a random top_level and
       * construct a new node. The new node's forward array (which contains the
       * next node in increasing order of key, at each level) starts at
       * [top_level] and goes to 0. Each entry will point to the successors in
         the [succ] array for that level. */
      int top_level = random_level();
      /* attentive readers will have noticed that we assume memory is aligned to
       * atleast even addresses. This is certainly the case on glibc amd64 and
       * Visual C++ on Windows though I can find no guarantees for other
         platorms. */
      struct lf_skipcell *new_cell = caml_stat_alloc(
          SIZEOF_LF_SKIPCELL + (top_level + 1) * sizeof(struct lf_skipcell *));
      new_cell->top_level = top_level;
      new_cell->key = key;
      new_cell->data = data;
      atomic_store_explicit(&new_cell->garbage_next,NULL,memory_order_relaxed);

      for (int level = 0; level <= top_level; level++) {
        atomic_store_explicit(&new_cell->forward[level], succs[level],
                              memory_order_release);
      }

      /* Now we need to actually slip the node in. We start at the bottom-most
         level (i.e the linked list of all nodes). This is because all searches
         must end up at this level and so as long as the node is present, it
         will be found - regardless of whether it has been added to the level
         above. Consider the staircasing referred to in [skiplist_find] earlier,
         the final step in finding a node is following the reference from it's
         predecessor at the bottom level. */
      pred = preds[0];
      succ = succs[0];

      /* We could be racing another insertion here and if we are then restart
         the whole insertion process. We can't just retry the CAS because the
         new node's predecessor and successors could have changed. There's also
         a possibility that the predecessor's forward pointer could have been
         marked and we would fail the CAS for that reason too. In that case the
         [skiplist_find] earlier on will take care of snipping the node before
         we get back to this point. */
      if (!atomic_compare_exchange_strong(&pred->forward[0], &succ, new_cell)) {
        caml_stat_free(new_cell);
        continue;
      }

      for (int level = 1; level <= top_level; level++) {
        while (1) {
          pred = preds[level];
          succ = succs[level];

          /* If we were able to insert the node then we proceed to the next
             level */
          if (atomic_compare_exchange_strong(&pred->forward[level], &succ,
                                             new_cell)) {
            break;
          }

          /* On the other hand if we failed it might be because the pointer was
             marked or because a new node was added between pred and succ nodes
             at level. In both cases we can fix things by calling
             [skiplist_find] and repopulating preds and succs */
          skiplist_find(sk, key, preds, succs);
        }
      }

      /* If we put the new node at a higher level than the current
         [search_level] then to speed up searches we need to bump it. We don't
         care too much if this fails though. */
      if (top_level >
          atomic_load_explicit(&sk->search_level, memory_order_relaxed)) {
        atomic_store_explicit(&sk->search_level, top_level,
                              memory_order_relaxed);
      }

      return 1;
    }
  }
}

/* Deletion in a skip list */

int caml_lf_skiplist_remove(struct lf_skiplist *sk, uintnat key) {
  struct lf_skipcell *preds[NUM_LEVELS];
  struct lf_skipcell *succs[NUM_LEVELS];
  struct lf_skipcell *succ;
  int marked;

  /* As with insert. If the node doesn't exist, we don't need to do anything.
     While we're checking for it we populate the predecessor nodes and successor
     nodes at each level. */
  int found = skiplist_find(sk, key, preds, succs);

  if (!found) {
    return 0;
  } else {
    /* When the node exists in the skiplist, then succs[0] must point to it.
       Note: this isn't the case for levels > 0. */
    struct lf_skipcell *to_remove = succs[0];
    for (int level = to_remove->top_level; level >= 1; level--) {
      /* We mark each of the forward pointers at every level the node is present
         at. We may be raced by another thread deleting the same node and by
         threads inserting new nodes directly after the node we are removing,
         so we need to retry the CAS in a loop to deal with the latter. */
      LF_SK_EXTRACT(to_remove->forward[level], marked, succ);

      while (!marked) {
        atomic_compare_exchange_strong(&to_remove->forward[level], &succ,
                                       LF_SK_MARKED(succ));
        LF_SK_EXTRACT(to_remove->forward[level], marked, succ);
      }
    }

    /* The bottom layer is what ultimately determines whether the node is
       present in the skiplist or not. We try to remove it and if we succeed
       then indicate so to the caller. If not then another thread raced us an
       won. */
    LF_SK_EXTRACT(to_remove->forward[0], marked, succ);
    while (1) {
      int mark_success = atomic_compare_exchange_strong(
          &to_remove->forward[0], &succ, LF_SK_MARKED(succ));

      LF_SK_EXTRACT(to_remove->forward[0], marked, succ);

      if (mark_success) {
        skiplist_find(sk, key, preds, succs); /* This will fix up the mark */
        return 1;
      } else if (marked) {
        return 0; /* Someone else beat us to removing it */
      }

      /* If we end up here then we lost to a thread inserting a node directly
         after the node we were removing. That's why we move on one successor.
       */
    }
  }
}

/* Collects freed nodes from the skiplist. This must be called periodically from
   a single thread at a time when there can be no concurrent access to this
   skiplist */

void caml_lf_skiplist_free_garbage(struct lf_skiplist *sk) {
  struct lf_skipcell *curr =
      atomic_load_explicit(&sk->garbage_head, memory_order_acquire);

  struct lf_skipcell *head = sk->head;
  while (curr != head) {
    struct lf_skipcell *next = atomic_load_explicit
                                  (&curr->garbage_next, memory_order_relaxed);
    // acquire not useful, if executed in STW
    caml_stat_free(curr);
    curr = next;
  }

  atomic_store_explicit(&sk->garbage_head, sk->head, memory_order_release);
}
