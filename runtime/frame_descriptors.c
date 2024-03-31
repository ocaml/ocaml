/**************************************************************************/
/*        ^o3                                                             */
/* ~/\_/\_|)                       OCaml                                  */
/* |/=_=\|                                                                */
/* "     "                                                                */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Tom Kelly, OCaml Labs Consultancy                    */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2021 OCaml Labs Consultancy Ltd                            */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/platform.h"
#include "caml/frame_descriptors.h"
#include "caml/major_gc.h" /* for caml_major_cycles_completed */
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/shared_heap.h"
#include <stddef.h>

struct caml_frame_descrs {
  int num_descr;
  int mask;
  frame_descr** descriptors;
  caml_frametable_list *frametables;
  caml_frametable_list *zombies;
  caml_plat_mutex mutex;
};
/* Let us call 'capacity' the length of the descriptors array.

   We maintain the following invariants:
     capacity = mask + 1
     capacity = 0 || Is_power_of_2(capacity)
     num_desc <= 2 * num_descr <= capacity

   For an extensible array we would maintain
      num_desc <= capacity,
    but this is a linear-problem hash table, we need to ensure that
    free slots are frequent enough, so we use a twice-larger capacity:
      num_desc * 2 <= capacity

   We keep the list of frametables that was used to build the hashtable.
   We use it when rebuilding the table after resizing.

   We keep the list of frametables which need to be removed (zombies).
   We remove them in a STW handler before any new addition.

   We allow the user to call functions for frametable removal at any
   time, even inside a STW section (e.g. during custom block finalization).
   To this end, we use a mutex to protect the frametable insertions in
   the zombies list.

   All other modifications should only happen in a STW section.
*/

/* Defined in code generated by ocamlopt */
extern intnat * caml_frametable[];

/* Note: [cur] is bound by this macro */
#define iter_list(list,cur) \
  for (caml_frametable_list *cur = list; cur != NULL; cur = cur->next)

static frame_descr * next_frame_descr(frame_descr * d) {
  unsigned char num_allocs = 0, *p;
  CAMLassert(d->retaddr >= 4096);
  if (!frame_return_to_C(d)) {
    /* Skip to end of live_ofs */
    p = (unsigned char*)&d->live_ofs[d->num_live];
    /* Skip alloc_lengths if present */
    if (frame_has_allocs(d)) {
      num_allocs = *p;
      p += num_allocs + 1;
    }
    /* Skip debug info if present */
    if (frame_has_debug(d)) {
      /* Align to 32 bits */
      p = Align_to(p, uint32_t);
      p += sizeof(uint32_t) * (frame_has_allocs(d) ? num_allocs : 1);
    }
    /* Align to word size */
    p = Align_to(p, void*);
    return ((frame_descr*) p);
  } else {
    /* This marks the top of an ML stack chunk. Skip over empty
     * frame descriptor */
    /* Skip to address of zero-sized live_ofs */
    CAMLassert(d->num_live == 0);
    p = (unsigned char*)&d->live_ofs[0];
    /* Align to word size */
    p = Align_to(p, void*);
    return ((frame_descr*) p);
  }
}

static intnat count_descriptors(caml_frametable_list *list) {
  intnat num_descr = 0;
  iter_list(list,cur) {
    num_descr += *((intnat*) cur->frametable);
  }
  return num_descr;
}

static caml_frametable_list* frametables_list_tail(caml_frametable_list *list) {
  caml_frametable_list *tail = NULL;
  iter_list(list,cur) {
    tail = cur;
  }
  return tail;
}

static int capacity(caml_frame_descrs table) {
  int capacity = table.mask + 1;
  CAMLassert(capacity == 0 || Is_power_of_2(capacity));
  return capacity;
}

static void fill_hashtable(
  caml_frame_descrs *table, caml_frametable_list *new_frametables)
{
  iter_list(new_frametables,cur) {
    intnat * tbl = (intnat*) cur->frametable;
    intnat len = *tbl;
    frame_descr * d = (frame_descr *)(tbl + 1);
    for (intnat j = 0; j < len; j++) {
      uintnat h = Hash_retaddr(d->retaddr, table->mask);
      while (table->descriptors[h] != NULL) {
        h = (h+1) & table->mask;
      }
      table->descriptors[h] = d;
      d = next_frame_descr(d);
    }
  }
}

static void remove_entry(caml_frame_descrs *table, frame_descr * d) {
  uintnat i;
  uintnat r;
  uintnat j;

  i = Hash_retaddr(d->retaddr, table->mask);
  while (table->descriptors[i] != d) {
    i = (i+1) & table->mask;
  }

 r1:
  j = i;
  table->descriptors[i] = NULL;
 r2:
  i = (i+1) & table->mask;
  // r3
  if(table->descriptors[i] == NULL) return;
  r = Hash_retaddr(table->descriptors[i]->retaddr, table->mask);
  /* If r is between i and j (cyclically), i.e. if
     table->descriptors[i]->retaddr don't need to be moved */
  if(( ( j < r )  && ( r <= i ) ) ||
     ( ( i < j )  && ( j < r )  ) ||      /* i cycled, r not */
     ( ( r <= i ) && ( i < j ) )     ) {  /* i and r cycled */
    goto r2;
  }
  // r4
  table->descriptors[j] = table->descriptors[i];
  goto r1;
}

static void clean_frame_descriptors(caml_frame_descrs *table)
{
  intnat *tbl, len, decrease = 0;
  frame_descr * d;
  caml_frametable_list *cur = table->zombies, *rem;
  while (cur != NULL) {
    tbl = (intnat*) cur->frametable;
    len = *tbl;
    d = (frame_descr *)(tbl + 1);
    for (intnat j = 0; j < len; j++) {
      remove_entry(table, d);
      d = next_frame_descr(d);
    }
    decrease += len;
    rem = cur;
    cur = cur->next;
    caml_stat_free(rem);
  }
  table->num_descr -= decrease;
  table->zombies = NULL;
}

static void add_frame_descriptors(
  caml_frame_descrs *table,
  caml_frametable_list *new_frametables)
{
  CAMLassert(new_frametables != NULL);

  caml_frametable_list *tail = frametables_list_tail(new_frametables);
  intnat increase = count_descriptors(new_frametables);
  intnat tblsize = capacity(*table);

  /* The size of the hashtable is a power of 2 that must remain
     greater or equal to 2 times the number of descriptors. */

  /* Reallocate the caml_frame_descriptor table if it is too small */
  if(tblsize < (table->num_descr + increase) * 2) {

    /* Merge both lists */
    tail->next = table->frametables;
    table->frametables = NULL;

    intnat num_descr = table->num_descr + increase;

    tblsize = 4;
    while (tblsize < 2 * num_descr) tblsize *= 2;

    table->num_descr = num_descr;
    table->mask = tblsize - 1;

    if (table->descriptors != NULL) caml_stat_free(table->descriptors);
    table->descriptors =
      (frame_descr **) caml_stat_calloc_noexc(tblsize, sizeof(frame_descr *));
    if (table->descriptors == NULL) caml_raise_out_of_memory();

    fill_hashtable(table, new_frametables);
  } else {
    table->num_descr += increase;
    fill_hashtable(table, new_frametables);
    tail->next = table->frametables;
  }

  table->frametables = new_frametables;
}

/* protected by STW sections */
static caml_frame_descrs current_frame_descrs =
  { 0, -1, NULL, NULL, NULL, PTHREAD_MUTEX_INITIALIZER };

static caml_frametable_list *cons(
  intnat *frametable, caml_frametable_list *tl)
{
  caml_frametable_list *li = caml_stat_alloc(sizeof(caml_frametable_list));
  li->frametable = frametable;
  li->next = tl;
  return li;
}

/* This function not only creates a new caml_frametable_list cell but
   also makes a copy of the new frametable.
   Here, we allocate, in a single malloc call, the space for the cons
   cell and the (appended) frametable copy. This way, we do not have
   to change the code that unregisters the frametable since calling free
   on the cons cell will automatically free the frametable copy at the
   same time.
*/
static caml_frametable_list *copy_cons(
  intnat **frametable, intnat size, caml_frametable_list *tl)
{
  caml_frametable_list *li =
    caml_stat_alloc(sizeof(caml_frametable_list) + size);
  intnat *frametable_copy = (intnat*)(li + 1);
  memcpy(frametable_copy, *frametable, size);
  *frametable = frametable_copy;
  li->frametable = frametable_copy;
  li->next = tl;
  return li;
}

void caml_init_frame_descriptors(void)
{
  caml_frametable_list *frametables = NULL;
  for (int i = 0; caml_frametable[i] != 0; i++)
    frametables = cons(caml_frametable[i], frametables);

  /* `init_frame_descriptors` is called from `init_gc`, before
     any mutator can run. We can mutate [current_frame_descrs]
     at will. */
  add_frame_descriptors(&current_frame_descrs, frametables);
}

static void register_frametables_from_stw_single(
  caml_frametable_list *new_frametables)
{
  clean_frame_descriptors(&current_frame_descrs);
  add_frame_descriptors(&current_frame_descrs, new_frametables);
}

static void stw_register_frametables(
    caml_domain_state* domain,
    void* frametables,
    int participating_count,
    caml_domain_state** participating)
{
  barrier_status b = caml_global_barrier_begin ();

  if (caml_global_barrier_is_final(b)) {
    register_frametables_from_stw_single((caml_frametable_list*) frametables);
  }

  caml_global_barrier_end(b);
}

void caml_register_frametables(void **table, int ntables) {
  caml_frametable_list *new_frametables = NULL;
  for (int i = 0; i < ntables; i++)
    new_frametables = cons(table[i], new_frametables);

  do {} while (!caml_try_run_on_all_domains(
                 &stw_register_frametables, new_frametables, 0));
}

void caml_copy_and_register_frametables(
  void **table, int * sizes, int ntables)
{
  caml_frametable_list *new_frametables = NULL;
  for (int i = 0; i < ntables; i++)
    new_frametables = copy_cons((intnat **)(table + i),
                                sizes[i], new_frametables);

  do {} while (!caml_try_run_on_all_domains(
                 &stw_register_frametables, new_frametables, 0));
}

static void remove_frame_descriptors(
  caml_frame_descrs * table, void ** frametables, int ntables)
{
  void *frametable;
  caml_frametable_list ** previous;

  caml_plat_lock(&table->mutex);

  previous = &table->frametables;

  iter_list(table->frametables, current) {
  resume:
    for (int i = 0; i < ntables; i++) {
      if (current->frametable == frametables[i]) {
        *previous = current->next;
        current->next = table->zombies;
        table->zombies = current;
        ntables--;
        if (ntables == 0) goto release;
        current = *previous;
        frametable = frametables[i];
        frametables[i] = frametables[ntables];
        frametables[ntables] = frametable;
        goto resume;
      }
    }
    previous = &current->next;
  }

 release:
  caml_plat_unlock(&table->mutex);
}

void caml_unregister_frametables(void ** frametables, int ntables)
{
  remove_frame_descriptors(&current_frame_descrs, frametables, ntables);
}

void caml_register_frametable(void * frametables)
{
  caml_register_frametables(&frametables, 1);
}

void* caml_copy_and_register_frametable(void * frametable, int size)
{
  caml_copy_and_register_frametables(&frametable, &size, 1);
  return frametable;
}

void caml_unregister_frametable(void * frametables)
{
  caml_unregister_frametables(&frametables, 1);
}

caml_frame_descrs* caml_get_frame_descrs(void)
{
  return &current_frame_descrs;
}

frame_descr* caml_find_frame_descr(caml_frame_descrs *fds, uintnat pc)
{
  frame_descr * d;
  uintnat h;

  h = Hash_retaddr(pc, fds->mask);
  while (1) {
    d = fds->descriptors[h];
    if (d == 0) return NULL; /* can happen if some code compiled without -g */
    if (d->retaddr == pc) break;
    h = (h+1) & fds->mask;
  }
  return d;
}
