/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                Stephen Dolan, University of Cambridge                  */
/*                                                                        */
/*   Copyright 2020 Indian Institute of Technology, Madras                */
/*   Copyright 2020 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/config.h"
#include "caml/memory.h"
#include "caml/addrmap.h"
#include "caml/startup_aux.h"

#define MAX_CHAIN 10

static uintnat pos_initial(struct addrmap* t, value key)
{
  return key % (t->size);
}

static uintnat pos_next(struct addrmap* t, uintnat pos)
{
  return (pos + 1) % (t->size);
}

static void addrmap_alloc(struct addrmap* t, uintnat sz)
{
  uintnat i;
  CAMLassert(sz > 0 && (sz & (sz - 1)) == 0); /* sz must be a power of 2 */
  t->entries = caml_stat_alloc(sizeof(struct addrmap_entry) * sz);
  t->size = sz;
  for (i = 0; i < sz; i++) {
    t->entries[i].key = ADDRMAP_INVALID_KEY;
    t->entries[i].value = ADDRMAP_NOT_PRESENT;
  }
}

void caml_addrmap_clear(struct addrmap* t) {
  caml_stat_free(t->entries);
  t->entries = 0;
  t->size = 0;
}

void caml_addrmap_initialize(struct addrmap *t) {
  if (!t->entries) {
    /* first call, initialise table with a small initial size */
    addrmap_alloc(t, caml_init_intern_addrmap_size);
  }
}

value* caml_addrmap_insert_pos(struct addrmap* t, value key) {
  uintnat i, pos , old_size;
  struct addrmap_entry* old_table;

  CAMLassert(Is_block(key));

  for (i = 0, pos = pos_initial(t, key);
       i < MAX_CHAIN;
       i++,   pos = pos_next(t, pos)) {
    if (t->entries[pos].key == ADDRMAP_INVALID_KEY) {
      t->entries[pos].key = key;
    }
    if (t->entries[pos].key == key) {
      return &t->entries[pos].value;
    }
  }

  /* failed to insert, rehash and try again */
  old_table = t->entries;
  old_size = t->size;
  addrmap_alloc(t, old_size * 2);
  for (i = 0; i < old_size; i++) {
    if (old_table[i].key != ADDRMAP_INVALID_KEY) {
      value* p = caml_addrmap_insert_pos(t, old_table[i].key);
      CAMLassert(*p == ADDRMAP_NOT_PRESENT);
      *p = old_table[i].value;
    }
  }
  caml_stat_free(old_table);
  return caml_addrmap_insert_pos(t, key);
}
