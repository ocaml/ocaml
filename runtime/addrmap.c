/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2015 University of Cambridge                               */
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

#define MAX_CHAIN 100

Caml_inline uintnat pos_initial(struct addrmap* t, value key)
{
  uintnat pos = (uintnat)key;
  pos *= 0xcc9e2d51;
  pos ^= (pos >> 17);

  CAMLassert(Is_power_of_2(t->size));
  return pos & (t->size - 1);
}

Caml_inline uintnat pos_next(struct addrmap* t, uintnat pos)
{
  return (pos + 1) & (t->size - 1);
}

int caml_addrmap_contains(struct addrmap* t, value key)
{
  uintnat pos, i;

  CAMLassert(Is_block(key));
  if (!t->entries) return 0;

  for (i = 0, pos = pos_initial(t, key);
       i < MAX_CHAIN;
       i++, pos = pos_next(t, pos)) {
    if (t->entries[pos].key == ADDRMAP_INVALID_KEY) break;
    if (t->entries[pos].key == key) return 1;
  }
  return 0;
}

value caml_addrmap_lookup(struct addrmap* t, value key)
{
  uintnat pos;

  CAMLassert(Is_block(key));
  CAMLassert(t->entries);

  for (pos = pos_initial(t, key); ; pos = pos_next(t, pos)) {
    CAMLassert(t->entries[pos].key != ADDRMAP_INVALID_KEY);
    if (t->entries[pos].key == key)
      return t->entries[pos].value;
  }
}

static void addrmap_alloc(struct addrmap* t, uintnat sz)
{
  uintnat i;
  CAMLassert(Is_power_of_2(sz));
  t->entries = caml_stat_alloc(sizeof(struct addrmap_entry) * sz);
  t->size = sz;
  for (i = 0; i < sz; i++) {
    t->entries[i].key = ADDRMAP_INVALID_KEY;
    t->entries[i].value = ADDRMAP_NOT_PRESENT;
  }
}

void caml_addrmap_init(struct addrmap* t) {
  t->entries = NULL;
  t->size = 0;
}

void caml_addrmap_clear(struct addrmap* t) {
  caml_stat_free(t->entries);
  t->entries = NULL;
  t->size = 0;
}



value* caml_addrmap_insert_pos(struct addrmap* t, value key) {
  uintnat i, pos;
  CAMLassert(Is_block(key));
  if (!t->entries) {
    /* first call, initialise table with a small initial size */
    addrmap_alloc(t, 256);
  }
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
  {
    struct addrmap_entry* old_table = t->entries;
    uintnat old_size = t->size;
    addrmap_alloc(t, old_size * 2);
    for (i = 0; i < old_size; i++) {
      if (old_table[i].key != ADDRMAP_INVALID_KEY) {
        value* p = caml_addrmap_insert_pos(t, old_table[i].key);
        CAMLassert(*p == ADDRMAP_NOT_PRESENT);
        *p = old_table[i].value;
      }
    }
    caml_stat_free(old_table);
  }
  return caml_addrmap_insert_pos(t, key);
}

void caml_addrmap_insert(struct addrmap* t, value k, value v) {
  value* p = caml_addrmap_insert_pos(t, k);
  CAMLassert(*p == ADDRMAP_NOT_PRESENT);
  *p = v;
}

void caml_addrmap_iter(struct addrmap* t, void (*f)(value, value)) {
  addrmap_iterator i;
  for (i = caml_addrmap_iterator(t);
       caml_addrmap_iter_ok(t, i);
       i = caml_addrmap_next(t, i)) {
    f(caml_addrmap_iter_key(t, i),
      caml_addrmap_iter_value(t, i));
  }
}
