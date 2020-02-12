/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
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

#ifndef CAML_ADDRMAP_H
#define CAML_ADDRMAP_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"

/* An addrmap is a value -> value hashmap, where
   the values are blocks */

struct addrmap_entry { value key, value; };
struct addrmap {
  struct addrmap_entry* entries;
  uintnat size;
};

#define ADDRMAP_INIT {0,0}

int caml_addrmap_contains(struct addrmap* t, value v);
value caml_addrmap_lookup(struct addrmap* t, value v);

#define ADDRMAP_NOT_PRESENT ((value)(0))
#define ADDRMAP_INVALID_KEY ((value)(0))

value* caml_addrmap_insert_pos(struct addrmap* t, value v);

/* must not already be present */
void caml_addrmap_insert(struct addrmap* t, value k, value v);

void caml_addrmap_clear(struct addrmap* t);

void caml_addrmap_iter(struct addrmap* t, void (*f)(value, value));

/* iteration */
typedef uintnat addrmap_iterator;
static inline addrmap_iterator caml_addrmap_iter_ok(struct addrmap* t,
                                                    addrmap_iterator i)
{
  if (i < t->size) {
    CAMLassert(t->entries[i].key != ADDRMAP_INVALID_KEY);
    return 1;
  } else {
    return 0;
  }
}

static inline addrmap_iterator caml_addrmap_next(struct addrmap* t,
                                                 addrmap_iterator i)
{
  if (!t->entries) return (uintnat)(-1);
  i++;
  while (i < t->size && t->entries[i].key == ADDRMAP_INVALID_KEY) {
    i++;
  }
  caml_addrmap_iter_ok(t, i); /* just for assert-checks */
  return i;
}

static inline value caml_addrmap_iter_key(struct addrmap* t,
                                          addrmap_iterator i)
{
  CAMLassert(caml_addrmap_iter_ok(t, i));
  return t->entries[i].key;
}

static inline value caml_addrmap_iter_value(struct addrmap* t,
                                            addrmap_iterator i)
{
  CAMLassert(caml_addrmap_iter_ok(t, i));
  return t->entries[i].value;
}

static inline value* caml_addrmap_iter_val_pos(struct addrmap* t,
                                               addrmap_iterator i)
{
  CAMLassert(caml_addrmap_iter_ok(t, i));
  return &t->entries[i].value;
}

static inline addrmap_iterator caml_addrmap_iterator(struct addrmap* t)
{
  return caml_addrmap_next(t, (uintnat)(-1));
}

#endif /* CAML_INTERNALS */

#endif /* CAML_ADDRMAP_H */
