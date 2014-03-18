#include "mlvalues.h"

#ifndef CAML_ADDRMAP_H
#define CAML_ADDRMAP_H

/* An addrmap is a value -> value hashmap, where
   the values are blocks */

struct addrmap_entry { value key, value; };
struct addrmap {
  struct addrmap_entry* entries;
  uintnat size;
};

#define ADDRMAP_INIT {0,0}


value caml_addrmap_lookup(struct addrmap* t, value v);

#define ADDRMAP_NOT_PRESENT ((value)(0))

value* caml_addrmap_insert_pos(struct addrmap* t, value v);


void caml_addrmap_clear(struct addrmap* t);

#endif
