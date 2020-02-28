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

void caml_addrmap_initialize(struct addrmap* t);

#endif /* CAML_INTERNALS */

#endif /* CAML_ADDRMAP_H */
