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

#define Page(p) ((uintnat) (p) >> 2)

/* Multiplicative Fibonacci hashing
   (Knuth, TAOCP vol 3, section 6.4, page 518).
   HASH_FACTOR is (sqrt(5) - 1) / 2 * 2^wordsize. */
#ifdef ARCH_SIXTYFOUR
#define HASH_FACTOR 11400714819323198486UL
#else
#define HASH_FACTOR 2654435769UL
#endif
#define Hash(key,shift) (((key) * HASH_FACTOR) >> shift)

struct addrmap_entry { value key, value; };

struct addrmap_page_table {
  mlsize_t size;                   /* size == 1 << (wordsize - shift) */
  int shift;
  mlsize_t mask;                   /* mask == size - 1 */
  mlsize_t occupancy;
  struct addrmap_entry* entries;   /* [size]  */
};

value* caml_addrmap_lookup(struct addrmap_page_table* t, value key);

#define ADDRMAP_NOT_PRESENT ((value)(0))
#define ADDRMAP_INVALID_KEY ((value)(0))

value* caml_addrmap_insert_pos(struct addrmap_page_table* t, value v);

void caml_addrmap_clear(struct addrmap_page_table* t);

void caml_addrmap_initialize(struct addrmap_page_table* t);

void display_addrmap(struct addrmap_page_table* t);
value* addrmap_page_table_lookup(struct addrmap_page_table* t, value key);
int addrmap_page_table_resize(struct addrmap_page_table* t);

#endif /* CAML_INTERNALS */

#endif /* CAML_ADDRMAP_H */
