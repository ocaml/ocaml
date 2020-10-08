/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2011 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Auxiliary functions for custom hash functions */

#ifndef CAML_HASH_H
#define CAML_HASH_H

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern uint32_t caml_hash_mix_uint32(uint32_t h, uint32_t d);
CAMLextern uint32_t caml_hash_mix_intnat(uint32_t h, intnat d);
CAMLextern uint32_t caml_hash_mix_int64(uint32_t h, int64_t d);
CAMLextern uint32_t caml_hash_mix_double(uint32_t h, double d);
CAMLextern uint32_t caml_hash_mix_float(uint32_t h, float d);
CAMLextern uint32_t caml_hash_mix_string(uint32_t h, value s);

struct caml_hash_state; /* opaque struct, passed by reference */

CAMLextern void caml_hash_add_header(struct caml_hash_state * st, 
                                     uintnat sz, uint8_t tag);
CAMLextern void caml_hash_add_uint64(struct caml_hash_state * st, uint64_t n);
CAMLextern void caml_hash_add_double(struct caml_hash_state * st, double d);
CAMLextern void caml_hash_add_string(struct caml_hash_state * st,
                                     const uint8_t * s, uintnat len);

Caml_inline void caml_hash_add_intnat(struct caml_hash_state * st, intnat n)
{
  /* Force sign extension to 64-bits to make sure that negative numbers
     representable in 32 bits produce the same hash
     on 32 and 64 bit platforms. */
 caml_hash_add_uint64(st, (int64_t) n);
}

Caml_inline void caml_hash_add_float(struct caml_hash_state * st, float f)
{ caml_hash_add_double(st, (double) f); }

#ifdef __cplusplus
}
#endif


#endif /* CAML_HASH_H */
