/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2011 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Auxiliary functions for custom hash functions */

#ifndef CAML_HASH_H
#define CAML_HASH_H

#include "mlvalues.h"

typedef uint32 hash_key;
typedef uint32 hash_out;

typedef uint32 *hash_t;
#define CAML_HASH_T_SIZE sizeof(uint32)

CAMLextern hash_t caml_hash_init(void* h, hash_key k);
CAMLextern hash_out caml_hash_final(hash_t h);

CAMLextern void caml_hash_mix_uint32(hash_t h, uint32 d);
CAMLextern void caml_hash_mix_intnat(hash_t h, intnat d);
CAMLextern void caml_hash_mix_int64(hash_t h, int64 d);
CAMLextern void caml_hash_mix_double(hash_t h, double d);
CAMLextern void caml_hash_mix_float(hash_t h, float d);
CAMLextern void caml_hash_mix_string(hash_t h, value s);

#endif
