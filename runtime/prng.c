/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy, projet Cambium, College de France and Inria     */
/*                                                                        */
/*   Copyright 2021 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/mlvalues.h"

/* The Xoshiro256++ pseudo-random number generator */
/* Adapted from https://prng.di.unimi.it/xoshiro256plusplus.c */
/* Original code written in 2019 by David Blackman and Sebastiano Vigna */
/* Original code dedicated to the public domain
   https://creativecommons.org/publicdomain/zero/1.0/  */

typedef uint64_t xoshiro_state[4];

static inline uint64_t rotl(const uint64_t x, int k) {
  return (x << k) | (x >> (64 - k));
}

static inline uint64_t next(uint64_t s[4])
{
  const uint64_t result = rotl(s[0] + s[3], 23) + s[0];
  const uint64_t t = s[1] << 17;
  s[2] ^= s[0];
  s[3] ^= s[1];
  s[1] ^= s[2];
  s[0] ^= s[3];
  s[2] ^= t;
  s[3] = rotl(s[3], 45);
  return result;
}


/* This is the jump function for the generator. It is equivalent
   to 2^128 calls to next(); it can be used to generate 2^128
   non-overlapping subsequences for parallel computations. */

static inline void jump(xoshiro_state s)
{
  static const uint64_t JUMP[] =
    { 0x180ec6d33cfd0aba, 0xd5a61266f0c9392c,
      0xa9582618e03fc9aa, 0x39abdc4529b1661c };
  uint64_t s0 = 0;
  uint64_t s1 = 0;
  uint64_t s2 = 0;
  uint64_t s3 = 0;
  for(int i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
    for(int b = 0; b < 64; b++) {
      if (JUMP[i] & (INT64_LITERAL(1) << b)) {
        s0 ^= s[0];
        s1 ^= s[1];
        s2 ^= s[2];
        s3 ^= s[3];
      }
      next(s);	
    }
  s[0] = s0;
  s[1] = s1;
  s[2] = s2;
  s[3] = s3;
}

/* This is the long-jump function for the generator. It is equivalent to
   2^192 calls to next(); it can be used to generate 2^64 starting points,
   from each of which jump() will generate 2^64 non-overlapping
   subsequences for parallel distributed computations. */

static inline void long_jump(xoshiro_state s)
{
  static const uint64_t LONG_JUMP[] =
    { 0x76e15d3efefdcbbf, 0xc5004e441c522fb3,
      0x77710069854ee241, 0x39109bb02acbe635 };
  uint64_t s0 = 0;
  uint64_t s1 = 0;
  uint64_t s2 = 0;
  uint64_t s3 = 0;
  for(int i = 0; i < sizeof LONG_JUMP / sizeof *LONG_JUMP; i++)
    for(int b = 0; b < 64; b++) {
      if (LONG_JUMP[i] & (INT64_LITERAL(1) << b)) {
        s0 ^= s[0];
        s1 ^= s[1];
        s2 ^= s[2];
        s3 ^= s[3];
      }
      next(s);	
    }
  s[0] = s0;
  s[1] = s1;
  s[2] = s2;
  s[3] = s3;
}

/* Wrapping Xoshiro256++ as primitives operating over an OCaml bigarray */

#define Xoshiro_val(v) ((uint64_t *) Caml_ba_data_val(v))

CAMLprim uint64_t caml_xoshiro_next_unboxed(value vs)
{
  return next(Xoshiro_val(vs));
}

CAMLprim value caml_xoshiro_next(value vs)
{
  return caml_copy_int64(caml_xoshiro_next_unboxed(vs));
}

CAMLprim value caml_xoshiro_jump(value vs)
{
  jump(Xoshiro_val(vs));
  return Val_unit;
}

CAMLprim value caml_xoshiro_long_jump(value vs)
{
  long_jump(Xoshiro_val(vs));
  return Val_unit;
}

CAMLprim value caml_xoshiro_init(value vs, value vinit)
{
  uint64_t * s = Xoshiro_val(vs);
  mlsize_t len = Wosize_val(vinit);
  const uint64_t mix = 6364136223846793005;
  /* Multiplier taken from the MMIX LCG, Knoth TAOCP vol 2, 1998 edition */

  s[0] = s[1] = s[2] = s[3] = 0;
  for (mlsize_t i = 0; i < len; i++) {
    s[i % 4] = s[i % 4] * mix + Long_val(Field(vinit, i));
  }
  for (int i = 0; i < 4; i++) {
    if (s[i] == 0) s[i] = 1 << i;
  }
  return Val_unit;
}
