/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Xavier Leroy, Collège de France and Inria Paris              */
/*                                                                        */
/*   Copyright 2022 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include "caml/alloc.h"
#include "caml/blake2.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

/* BLAKE2 message digest */

static inline uint64_t U8TO64LE(const unsigned char * src)
{
  return (uint64_t) src[0]         | ((uint64_t) src[1] << 8)
       | ((uint64_t) src[2] << 16) | ((uint64_t) src[3] << 24)
       | ((uint64_t) src[4] << 32) | ((uint64_t) src[5] << 40)
       | ((uint64_t) src[6] << 48) | ((uint64_t) src[7] << 56);
}

static inline uint64_t ROTR64(uint64_t x, int amount)
{
  return (x >> amount) | (x << (64 - amount));
}

static const uint64_t caml_BLAKE2_iv[8] = {
  0x6a09e667f3bcc908,
  0xbb67ae8584caa73b,
  0x3c6ef372fe94f82b,
  0xa54ff53a5f1d36f1,
  0x510e527fade682d1,
  0x9b05688c2b3e6c1f,
  0x1f83d9abfb41bd6b,
  0x5be0cd19137e2179
};

static const uint8_t BLAKE2_sigma[12][16] = {
  {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 },
  { 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 },
  { 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 },
  {  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 },
  {  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 },
  {  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 },
  { 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 },
  { 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 },
  {  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 },
  { 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13,  0 },
  {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 },
  { 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 }
};

#define MIX2B(a,b,c,d,x,y)                                                  \
  do {                                                                      \
    a += b + x;                                                             \
    d = ROTR64(d ^ a, 32);                                                  \
    c += d;                                                                 \
    b = ROTR64(b ^ c, 24);                                                  \
    a += b + y;                                                             \
    d = ROTR64(d ^ a, 16);                                                  \
    c += d;                                                                 \
    b = ROTR64(b ^ c, 63);                                                  \
  } while(0)                                                                \

static void
caml_BLAKE2Compress(struct BLAKE2_context * s,
                    const unsigned char * data, size_t numbytes,
                    int is_last_block)
{
  uint64_t v0, v1, v2, v3, v4, v5, v6, v7,
           v8, v9, v10, v11, v12, v13, v14, v15;
  uint64_t m[16];

  /* Update the length */
  s->len[0] += numbytes;
  if (s->len[0] < numbytes) s->len[1]++; /* carry */
  /* Initialize work space */
  v0 = s->h[0];  v1 = s->h[1];
  v2 = s->h[2];  v3 = s->h[3];
  v4 = s->h[4];  v5 = s->h[5];
  v6 = s->h[6];  v7 = s->h[7];
  v8 = caml_BLAKE2_iv[0];  v9 = caml_BLAKE2_iv[1];
  v10 = caml_BLAKE2_iv[2]; v11 = caml_BLAKE2_iv[3];
  v12 = caml_BLAKE2_iv[4] ^ s->len[0];  v13 = caml_BLAKE2_iv[5] ^ s->len[1];
  v14 = is_last_block ? ~ caml_BLAKE2_iv[6] : caml_BLAKE2_iv[6];
  v15 = caml_BLAKE2_iv[7];
  /* Convert data to 16 64-bit words */
  for (int i = 0; i < 16; i++) {
    m[i] = U8TO64LE(data + i * 8);
  }
  /* Twelve rounds of mixing */
  for (int i = 0; i < 12; i++) {
    const uint8_t * sigma = BLAKE2_sigma[i];
    MIX2B(v0, v4, v8,  v12, m[sigma[0]],  m[sigma[1]]);
    MIX2B(v1, v5, v9,  v13, m[sigma[2]],  m[sigma[3]]);
    MIX2B(v2, v6, v10, v14, m[sigma[4]],  m[sigma[5]]);
    MIX2B(v3, v7, v11, v15, m[sigma[6]],  m[sigma[7]]);
    MIX2B(v0, v5, v10, v15, m[sigma[8]],  m[sigma[9]]);
    MIX2B(v1, v6, v11, v12, m[sigma[10]], m[sigma[11]]);
    MIX2B(v2, v7, v8,  v13, m[sigma[12]], m[sigma[13]]);
    MIX2B(v3, v4, v9,  v14, m[sigma[14]], m[sigma[15]]);
  }
  /* Update state  */
  s->h[0] ^= v0 ^ v8;   s->h[1] ^= v1 ^ v9;
  s->h[2] ^= v2 ^ v10;  s->h[3] ^= v3 ^ v11;
  s->h[4] ^= v4 ^ v12;  s->h[5] ^= v5 ^ v13;
  s->h[6] ^= v6 ^ v14;  s->h[7] ^= v7 ^ v15;
}

CAMLexport void
caml_BLAKE2Init(struct BLAKE2_context * s,
                size_t hashlen,
                size_t keylen, const unsigned char * key)
{
  CAMLassert (hashlen <= 64);
  for (int i = 0; i < 8; i++) s->h[i] = caml_BLAKE2_iv[i];
  s->h[0] ^= 0x01010000 | (keylen << 8) | hashlen;
  s->len[0] = s->len[1] = 0;
  s->numbytes = 0;
  /* If key was supplied, pad to 128 bytes and prepend to message */
  if (keylen > 0) {
    if (keylen > 64) keylen = 64;
    memcpy(s->buffer, key, keylen);
    memset(s->buffer + keylen, 0, BLAKE2_BLOCKSIZE - keylen);
    s->numbytes = BLAKE2_BLOCKSIZE;
  }
}

CAMLexport void
caml_BLAKE2Update(struct BLAKE2_context * s,
                  const unsigned char * data, size_t len)
{
  /* If data was left in buffer, pad it with fresh data and compress */
  if (s->numbytes > 0) {
    size_t n = BLAKE2_BLOCKSIZE - s->numbytes;
    if (len <= n) {
      /* Not enough fresh data to compress.  Buffer the data. */
      memcpy(s->buffer + s->numbytes, data, len);
      s->numbytes += len;
      return;
    }
    memcpy(s->buffer + s->numbytes, data, n);
    caml_BLAKE2Compress(s, s->buffer, BLAKE2_BLOCKSIZE, 0);
    data += n; len -= n;
  }
  /* Process data by blocks of BLAKE2_BLOCKSIZE */
  while (len > BLAKE2_BLOCKSIZE) {
    caml_BLAKE2Compress(s, data, BLAKE2_BLOCKSIZE, 0);
    data += BLAKE2_BLOCKSIZE; len -= BLAKE2_BLOCKSIZE;
  }
  /* Save remaining data, up to one full block.  This is because the
     last block is treated specially in caml_BLAKE2Final. */
  memcpy(s->buffer, data, len);
  s->numbytes = len;
}

CAMLexport void
caml_BLAKE2Final(struct BLAKE2_context * s,
                 size_t hashlen, unsigned char * hash)
{
  CAMLassert(0 < hashlen);
  CAMLassert(hashlen <= 64);
  /* The final block is composed of the remaining data padded with zeros. */
  memset(s->buffer + s->numbytes, 0, BLAKE2_BLOCKSIZE - s->numbytes);
  caml_BLAKE2Compress(s, s->buffer, s->numbytes, 1);
  /* Extract the hash */
  for (unsigned int i = 0; i < hashlen; i++) {
    hash[i] = s->h[i / 8] >> (8 * (i % 8));
  }
}

/* OCaml wrappers */

#define BLAKE2_context_val(v) (*((struct BLAKE2_context **) Data_custom_val(v)))

static void caml_blake2_finalize(value ctx)
{
  caml_stat_free(BLAKE2_context_val(ctx));
}

static struct custom_operations caml_blake2_operations = {
  "_blake2",
  caml_blake2_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_blake2_create(value hashlen, value key)
{
  CAMLparam1(key);
  struct BLAKE2_context * ctx =
    caml_stat_alloc(sizeof(struct BLAKE2_context));
  value res =
    caml_alloc_custom_mem(&caml_blake2_operations,
                          sizeof(struct BLAKE2_context *),
                          sizeof(struct BLAKE2_context));
  caml_BLAKE2Init(ctx, Long_val(hashlen),
                  caml_string_length(key), &Byte_u(key, 0));
  BLAKE2_context_val(res) = ctx;
  CAMLreturn(res);
}

CAMLprim value caml_blake2_update(value ctx, value buf, value ofs, value len)
{
  caml_BLAKE2Update(BLAKE2_context_val(ctx),
                    &Byte_u(buf, Long_val(ofs)), Long_val(len));
  return Val_unit;
}

CAMLprim value caml_blake2_final(value ctx, value hashlen)
{
  CAMLparam1(ctx);
  size_t len = Long_val(hashlen);
  value hash = caml_alloc_string(len);
  caml_BLAKE2Final(BLAKE2_context_val(ctx), len, &Byte_u(hash, 0));
  CAMLreturn(hash);
}

CAMLprim value caml_blake2_string(value hashlen, value key,
                                  value buf, value ofs, value len)
{
  struct BLAKE2_context ctx;
  size_t hlen = Long_val(hashlen);
  caml_BLAKE2Init(&ctx, hlen, caml_string_length(key), &Byte_u(key, 0));
  caml_BLAKE2Update(&ctx, &Byte_u(buf, Long_val(ofs)), Long_val(len));
  value hash = caml_alloc_string(hlen);
  caml_BLAKE2Final(&ctx, hlen, &Byte_u(hash, 0));
  return hash;
}

CAMLprim value caml_blake2_bytes(value hashlen, value key,
                                  value buf, value ofs, value len)
{
  return caml_blake2_string(hashlen, key, buf, ofs, len);
}
