// Adapted from RFC 7693 https://datatracker.ietf.org/doc/html/rfc7693
// Self test Module for BLAKE2b

#define CAML_NAME_SPACE

#include <caml/mlvalues.h>
#include <caml/memory.h>
#define CAML_INTERNALS
#include <caml/blake2.h>
#undef CAML_INTERNALS

#include <stdio.h>
#include <stdint.h>

// Deterministic sequences (Fibonacci generator).

static void selftest_seq(uint8_t *out, size_t len, uint32_t seed)
{
  uint32_t t, a , b;

  a = 0xDEAD4BAD * seed;              // prime
  b = 1;

  for (size_t i = 0; i < len; i++) {         // fill the buf
    t = a + b;
    a = b;
    b = t;
    out[i] = (t >> 24) & 0xFF;
  }
}

// All-in-one convenience function.
static int blake2b(void *out, size_t outlen,   // return buffer for digest
    const void *key, size_t keylen,            // optional secret key
    const void *in, size_t inlen)              // data to be hashed
{
  struct BLAKE2_context ctx;

  caml_BLAKE2Init(&ctx, outlen, keylen, key);
  caml_BLAKE2Update(&ctx, in, inlen);
  caml_BLAKE2Final(&ctx, outlen, out);

  return 0;
}

// BLAKE2b self-test validation. Return 0 when OK.

int blake2b_selftest()
{
  // grand hash of hash results
  const uint8_t blake2b_res[32] = {
    0xC2, 0x3A, 0x78, 0x00, 0xD9, 0x81, 0x23, 0xBD,
    0x10, 0xF5, 0x06, 0xC6, 0x1E, 0x29, 0xDA, 0x56,
    0x03, 0xD7, 0x63, 0xB8, 0xBB, 0xAD, 0x2E, 0x73,
    0x7F, 0x5E, 0x76, 0x5A, 0x7B, 0xCC, 0xD4, 0x75
  };
  // parameter sets
  const size_t b2b_md_len[4] = { 20, 32, 48, 64 };
  const size_t b2b_in_len[6] = { 0, 3, 128, 129, 255, 1024 };

  size_t outlen, inlen;
  uint8_t in[1024], md[64], key[64];
  struct BLAKE2_context ctx;

  // 256-bit hash for testing
  caml_BLAKE2Init(&ctx, 32, 0, NULL);

  for (size_t i = 0; i < 4; i++) {
    outlen = b2b_md_len[i];
    for (size_t j = 0; j < 6; j++) {
      inlen = b2b_in_len[j];

      selftest_seq(in, inlen, inlen);     // unkeyed hash
      blake2b(md, outlen, NULL, 0, in, inlen);
      caml_BLAKE2Update(&ctx, md, outlen);   // hash the hash

      selftest_seq(key, outlen, outlen);  // keyed hash
      blake2b(md, outlen, key, outlen, in, inlen);
      caml_BLAKE2Update(&ctx, md, outlen);   // hash the hash
    }
  }

  // compute and compare the hash of hashes
  caml_BLAKE2Final(&ctx, 32, md);
  for (size_t i = 0; i < 32; i++) {
    if (md[i] != blake2b_res[i])
      return -1;
  }

  return 0;
}

int self_test_main(value unused)
{
  CAMLparam1(unused);
  printf("blake2b_selftest() = %s\n",
      blake2b_selftest() ? "FAIL" : "OK");

  CAMLreturn (Val_unit);
}
