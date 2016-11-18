/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdio.h>
#include <string.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

static char * parse_sign_and_base(char * p,
                                  /*out*/ int * base,
                                  /*out*/ int * signedness,
                                  /*out*/ int * sign)
{
  *sign = 1;
  if (*p == '-') {
    *sign = -1;
    p++;
  } else if (*p == '+')
    p++;
  *base = 10; *signedness = 1;
  if (*p == '0') {
    switch (p[1]) {
    case 'x': case 'X':
      *base = 16; *signedness = 0; p += 2; break;
    case 'o': case 'O':
      *base = 8; *signedness = 0; p += 2; break;
    case 'b': case 'B':
      *base = 2; *signedness = 0; p += 2; break;
    case 'u': case 'U':
      *signedness = 0; p += 2; break;
    }
  }
  return p;
}

static int parse_digit(char c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else
    return -1;
}

#define INT_ERRMSG "int_of_string"
#define INT32_ERRMSG "Int32.of_string"
#define INT64_ERRMSG "Int64.of_string"
#define INTNAT_ERRMSG "Nativeint.of_string"

static intnat parse_intnat(value s, int nbits, const char *errmsg)
{
  char * p;
  uintnat res, threshold;
  int sign, base, signedness, d;

  p = parse_sign_and_base(String_val(s), &base, &signedness, &sign);
  threshold = ((uintnat) -1) / base;
  d = parse_digit(*p);
  if (d < 0 || d >= base) caml_failwith(errmsg);
  for (p++, res = d; /*nothing*/; p++) {
    char c = *p;
    if (c == '_') continue;
    d = parse_digit(c);
    if (d < 0 || d >= base) break;
    /* Detect overflow in multiplication base * res */
    if (res > threshold) caml_failwith(errmsg);
    res = base * res + d;
    /* Detect overflow in addition (base * res) + d */
    if (res < (uintnat) d) caml_failwith(errmsg);
  }
  if (p != String_val(s) + caml_string_length(s)){
    caml_failwith(errmsg);
  }
  if (signedness) {
    /* Signed representation expected, allow -2^(nbits-1) to 2^(nbits-1) - 1 */
    if (sign >= 0) {
      if (res >= (uintnat)1 << (nbits - 1)) caml_failwith(errmsg);
    } else {
      if (res >  (uintnat)1 << (nbits - 1)) caml_failwith(errmsg);
    }
  } else {
    /* Unsigned representation expected, allow 0 to 2^nbits - 1
       and tolerate -(2^nbits - 1) to 0 */
    if (nbits < sizeof(uintnat) * 8 && res >= (uintnat)1 << nbits)
      caml_failwith(errmsg);
  }
  return sign < 0 ? -((intnat) res) : (intnat) res;
}

value caml_bswap16_direct(value x)
{
  return ((((x & 0x00FF) << 8) |
           ((x & 0xFF00) >> 8)));
}

CAMLprim value caml_bswap16(value v)
{
  intnat x = Int_val(v);
  return (Val_int ((((x & 0x00FF) << 8) |
                    ((x & 0xFF00) >> 8))));
}

/* Tagged integers */

CAMLprim value caml_int_compare(value v1, value v2)
{
  int res = (v1 > v2) - (v1 < v2);
  return Val_int(res);
}

CAMLprim value caml_int_of_string(value s)
{
    return Val_long(parse_intnat(s, 8 * sizeof(value) - 1, INT_ERRMSG));
}

#define FORMAT_BUFFER_SIZE 32

static char parse_format(value fmt,
                         char * suffix,
                         char format_string[FORMAT_BUFFER_SIZE])
{
  char * p;
  char lastletter;
  mlsize_t len, len_suffix;

  /* Copy OCaml format fmt to format_string,
     adding the suffix before the last letter of the format */
  len = caml_string_length(fmt);
  len_suffix = strlen(suffix);
  if (len + len_suffix + 1 >= FORMAT_BUFFER_SIZE)
    caml_invalid_argument("format_int: format too long");
  memmove(format_string, String_val(fmt), len);
  p = format_string + len - 1;
  lastletter = *p;
  /* Compress two-letter formats, ignoring the [lnL] annotation */
  if (p[-1] == 'l' || p[-1] == 'n' || p[-1] == 'L') p--;
  memmove(p, suffix, len_suffix);  p += len_suffix;
  *p++ = lastletter;
  *p = 0;
  /* Return the conversion type (last letter) */
  return lastletter;
}

CAMLprim value caml_format_int(value fmt, value arg)
{
  char format_string[FORMAT_BUFFER_SIZE];
  char conv;
  value res;

  conv = parse_format(fmt, ARCH_INTNAT_PRINTF_FORMAT, format_string);
  switch (conv) {
  case 'u': case 'x': case 'X': case 'o':
    res = caml_alloc_sprintf(format_string, Unsigned_long_val(arg));
    break;
  default:
    res = caml_alloc_sprintf(format_string, Long_val(arg));
    break;
  }
  return res;
}

/* 32-bit integers */

static int int32_cmp(value v1, value v2)
{
  int32_t i1 = Int32_val(v1);
  int32_t i2 = Int32_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intnat int32_hash(value v)
{
  return Int32_val(v);
}

static void int32_serialize(value v, uintnat * bsize_32,
                            uintnat * bsize_64)
{
  caml_serialize_int_4(Int32_val(v));
  *bsize_32 = *bsize_64 = 4;
}

static uintnat int32_deserialize(void * dst)
{
  *((int32_t *) dst) = caml_deserialize_sint_4();
  return 4;
}

CAMLexport struct custom_operations caml_int32_ops = {
  "_i",
  custom_finalize_default,
  int32_cmp,
  int32_hash,
  int32_serialize,
  int32_deserialize,
  custom_compare_ext_default
};

CAMLexport value caml_copy_int32(int32_t i)
{
  value res = caml_alloc_custom(&caml_int32_ops, 4, 0, 1);
  Int32_val(res) = i;
  return res;
}

CAMLprim value caml_int32_neg(value v)
{ return caml_copy_int32(- Int32_val(v)); }

CAMLprim value caml_int32_add(value v1, value v2)
{ return caml_copy_int32(Int32_val(v1) + Int32_val(v2)); }

CAMLprim value caml_int32_sub(value v1, value v2)
{ return caml_copy_int32(Int32_val(v1) - Int32_val(v2)); }

CAMLprim value caml_int32_mul(value v1, value v2)
{ return caml_copy_int32(Int32_val(v1) * Int32_val(v2)); }

CAMLprim value caml_int32_div(value v1, value v2)
{
  int32_t dividend = Int32_val(v1);
  int32_t divisor = Int32_val(v2);
  if (divisor == 0) caml_raise_zero_divide();
  /* PR#4740: on some processors, division crashes on overflow.
     Implement the same behavior as for type "int". */
  if (dividend == (1<<31) && divisor == -1) return v1;
  return caml_copy_int32(dividend / divisor);
}

CAMLprim value caml_int32_mod(value v1, value v2)
{
  int32_t dividend = Int32_val(v1);
  int32_t divisor = Int32_val(v2);
  if (divisor == 0) caml_raise_zero_divide();
  /* PR#4740: on some processors, modulus crashes if division overflows.
     Implement the same behavior as for type "int". */
  if (dividend == (1<<31) && divisor == -1) return caml_copy_int32(0);
  return caml_copy_int32(dividend % divisor);
}

CAMLprim value caml_int32_and(value v1, value v2)
{ return caml_copy_int32(Int32_val(v1) & Int32_val(v2)); }

CAMLprim value caml_int32_or(value v1, value v2)
{ return caml_copy_int32(Int32_val(v1) | Int32_val(v2)); }

CAMLprim value caml_int32_xor(value v1, value v2)
{ return caml_copy_int32(Int32_val(v1) ^ Int32_val(v2)); }

CAMLprim value caml_int32_shift_left(value v1, value v2)
{ return caml_copy_int32(Int32_val(v1) << Int_val(v2)); }

CAMLprim value caml_int32_shift_right(value v1, value v2)
{ return caml_copy_int32(Int32_val(v1) >> Int_val(v2)); }

CAMLprim value caml_int32_shift_right_unsigned(value v1, value v2)
{ return caml_copy_int32((uint32_t)Int32_val(v1) >> Int_val(v2)); }

static int32_t caml_swap32(int32_t x)
{
  return (((x & 0x000000FF) << 24) |
          ((x & 0x0000FF00) << 8) |
          ((x & 0x00FF0000) >> 8) |
          ((x & 0xFF000000) >> 24));
}

value caml_int32_direct_bswap(value v)
{ return caml_swap32(v); }

CAMLprim value caml_int32_bswap(value v)
{ return caml_copy_int32(caml_swap32(Int32_val(v))); }

CAMLprim value caml_int32_of_int(value v)
{ return caml_copy_int32(Long_val(v)); }

CAMLprim value caml_int32_to_int(value v)
{ return Val_long(Int32_val(v)); }

int32_t caml_int32_of_float_unboxed(double x)
{ return x; }

CAMLprim value caml_int32_of_float(value v)
{ return caml_copy_int32((int32_t)(Double_val(v))); }

double caml_int32_to_float_unboxed(int32_t x)
{ return x; }

CAMLprim value caml_int32_to_float(value v)
{ return caml_copy_double((double)(Int32_val(v))); }

intnat caml_int32_compare_unboxed(int32_t i1, int32_t i2)
{
  return (i1 > i2) - (i1 < i2);
}

CAMLprim value caml_int32_compare(value v1, value v2)
{
  return Val_int(caml_int32_compare_unboxed(Int32_val(v1),Int32_val(v2)));
}

CAMLprim value caml_int32_format(value fmt, value arg)
{
  char format_string[FORMAT_BUFFER_SIZE];

  parse_format(fmt, ARCH_INT32_PRINTF_FORMAT, format_string);
  return caml_alloc_sprintf(format_string, Int32_val(arg));
}

CAMLprim value caml_int32_of_string(value s)
{
  return caml_copy_int32(parse_intnat(s, 32, INT32_ERRMSG));
}

int32_t caml_int32_bits_of_float_unboxed(double d)
{
  union { float d; int32_t i; } u;
  u.d = d;
  return u.i;
}

double caml_int32_float_of_bits_unboxed(int32_t i)
{
  union { float d; int32_t i; } u;
  u.i = i;
  return u.d;
}

CAMLprim value caml_int32_bits_of_float(value vd)
{
  return caml_copy_int32(caml_int32_bits_of_float_unboxed(Double_val(vd)));
}

CAMLprim value caml_int32_float_of_bits(value vi)
{
  return caml_copy_double(caml_int32_float_of_bits_unboxed(Int32_val(vi)));
}

/* 64-bit integers */

#ifdef ARCH_ALIGN_INT64

CAMLexport int64_t caml_Int64_val(value v)
{
  union { int32_t i[2]; int64_t j; } buffer;
  buffer.i[0] = ((int32_t *) Data_custom_val(v))[0];
  buffer.i[1] = ((int32_t *) Data_custom_val(v))[1];
  return buffer.j;
}

#endif

static int int64_cmp(value v1, value v2)
{
  int64_t i1 = Int64_val(v1);
  int64_t i2 = Int64_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intnat int64_hash(value v)
{
  int64_t x = Int64_val(v);
  uint32_t lo = (uint32_t) x, hi = (uint32_t) (x >> 32);
  return hi ^ lo;
}

static void int64_serialize(value v, uintnat * bsize_32,
                            uintnat * bsize_64)
{
  caml_serialize_int_8(Int64_val(v));
  *bsize_32 = *bsize_64 = 8;
}

static uintnat int64_deserialize(void * dst)
{
#ifndef ARCH_ALIGN_INT64
  *((int64_t *) dst) = caml_deserialize_sint_8();
#else
  union { int32_t i[2]; int64_t j; } buffer;
  buffer.j = caml_deserialize_sint_8();
  ((int32_t *) dst)[0] = buffer.i[0];
  ((int32_t *) dst)[1] = buffer.i[1];
#endif
  return 8;
}

CAMLexport struct custom_operations caml_int64_ops = {
  "_j",
  custom_finalize_default,
  int64_cmp,
  int64_hash,
  int64_serialize,
  int64_deserialize,
  custom_compare_ext_default
};

CAMLexport value caml_copy_int64(int64_t i)
{
  value res = caml_alloc_custom(&caml_int64_ops, 8, 0, 1);
#ifndef ARCH_ALIGN_INT64
  Int64_val(res) = i;
#else
  union { int32_t i[2]; int64_t j; } buffer;
  buffer.j = i;
  ((int32_t *) Data_custom_val(res))[0] = buffer.i[0];
  ((int32_t *) Data_custom_val(res))[1] = buffer.i[1];
#endif
  return res;
}

CAMLprim value caml_int64_neg(value v)
{ return caml_copy_int64(- Int64_val(v)); }

CAMLprim value caml_int64_add(value v1, value v2)
{ return caml_copy_int64(Int64_val(v1) + Int64_val(v2)); }

CAMLprim value caml_int64_sub(value v1, value v2)
{ return caml_copy_int64(Int64_val(v1) - Int64_val(v2)); }

CAMLprim value caml_int64_mul(value v1, value v2)
{ return caml_copy_int64(Int64_val(v1) * Int64_val(v2)); }

#define Int64_min_int ((intnat) 1 << (sizeof(intnat) * 8 - 1))

CAMLprim value caml_int64_div(value v1, value v2)
{
  int64_t dividend = Int64_val(v1);
  int64_t divisor = Int64_val(v2);
  if (divisor == 0) caml_raise_zero_divide();
  /* PR#4740: on some processors, division crashes on overflow.
     Implement the same behavior as for type "int". */
  if (dividend == ((int64_t)1 << 63) && divisor == -1) return v1;
  return caml_copy_int64(Int64_val(v1) / divisor);
}

CAMLprim value caml_int64_mod(value v1, value v2)
{
  int64_t dividend = Int64_val(v1);
  int64_t divisor = Int64_val(v2);
  if (divisor == 0) caml_raise_zero_divide();
  /* PR#4740: on some processors, division crashes on overflow.
     Implement the same behavior as for type "int". */
  if (dividend == ((int64_t)1 << 63) && divisor == -1){
    return caml_copy_int64(0);
  }
  return caml_copy_int64(Int64_val(v1) % divisor);
}

CAMLprim value caml_int64_and(value v1, value v2)
{ return caml_copy_int64(Int64_val(v1) & Int64_val(v2)); }

CAMLprim value caml_int64_or(value v1, value v2)
{ return caml_copy_int64(Int64_val(v1) | Int64_val(v2)); }

CAMLprim value caml_int64_xor(value v1, value v2)
{ return caml_copy_int64(Int64_val(v1) ^ Int64_val(v2)); }

CAMLprim value caml_int64_shift_left(value v1, value v2)
{ return caml_copy_int64(Int64_val(v1) << Int_val(v2)); }

CAMLprim value caml_int64_shift_right(value v1, value v2)
{ return caml_copy_int64(Int64_val(v1) >> Int_val(v2)); }

CAMLprim value caml_int64_shift_right_unsigned(value v1, value v2)
{ return caml_copy_int64((uint64_t) (Int64_val(v1)) >>  Int_val(v2)); }

#ifdef ARCH_SIXTYFOUR
static value caml_swap64(value x)
{
  return (((((x) & 0x00000000000000FF) << 56) |
           (((x) & 0x000000000000FF00) << 40) |
           (((x) & 0x0000000000FF0000) << 24) |
           (((x) & 0x00000000FF000000) << 8) |
           (((x) & 0x000000FF00000000) >> 8) |
           (((x) & 0x0000FF0000000000) >> 24) |
           (((x) & 0x00FF000000000000) >> 40) |
           (((x) & 0xFF00000000000000) >> 56)));
}

value caml_int64_direct_bswap(value v)
{ return caml_swap64(v); }
#endif

/* Microsoft introduced the LL integer literal suffix in Visual C++ .NET 2003 */
#if defined(_MSC_VER) && _MSC_VER < 1400
#define INT64_LITERAL(s) s ## i64
#else
#define INT64_LITERAL(s) s ## LL
#endif

CAMLprim value caml_int64_bswap(value v)
{
  int64_t x = Int64_val(v);
  return caml_copy_int64
    (((x & INT64_LITERAL(0x00000000000000FFU)) << 56) |
     ((x & INT64_LITERAL(0x000000000000FF00U)) << 40) |
     ((x & INT64_LITERAL(0x0000000000FF0000U)) << 24) |
     ((x & INT64_LITERAL(0x00000000FF000000U)) << 8) |
     ((x & INT64_LITERAL(0x000000FF00000000U)) >> 8) |
     ((x & INT64_LITERAL(0x0000FF0000000000U)) >> 24) |
     ((x & INT64_LITERAL(0x00FF000000000000U)) >> 40) |
     ((x & INT64_LITERAL(0xFF00000000000000U)) >> 56));
}

CAMLprim value caml_int64_of_int(value v)
{ return caml_copy_int64((int64_t) (Long_val(v))); }

CAMLprim value caml_int64_to_int(value v)
{ return Val_long((intnat) (Int64_val(v))); }

int64_t caml_int64_of_float_unboxed(double x)
{ return x; }

CAMLprim value caml_int64_of_float(value v)
{ return caml_copy_int64((int64_t) (Double_val(v))); }

double caml_int64_to_float_unboxed(int64_t x)
{ return x; }

CAMLprim value caml_int64_to_float(value v)
{ return caml_copy_double((double) (Int64_val(v))); }

CAMLprim value caml_int64_of_int32(value v)
{ return caml_copy_int64((int64_t) (Int32_val(v))); }

CAMLprim value caml_int64_to_int32(value v)
{ return caml_copy_int32((int32_t) (Int64_val(v))); }

CAMLprim value caml_int64_of_nativeint(value v)
{ return caml_copy_int64((int64_t) (Nativeint_val(v))); }

CAMLprim value caml_int64_to_nativeint(value v)
{ return caml_copy_nativeint((intnat) (Int64_val(v))); }

intnat caml_int64_compare_unboxed(int64_t i1, int64_t i2)
{
  return (i1 > i2) - (i1 < i2);
}

CAMLprim value caml_int64_compare(value v1, value v2)
{
  return Val_int(caml_int64_compare_unboxed(Int64_val(v1),Int64_val(v2)));
}

CAMLprim value caml_int64_format(value fmt, value arg)
{
  char format_string[FORMAT_BUFFER_SIZE];

  parse_format(fmt, ARCH_INT64_PRINTF_FORMAT, format_string);
  return caml_alloc_sprintf(format_string, Int64_val(arg));
}

CAMLprim value caml_int64_of_string(value s)
{
  char * p;
  uint64_t res, threshold;
  int sign, base, signedness, d;

  p = parse_sign_and_base(String_val(s), &base, &signedness, &sign);
  threshold = ((uint64_t) -1) / base;
  d = parse_digit(*p);
  if (d < 0 || d >= base) caml_failwith(INT64_ERRMSG);
  res = d;
  for (p++; /*nothing*/; p++) {
    char c = *p;
    if (c == '_') continue;
    d = parse_digit(c);
    if (d < 0 || d >= base) break;
    /* Detect overflow in multiplication base * res */
    if (res > threshold) caml_failwith(INT64_ERRMSG);
    res = base * res + d;
    /* Detect overflow in addition (base * res) + d */
    if (res < (uint64_t) d) caml_failwith(INT64_ERRMSG);
  }
  if (p != String_val(s) + caml_string_length(s)){
    caml_failwith(INT64_ERRMSG);
  }
  if (signedness) {
    /* Signed representation expected, allow -2^63 to 2^63 - 1 only */
    if (sign >= 0) {
      if (res >= (uint64_t)1 << 63) caml_failwith(INT64_ERRMSG);
    } else {
      if (res >  (uint64_t)1 << 63) caml_failwith(INT64_ERRMSG);
    }
  }
  if (sign < 0) res = - res;
  return caml_copy_int64(res);
}

int64_t caml_int64_bits_of_float_unboxed(double d)
{
  union { double d; int64_t i; int32_t h[2]; } u;
  u.d = d;
#if defined(__arm__) && !defined(__ARM_EABI__)
  { int32_t t = u.h[0]; u.h[0] = u.h[1]; u.h[1] = t; }
#endif
  return u.i;
}

double caml_int64_float_of_bits_unboxed(int64_t i)
{
  union { double d; int64_t i; int32_t h[2]; } u;
  u.i = i;
#if defined(__arm__) && !defined(__ARM_EABI__)
  { int32_t t = u.h[0]; u.h[0] = u.h[1]; u.h[1] = t; }
#endif
  return u.d;
}

CAMLprim value caml_int64_bits_of_float(value vd)
{
  return caml_copy_int64(caml_int64_bits_of_float_unboxed(Double_val(vd)));
}

CAMLprim value caml_int64_float_of_bits(value vi)
{
  return caml_copy_double(caml_int64_float_of_bits_unboxed(Int64_val(vi)));
}

/* Native integers */

static int nativeint_cmp(value v1, value v2)
{
  intnat i1 = Nativeint_val(v1);
  intnat i2 = Nativeint_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intnat nativeint_hash(value v)
{
  intnat n = Nativeint_val(v);
#ifdef ARCH_SIXTYFOUR
  /* 32/64 bits compatibility trick.  See explanations in file "hash.c",
     function caml_hash_mix_intnat. */
  return (n >> 32) ^ (n >> 63) ^ n;
#else
  return n;
#endif
}

static void nativeint_serialize(value v, uintnat * bsize_32,
                                uintnat * bsize_64)
{
  intnat l = Nativeint_val(v);
#ifdef ARCH_SIXTYFOUR
  if (l >= -((intnat)1 << 31) && l < ((intnat)1 << 31)) {
    caml_serialize_int_1(1);
    caml_serialize_int_4((int32_t) l);
  } else {
    caml_serialize_int_1(2);
    caml_serialize_int_8(l);
  }
#else
  caml_serialize_int_1(1);
  caml_serialize_int_4(l);
#endif
  *bsize_32 = 4;
  *bsize_64 = 8;
}

static uintnat nativeint_deserialize(void * dst)
{
  switch (caml_deserialize_uint_1()) {
  case 1:
    *((intnat *) dst) = caml_deserialize_sint_4();
    break;
  case 2:
#ifdef ARCH_SIXTYFOUR
    *((intnat *) dst) = caml_deserialize_sint_8();
#else
    caml_deserialize_error("input_value: native integer value too large");
#endif
    break;
  default:
    caml_deserialize_error("input_value: ill-formed native integer");
  }
  return sizeof(long);
}

CAMLexport struct custom_operations caml_nativeint_ops = {
  "_n",
  custom_finalize_default,
  nativeint_cmp,
  nativeint_hash,
  nativeint_serialize,
  nativeint_deserialize,
  custom_compare_ext_default
};

CAMLexport value caml_copy_nativeint(intnat i)
{
  value res = caml_alloc_custom(&caml_nativeint_ops, sizeof(intnat), 0, 1);
  Nativeint_val(res) = i;
  return res;
}

CAMLprim value caml_nativeint_neg(value v)
{ return caml_copy_nativeint(- Nativeint_val(v)); }

CAMLprim value caml_nativeint_add(value v1, value v2)
{ return caml_copy_nativeint(Nativeint_val(v1) + Nativeint_val(v2)); }

CAMLprim value caml_nativeint_sub(value v1, value v2)
{ return caml_copy_nativeint(Nativeint_val(v1) - Nativeint_val(v2)); }

CAMLprim value caml_nativeint_mul(value v1, value v2)
{ return caml_copy_nativeint(Nativeint_val(v1) * Nativeint_val(v2)); }

#define Nativeint_min_int ((intnat) 1 << (sizeof(intnat) * 8 - 1))

CAMLprim value caml_nativeint_div(value v1, value v2)
{
  intnat dividend = Nativeint_val(v1);
  intnat divisor = Nativeint_val(v2);
  if (divisor == 0) caml_raise_zero_divide();
  /* PR#4740: on some processors, modulus crashes if division overflows.
     Implement the same behavior as for type "int". */
  if (dividend == Nativeint_min_int && divisor == -1) return v1;
  return caml_copy_nativeint(dividend / divisor);
}

CAMLprim value caml_nativeint_mod(value v1, value v2)
{
  intnat dividend = Nativeint_val(v1);
  intnat divisor = Nativeint_val(v2);
  if (divisor == 0) caml_raise_zero_divide();
  /* PR#4740: on some processors, modulus crashes if division overflows.
     Implement the same behavior as for type "int". */
  if (dividend == Nativeint_min_int && divisor == -1){
    return caml_copy_nativeint(0);
  }
  return caml_copy_nativeint(dividend % divisor);
}

CAMLprim value caml_nativeint_and(value v1, value v2)
{ return caml_copy_nativeint(Nativeint_val(v1) & Nativeint_val(v2)); }

CAMLprim value caml_nativeint_or(value v1, value v2)
{ return caml_copy_nativeint(Nativeint_val(v1) | Nativeint_val(v2)); }

CAMLprim value caml_nativeint_xor(value v1, value v2)
{ return caml_copy_nativeint(Nativeint_val(v1) ^ Nativeint_val(v2)); }

CAMLprim value caml_nativeint_shift_left(value v1, value v2)
{ return caml_copy_nativeint(Nativeint_val(v1) << Int_val(v2)); }

CAMLprim value caml_nativeint_shift_right(value v1, value v2)
{ return caml_copy_nativeint(Nativeint_val(v1) >> Int_val(v2)); }

CAMLprim value caml_nativeint_shift_right_unsigned(value v1, value v2)
{ return caml_copy_nativeint((uintnat)Nativeint_val(v1) >> Int_val(v2)); }

value caml_nativeint_direct_bswap(value v)
{
#ifdef ARCH_SIXTYFOUR
  return caml_swap64(v);
#else
  return caml_swap32(v);
#endif
}

CAMLprim value caml_nativeint_bswap(value v)
{
#ifdef ARCH_SIXTYFOUR
  return caml_copy_nativeint(caml_swap64(Nativeint_val(v)));
#else
  return caml_copy_nativeint(caml_swap32(Nativeint_val(v)));
#endif
}

CAMLprim value caml_nativeint_of_int(value v)
{ return caml_copy_nativeint(Long_val(v)); }

CAMLprim value caml_nativeint_to_int(value v)
{ return Val_long(Nativeint_val(v)); }

intnat caml_nativeint_of_float_unboxed(double x)
{ return x; }

CAMLprim value caml_nativeint_of_float(value v)
{ return caml_copy_nativeint((intnat)(Double_val(v))); }

double caml_nativeint_to_float_unboxed(intnat x)
{ return x; }

CAMLprim value caml_nativeint_to_float(value v)
{ return caml_copy_double((double)(Nativeint_val(v))); }

CAMLprim value caml_nativeint_of_int32(value v)
{ return caml_copy_nativeint(Int32_val(v)); }

CAMLprim value caml_nativeint_to_int32(value v)
{ return caml_copy_int32(Nativeint_val(v)); }

intnat caml_nativeint_compare_unboxed(intnat i1, intnat i2)
{
  return (i1 > i2) - (i1 < i2);
}

CAMLprim value caml_nativeint_compare(value v1, value v2)
{
  return Val_int(caml_nativeint_compare_unboxed(Nativeint_val(v1),
                                                Nativeint_val(v2)));
}

CAMLprim value caml_nativeint_format(value fmt, value arg)
{
  char format_string[FORMAT_BUFFER_SIZE];

  parse_format(fmt, ARCH_INTNAT_PRINTF_FORMAT, format_string);
  return caml_alloc_sprintf(format_string, Nativeint_val(arg));
}

CAMLprim value caml_nativeint_of_string(value s)
{
  return caml_copy_nativeint(parse_intnat(s, 8 * sizeof(value), INTNAT_ERRMSG));
}
