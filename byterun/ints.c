/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdio.h>
#include <string.h>
#include "alloc.h"
#include "custom.h"
#include "fail.h"
#include "intext.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"

static char * parse_sign_and_base(char * p,
                                  /*out*/ int * base,
                                  /*out*/ int * sign)
{
  *sign = 1;
  if (*p == '-') {
    *sign = -1;
    p++;
  }
  *base = 10;
  if (*p == '0') {
    switch (p[1]) {
    case 'x': case 'X':
      *base = 16; p += 2; break;
    case 'o': case 'O':
      *base = 8; p += 2; break;
    case 'b': case 'B':
      *base = 2; p += 2; break;
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

static long parse_long(char * p)
{
  unsigned long res;
  int sign, base, d;

  p = parse_sign_and_base(p, &base, &sign);
  d = parse_digit(*p);
  if (d < 0 || d >= base) failwith("int_of_string");
  for (p++, res = d; /*nothing*/; p++) {
    char c = *p;
    if (c == '_') continue;
    d = parse_digit(c);
    if (d < 0 || d >= base) break;
    res = base * res + d;
  }
  if (*p != 0) failwith("int_of_string");
  return sign < 0 ? -((long) res) : (long) res;
}

CAMLprim value int_of_string(value s)
{
  return Val_long(parse_long(String_val(s)));
}

#define FORMAT_BUFFER_SIZE 32

static char * parse_format(value fmt,
                           char * suffix,
                           char format_string[],
                           char default_format_buffer[])
{
  int prec, lastletter;
  char * p;
  mlsize_t len, len_suffix;

  /* Copy Caml format fmt to format_string,
     adding the suffix before the last letter of the format */
  len = string_length(fmt);
  len_suffix = strlen(suffix);
  if (len + len_suffix + 1 >= FORMAT_BUFFER_SIZE)
    invalid_argument("format_int: format too long");
  memmove(format_string, String_val(fmt), len);
  p = format_string + len - 1;
  lastletter = *p;
  /* Compress two-letter formats, ignoring the [lnL] annotation */
  if (p[-1] == 'l' || p[-1] == 'n' || p[-1] == 'L') p--;
  memmove(p, suffix, len_suffix);  p += len_suffix;
  *p++ = lastletter;
  *p = 0;
  /* Determine space needed for result and allocate it dynamically if needed */
  prec = 22 + 5; /* 22 digits for 64-bit number in octal + 5 extra */
  for (p = String_val(fmt); *p != 0; p++) {
    if (*p >= '0' && *p <= '9') {
      prec = atoi(p) + 5;
      break;
    }
  }
  if (prec < FORMAT_BUFFER_SIZE)
    return default_format_buffer;
  else
    return stat_alloc(prec + 1);
}

CAMLprim value format_int(value fmt, value arg)
{
  char format_string[FORMAT_BUFFER_SIZE];
  char default_format_buffer[FORMAT_BUFFER_SIZE];
  char * buffer;
  value res;

  buffer = parse_format(fmt, "l", format_string, default_format_buffer);
  sprintf(buffer, format_string, Long_val(arg));
  res = copy_string(buffer);
  if (buffer != default_format_buffer) stat_free(buffer);
  return res;
}

/* 32-bit integers */

static int int32_compare(value v1, value v2)
{
  int32 i1 = Int32_val(v1);
  int32 i2 = Int32_val(v2);
  return i1 == i2 ? 0 : i1 < i2 ? -1 : 1;
}

static long int32_hash(value v)
{
  return Int32_val(v);
}

static void int32_serialize(value v, unsigned long * wsize_32,
                            unsigned long * wsize_64)
{
  serialize_int_4(Int32_val(v));
  *wsize_32 = *wsize_64 = 4;
}

static unsigned long int32_deserialize(void * dst)
{
  *((int32 *) dst) = deserialize_sint_4();
  return 4;
}

CAMLexport struct custom_operations int32_ops = {
  "_i",
  custom_finalize_default,
  int32_compare,
  int32_hash,
  int32_serialize,
  int32_deserialize
};

CAMLexport value copy_int32(int32 i)
{
  value res = alloc_custom(&int32_ops, 4, 0, 1);
  Int32_val(res) = i;
  return res;
}

CAMLprim value int32_neg(value v)
{ return copy_int32(- Int32_val(v)); }

CAMLprim value int32_add(value v1, value v2)
{ return copy_int32(Int32_val(v1) + Int32_val(v2)); }

CAMLprim value int32_sub(value v1, value v2)
{ return copy_int32(Int32_val(v1) - Int32_val(v2)); }

CAMLprim value int32_mul(value v1, value v2)
{ return copy_int32(Int32_val(v1) * Int32_val(v2)); }

CAMLprim value int32_div(value v1, value v2)
{
  int32 divisor = Int32_val(v2);
  if (divisor == 0) raise_zero_divide();
  return copy_int32(Int32_val(v1) / divisor);
}

CAMLprim value int32_mod(value v1, value v2)
{
  int32 divisor = Int32_val(v2);
  if (divisor == 0) raise_zero_divide();
  return copy_int32(Int32_val(v1) % divisor);
}

CAMLprim value int32_and(value v1, value v2)
{ return copy_int32(Int32_val(v1) & Int32_val(v2)); }

CAMLprim value int32_or(value v1, value v2)
{ return copy_int32(Int32_val(v1) | Int32_val(v2)); }

CAMLprim value int32_xor(value v1, value v2)
{ return copy_int32(Int32_val(v1) ^ Int32_val(v2)); }

CAMLprim value int32_shift_left(value v1, value v2)
{ return copy_int32(Int32_val(v1) << Int_val(v2)); }

CAMLprim value int32_shift_right(value v1, value v2)
{ return copy_int32(Int32_val(v1) >> Int_val(v2)); }

CAMLprim value int32_shift_right_unsigned(value v1, value v2)
{ return copy_int32((uint32)Int32_val(v1) >> Int_val(v2)); }

CAMLprim value int32_of_int(value v)
{ return copy_int32(Long_val(v)); }

CAMLprim value int32_to_int(value v)
{ return Val_long(Int32_val(v)); }

CAMLprim value int32_of_float(value v)
{ return copy_int32((int32)(Double_val(v))); }

CAMLprim value int32_to_float(value v)
{ return copy_double((double)(Int32_val(v))); }

CAMLprim value int32_format(value fmt, value arg)
{
  char format_string[FORMAT_BUFFER_SIZE];
  char default_format_buffer[FORMAT_BUFFER_SIZE];
  char * buffer;
  value res;

  buffer = parse_format(fmt, "", format_string, default_format_buffer);
  sprintf(buffer, format_string, (long) Int32_val(arg));
  res = copy_string(buffer);
  if (buffer != default_format_buffer) stat_free(buffer);
  return res;
}

CAMLprim value int32_of_string(value s)
{
  return copy_int32(parse_long(String_val(s)));
}

/* 64-bit integers */

#ifdef ARCH_INT64_TYPE

#ifdef ARCH_ALIGN_INT64

CAMLexport int64 Int64_val(value v)
{
  union { int32 i[2]; int64 j; } buffer;
  buffer.i[0] = ((int32 *) Data_custom_val(v))[0];
  buffer.i[1] = ((int32 *) Data_custom_val(v))[1];
  return buffer.j;
}

CAMLexport void Store_int64(value v, int64 i)
{
}

#endif

static int int64_compare(value v1, value v2)
{
  int64 i1 = Int64_val(v1);
  int64 i2 = Int64_val(v2);
  return i1 == i2 ? 0 : i1 < i2 ? -1 : 1;
}

static long int64_hash(value v)
{
  return (long) Int64_val(v);
}

static void int64_serialize(value v, unsigned long * wsize_32,
                            unsigned long * wsize_64)
{
  serialize_int_8(Int64_val(v));
  *wsize_32 = *wsize_64 = 8;
}

static unsigned long int64_deserialize(void * dst)
{
#ifndef ARCH_ALIGN_INT64
  *((int64 *) dst) = deserialize_sint_8();
#else
  union { int32 i[2]; int64 j; } buffer;
  buffer.j = deserialize_sint_8();
  ((int32 *) dst)[0] = buffer.i[0];
  ((int32 *) dst)[1] = buffer.i[1];
#endif
  return 8;
}

CAMLexport struct custom_operations int64_ops = {
  "_j",
  custom_finalize_default,
  int64_compare,
  int64_hash,
  int64_serialize,
  int64_deserialize
};

CAMLexport value copy_int64(int64 i)
{
  value res = alloc_custom(&int64_ops, 8, 0, 1);
#ifndef ARCH_ALIGN_INT64
  Int64_val(res) = i;
#else
  union { int32 i[2]; int64 j; } buffer;
  buffer.j = i;
  ((int32 *) Data_custom_val(res))[0] = buffer.i[0];
  ((int32 *) Data_custom_val(res))[1] = buffer.i[1];
#endif
  return res;
}

CAMLprim value int64_neg(value v)
{ return copy_int64(- Int64_val(v)); }

CAMLprim value int64_add(value v1, value v2)
{ return copy_int64(Int64_val(v1) + Int64_val(v2)); }

CAMLprim value int64_sub(value v1, value v2)
{ return copy_int64(Int64_val(v1) - Int64_val(v2)); }

CAMLprim value int64_mul(value v1, value v2)
{ return copy_int64(Int64_val(v1) * Int64_val(v2)); }

CAMLprim value int64_div(value v1, value v2)
{
  int64 divisor = Int64_val(v2);
  if (divisor == 0) raise_zero_divide();
  return copy_int64(Int64_val(v1) / divisor);
}

CAMLprim value int64_mod(value v1, value v2)
{
  int64 divisor = Int64_val(v2);
  if (divisor == 0) raise_zero_divide();
  return copy_int64(Int64_val(v1) % divisor);
}

CAMLprim value int64_and(value v1, value v2)
{ return copy_int64(Int64_val(v1) & Int64_val(v2)); }

CAMLprim value int64_or(value v1, value v2)
{ return copy_int64(Int64_val(v1) | Int64_val(v2)); }

CAMLprim value int64_xor(value v1, value v2)
{ return copy_int64(Int64_val(v1) ^ Int64_val(v2)); }

CAMLprim value int64_shift_left(value v1, value v2)
{ return copy_int64(Int64_val(v1) << Int_val(v2)); }

CAMLprim value int64_shift_right(value v1, value v2)
{ return copy_int64(Int64_val(v1) >> Int_val(v2)); }

CAMLprim value int64_shift_right_unsigned(value v1, value v2)
{ return copy_int64((uint64)Int64_val(v1) >> Int_val(v2)); }

CAMLprim value int64_of_int(value v)
{ return copy_int64(Long_val(v)); }

CAMLprim value int64_to_int(value v)
{ return Val_long((long) Int64_val(v)); }

CAMLprim value int64_of_float(value v)
{ return copy_int64((int64)(Double_val(v))); }

CAMLprim value int64_to_float(value v)
{ return copy_double((double)(Int64_val(v))); }

CAMLprim value int64_of_int32(value v)
{ return copy_int64(Int32_val(v)); }

CAMLprim value int64_to_int32(value v)
{ return copy_int32((int32) Int64_val(v)); }

CAMLprim value int64_of_nativeint(value v)
{ return copy_int64(Nativeint_val(v)); }

CAMLprim value int64_to_nativeint(value v)
{ return copy_nativeint((long) Int64_val(v)); }

CAMLprim value int64_format(value fmt, value arg)
#ifdef ARCH_INT64_PRINTF_FORMAT
{
  char format_string[FORMAT_BUFFER_SIZE];
  char default_format_buffer[FORMAT_BUFFER_SIZE];
  char * buffer;
  value res;

  buffer = parse_format(fmt, ARCH_INT64_PRINTF_FORMAT,
                        format_string, default_format_buffer);
  sprintf(buffer, format_string, Int64_val(arg));
  res = copy_string(buffer);
  if (buffer != default_format_buffer) stat_free(buffer);
  return res;
}
#else
{ invalid_argument ("Int64.format is not implemented on this platform"); }
#endif

CAMLprim value int64_of_string(value s)
{
  char * p;
  uint64 res;
  int sign, base, d;

  p = parse_sign_and_base(String_val(s), &base, &sign);
  d = parse_digit(*p);
  if (d < 0 || d >= base) failwith("int_of_string");
  for (p++, res = d; /*nothing*/; p++) {
    char c = *p;
    if (c == '_') continue;
    d = parse_digit(c);
    if (d < 0 || d >= base) break;
    res = base * res + d;
  }
  if (*p != 0) failwith("int_of_string");
  return copy_int64(sign < 0 ? -((int64) res) : (int64) res);
}

CAMLprim value int64_bits_of_float(value vd)
{
  union { double d; int64 i; } u;
  u.d = Double_val(vd);
  return copy_int64(u.i);
}

CAMLprim value int64_float_of_bits(value vi)
{
  union { double d; int64 i; } u;
  u.i = Int64_val(vi);
  return copy_double(u.d);
}

#else

static char int64_error[] =
  "The type Int64.t is not supported on this platform";

value copy_int64(int64 i)
{ invalid_argument(int64_error); }

value int64_neg(value v)
{ invalid_argument(int64_error); }

value int64_add(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_sub(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_mul(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_div(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_mod(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_and(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_or(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_xor(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_shift_left(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_shift_right(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_shift_right_unsigned(value v1, value v2)
{ invalid_argument(int64_error); }

value int64_of_int(value v)
{ invalid_argument(int64_error); }

value int64_to_int(value v)
{ invalid_argument(int64_error); }

value int64_of_float(value v)
{ invalid_argument(int64_error); }

value int64_to_float(value v)
{ invalid_argument(int64_error); }

value int64_of_int32(value v)
{ invalid_argument(int64_error); }

value int64_to_int32(value v)
{ invalid_argument(int64_error); }

value int64_of_nativeint(value v)
{ invalid_argument(int64_error); }

value int64_to_nativeint(value v)
{ invalid_argument(int64_error); }

value int64_format(value fmt, value arg)
{ invalid_argument(int64_error); }

value int64_of_string(value s)
{ invalid_argument(int64_error); }

value int64_bits_of_float(value vd)
{ invalid_argument(int64_error); }

value int64_float_of_bits(value vi)
{ invalid_argument(int64_error); }

#endif

/* Native integers */

static int nativeint_compare(value v1, value v2)
{
  long i1 = Nativeint_val(v1);
  long i2 = Nativeint_val(v2);
  return i1 == i2 ? 0 : i1 < i2 ? -1 : 1;
}

static long nativeint_hash(value v)
{
  return Nativeint_val(v);
}

static void nativeint_serialize(value v, unsigned long * wsize_32,
                            unsigned long * wsize_64)
{
  long l = Nativeint_val(v);
#ifdef ARCH_SIXTYFOUR
  if (l <= 0x7FFFFFFFL && l >= -0x80000000L) {
    serialize_int_1(1);
    serialize_int_4((int32) l);
  } else {
    serialize_int_1(2);
    serialize_int_8(l);
  }
#else
  serialize_int_1(1);
  serialize_int_4(l);
#endif
  *wsize_32 = 4;
  *wsize_64 = 8;
}

static unsigned long nativeint_deserialize(void * dst)
{
  switch (deserialize_uint_1()) {
  case 1:
    *((long *) dst) = deserialize_sint_4();
    break;
  case 2:
#ifdef ARCH_SIXTYFOUR
    *((long *) dst) = deserialize_sint_8();
#else
    deserialize_error("input_value: native integer value too large");
#endif
    break;
  default:
    deserialize_error("input_value: ill-formed native integer");
  }
  return sizeof(long);
}

CAMLexport struct custom_operations nativeint_ops = {
  "_n",
  custom_finalize_default,
  nativeint_compare,
  nativeint_hash,
  nativeint_serialize,
  nativeint_deserialize
};

CAMLexport value copy_nativeint(long i)
{
  value res = alloc_custom(&nativeint_ops, sizeof(long), 0, 1);
  Nativeint_val(res) = i;
  return res;
}

CAMLprim value nativeint_neg(value v)
{ return copy_nativeint(- Nativeint_val(v)); }

CAMLprim value nativeint_add(value v1, value v2)
{ return copy_nativeint(Nativeint_val(v1) + Nativeint_val(v2)); }

CAMLprim value nativeint_sub(value v1, value v2)
{ return copy_nativeint(Nativeint_val(v1) - Nativeint_val(v2)); }

CAMLprim value nativeint_mul(value v1, value v2)
{ return copy_nativeint(Nativeint_val(v1) * Nativeint_val(v2)); }

CAMLprim value nativeint_div(value v1, value v2)
{
  long divisor = Nativeint_val(v2);
  if (divisor == 0) raise_zero_divide();
  return copy_nativeint(Nativeint_val(v1) / divisor);
}

CAMLprim value nativeint_mod(value v1, value v2)
{
  long divisor = Nativeint_val(v2);
  if (divisor == 0) raise_zero_divide();
  return copy_nativeint(Nativeint_val(v1) % divisor);
}

CAMLprim value nativeint_and(value v1, value v2)
{ return copy_nativeint(Nativeint_val(v1) & Nativeint_val(v2)); }

CAMLprim value nativeint_or(value v1, value v2)
{ return copy_nativeint(Nativeint_val(v1) | Nativeint_val(v2)); }

CAMLprim value nativeint_xor(value v1, value v2)
{ return copy_nativeint(Nativeint_val(v1) ^ Nativeint_val(v2)); }

CAMLprim value nativeint_shift_left(value v1, value v2)
{ return copy_nativeint(Nativeint_val(v1) << Int_val(v2)); }

CAMLprim value nativeint_shift_right(value v1, value v2)
{ return copy_nativeint(Nativeint_val(v1) >> Int_val(v2)); }

CAMLprim value nativeint_shift_right_unsigned(value v1, value v2)
{ return copy_nativeint((unsigned long)Nativeint_val(v1) >> Int_val(v2)); }

CAMLprim value nativeint_of_int(value v)
{ return copy_nativeint(Long_val(v)); }

CAMLprim value nativeint_to_int(value v)
{ return Val_long(Nativeint_val(v)); }

CAMLprim value nativeint_of_float(value v)
{ return copy_nativeint((long)(Double_val(v))); }

CAMLprim value nativeint_to_float(value v)
{ return copy_double((double)(Nativeint_val(v))); }

CAMLprim value nativeint_of_int32(value v)
{ return copy_nativeint(Int32_val(v)); }

CAMLprim value nativeint_to_int32(value v)
{ return copy_int32(Nativeint_val(v)); }

CAMLprim value nativeint_format(value fmt, value arg)
{
  char format_string[FORMAT_BUFFER_SIZE];
  char default_format_buffer[FORMAT_BUFFER_SIZE];
  char * buffer;
  value res;

  buffer = parse_format(fmt, "l", format_string, default_format_buffer);
  sprintf(buffer, format_string, (long) Nativeint_val(arg));
  res = copy_string(buffer);
  if (buffer != default_format_buffer) stat_free(buffer);
  return res;
}

CAMLprim value nativeint_of_string(value s)
{
  return copy_nativeint(parse_long(String_val(s)));
}


