/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Operations on strings */

#include <string.h>
#include <ctype.h>
#include "alloc.h"
#include "fail.h"
#include "mlvalues.h"
#include "misc.h"
#ifdef HAS_LOCALE
#include <locale.h>
#endif

CAMLexport mlsize_t caml_string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}

CAMLprim value caml_ml_string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return Val_long(temp - Byte (s, temp));
}

CAMLprim value caml_create_string(value len)
{
  mlsize_t size = Long_val(len);
  if (size > Bsize_wsize (Max_wosize) - 1){
    caml_invalid_argument("String.create");
  }
  return caml_alloc_string(size);
}

CAMLprim value caml_string_get(value str, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str)) caml_array_bound_error();
  return Val_int(Byte_u(str, idx));
}

CAMLprim value caml_string_set(value str, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str)) caml_array_bound_error();
  Byte_u(str, idx) = Int_val(newval);
  return Val_unit;
}

CAMLprim value caml_string_get16(value str, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str) - 1) caml_array_bound_error();
  intnat res;
  unsigned char b1 = Byte_u(str, idx);
  unsigned char b2 = Byte_u(str, idx + 1);
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 8 | b2;
#else
  res = b2 << 8 | b1;
#endif
  return Val_int(res);
}

CAMLprim value caml_string_get32(value str, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str) - 3) caml_array_bound_error();
  intnat res;
  unsigned char b1 = Byte_u(str, idx);
  unsigned char b2 = Byte_u(str, idx + 1);
  unsigned char b3 = Byte_u(str, idx + 2);
  unsigned char b4 = Byte_u(str, idx + 3);
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 24 | b2 << 16 | b3 << 8 | b4;
#else
  res = b4 << 24 | b3 << 16 | b2 << 8 | b1;
#endif
  return caml_copy_int32(res);
}

#ifdef ARCH_INT64_TYPE
#include "int64_native.h"
#else
#include "int64_emul.h"
#endif

CAMLprim value caml_string_get64(value str, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str) - 7) caml_array_bound_error();
  uint32 reshi;
  uint32 reslo;
  unsigned char b1 = Byte_u(str, idx);
  unsigned char b2 = Byte_u(str, idx + 1);
  unsigned char b3 = Byte_u(str, idx + 2);
  unsigned char b4 = Byte_u(str, idx + 3);
  unsigned char b5 = Byte_u(str, idx + 4);
  unsigned char b6 = Byte_u(str, idx + 5);
  unsigned char b7 = Byte_u(str, idx + 6);
  unsigned char b8 = Byte_u(str, idx + 7);
#ifdef ARCH_BIG_ENDIAN
  reshi = b1 << 24 | b2 << 16 | b3 << 8 | b4;
  reslo = b5 << 24 | b6 << 16 | b7 << 8 | b8;
#else
  reshi = b8 << 24 | b7 << 16 | b6 << 8 | b5;
  reslo = b4 << 24 | b3 << 16 | b2 << 8 | b1;
#endif
  return caml_copy_int64(I64_literal(reshi,reslo));
}

CAMLprim value caml_string_set16(value str, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str) - 1) caml_array_bound_error();
  unsigned char b1, b2;
  intnat val = Long_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 8;
  b2 = 0xFF & val;
#else
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  Byte_u(str, idx) = b1;
  Byte_u(str, idx + 1) = b2;
  return Val_unit;
}

CAMLprim value caml_string_set32(value str, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str) - 3) caml_array_bound_error();
  unsigned char b1, b2, b3, b4;
  intnat val = Int32_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 24;
  b2 = 0xFF & val >> 16;
  b3 = 0xFF & val >> 8;
  b4 = 0xFF & val;
#else
  b4 = 0xFF & val >> 24;
  b3 = 0xFF & val >> 16;
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  Byte_u(str, idx) = b1;
  Byte_u(str, idx + 1) = b2;
  Byte_u(str, idx + 2) = b3;
  Byte_u(str, idx + 3) = b4;
  return Val_unit;
}

CAMLprim value caml_string_set64(value str, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str) - 7) caml_array_bound_error();
  unsigned char b1, b2, b3, b4, b5, b6, b7, b8;
  int64 val = Int64_val(newval);
  uint32 lo,hi;
  I64_split(val,hi,lo);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & hi >> 24;
  b2 = 0xFF & hi >> 16;
  b3 = 0xFF & hi >> 8;
  b4 = 0xFF & hi;
  b5 = 0xFF & lo >> 24;
  b6 = 0xFF & lo >> 16;
  b7 = 0xFF & lo >> 8;
  b8 = 0xFF & lo;
#else
  b8 = 0xFF & hi >> 24;
  b7 = 0xFF & hi >> 16;
  b6 = 0xFF & hi >> 8;
  b5 = 0xFF & hi;
  b4 = 0xFF & lo >> 24;
  b3 = 0xFF & lo >> 16;
  b2 = 0xFF & lo >> 8;
  b1 = 0xFF & lo;
#endif
  Byte_u(str, idx) = b1;
  Byte_u(str, idx + 1) = b2;
  Byte_u(str, idx + 2) = b3;
  Byte_u(str, idx + 3) = b4;
  Byte_u(str, idx + 4) = b5;
  Byte_u(str, idx + 5) = b6;
  Byte_u(str, idx + 6) = b7;
  Byte_u(str, idx + 7) = b8;
  return Val_unit;
}

CAMLprim value caml_string_equal(value s1, value s2)
{
  mlsize_t sz1, sz2;
  value * p1, * p2;

  if (s1 == s2) return Val_true;
  sz1 = Wosize_val(s1);
  sz2 = Wosize_val(s2);
  if (sz1 != sz2) return Val_false;
  for(p1 = Op_val(s1), p2 = Op_val(s2); sz1 > 0; sz1--, p1++, p2++)
    if (*p1 != *p2) return Val_false;
  return Val_true;
}

CAMLprim value caml_string_notequal(value s1, value s2)
{
  return Val_not(caml_string_equal(s1, s2));
}

CAMLprim value caml_string_compare(value s1, value s2)
{
  mlsize_t len1, len2;
  int res;

  if (s1 == s2) return Val_int(0);
  len1 = caml_string_length(s1);
  len2 = caml_string_length(s2);
  res = memcmp(String_val(s1), String_val(s2), len1 <= len2 ? len1 : len2);
  if (res < 0) return Val_int(-1);
  if (res > 0) return Val_int(1);
  if (len1 < len2) return Val_int(-1);
  if (len1 > len2) return Val_int(1);
  return Val_int(0);
}

CAMLprim value caml_string_lessthan(value s1, value s2)
{
  return caml_string_compare(s1, s2) < Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_string_lessequal(value s1, value s2)
{
  return caml_string_compare(s1, s2) <= Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_string_greaterthan(value s1, value s2)
{
  return caml_string_compare(s1, s2) > Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_string_greaterequal(value s1, value s2)
{
  return caml_string_compare(s1, s2) >= Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_blit_string(value s1, value ofs1, value s2, value ofs2,
                                value n)
{
  memmove(&Byte(s2, Long_val(ofs2)), &Byte(s1, Long_val(ofs1)), Int_val(n));
  return Val_unit;
}

CAMLprim value caml_fill_string(value s, value offset, value len, value init)
{
  memset(&Byte(s, Long_val(offset)), Int_val(init), Long_val(len));
  return Val_unit;
}

CAMLprim value caml_is_printable(value chr)
{
  int c;

#ifdef HAS_LOCALE
  static int locale_is_set = 0;
  if (! locale_is_set) {
    setlocale(LC_CTYPE, "");
    locale_is_set = 1;
  }
#endif
  c = Int_val(chr);
  return Val_bool(isprint(c));
}

CAMLprim value caml_bitvect_test(value bv, value n)
{
  int pos = Int_val(n);
  return Val_int(Byte_u(bv, pos >> 3) & (1 << (pos & 7)));
}
