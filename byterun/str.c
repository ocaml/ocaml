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

