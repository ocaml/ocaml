/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Operations on strings */

#include <string.h>
#include "alloc.h"
#include "fail.h"
#include "mlvalues.h"
#include "misc.h"

mlsize_t string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}

value ml_string_length(value s)     /* ML */
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return Val_long(temp - Byte (s, temp));
}

value create_string(value len)        /* ML */
{
  mlsize_t size = Long_val(len);
  if (size > Bsize_wsize (Max_wosize) - 1) invalid_argument("String.create");
  return alloc_string(size);
}

value string_get(value str, value index)    /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= string_length(str)) invalid_argument("String.get");
  return Val_int(Byte_u(str, idx));
}

value string_set(value str, value index, value newval)    /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= string_length(str)) invalid_argument("String.set");
  Byte_u(str, idx) = Int_val(newval);
  return Val_unit;
}

value string_equal(value s1, value s2)      /* ML */
{
  mlsize_t sz1 = Wosize_val(s1);
  mlsize_t sz2 = Wosize_val(s2);
  value * p1, * p2;
  if (sz1 != sz2) return Val_false;
  for(p1 = Op_val(s1), p2 = Op_val(s2); sz1 > 0; sz1--, p1++, p2++)
    if (*p1 != *p2) return Val_false;
  return Val_true;
}

value string_notequal(value s1, value s2)   /* ML */
{
  return Val_not(string_equal(s1, s2));
}
  
value blit_string(value s1, value ofs1, value s2, value ofs2, value n)   /* ML */
{
  bcopy(&Byte(s1, Long_val(ofs1)), &Byte(s2, Long_val(ofs2)), Int_val(n));
  return Val_unit;
}

value fill_string(value s, value offset, value len, value init) /* ML */
{
  register char * p;
  register mlsize_t n;
  register char c;

  c = Long_val(init);
  for(p = &Byte(s, Long_val(offset)), n = Long_val(len);
      n > 0; n--, p++)
    *p = c;
  return Val_unit;
}

static unsigned char printable_chars_ascii[] = { /* 0x20-0x7E */
  0, 0, 0, 0, 0xFF, 0xFF, 0xFF, 0xFF,
  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0
};
static unsigned char printable_chars_iso[] = { /* 0x20-0x7E 0xA1-0xFF */
  0, 0, 0, 0, 0xFF, 0xFF, 0xFF, 0xFF,
  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F,
  0, 0, 0, 0, 0xFE, 0xFF, 0xFF, 0xFF,
  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
};

value is_printable(value chr) /* ML */
{
  int c;
  unsigned char * printable_chars;

#ifdef _WIN32
  printable_chars = printable_chars_iso;
#else
  static int iso_charset = -1;
  if (iso_charset == -1) {
    char * lc_ctype = (char *) getenv("LC_CTYPE");
    iso_charset = (lc_ctype != 0 && strcmp(lc_ctype, "iso_8859_1") == 0);
  }
  printable_chars = iso_charset ? printable_chars_iso : printable_chars_ascii;
#endif
  c = Int_val(chr);
  return Val_bool(printable_chars[c >> 3] & (1 << (c & 7)));
}

value bitvect_test(value bv, value n)       /* ML */
{
  int pos = Int_val(n);
  return Val_int(Byte_u(bv, pos >> 3) & (1 << (pos & 7)));
}

