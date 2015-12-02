#include "u8tou16.h"

/* Copyright 2005 b8_bavard, INRIA, CML */
/*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* Stub code to interface with libiconv
 *
 * taken and modified from
 *   mldonkey/src/utils/lib/charsetstubs.c
 *
 */

/*
  20080415 ygrek
  Modified to use WideCharToMultiByte windows api
*/

#include "caml/memory.h"
#include "caml/alloc.h"

#define STRICT      /* Strict typing, please */
#include <windows.h>
#undef STRICT

#define FALSE 0
#define TRUE  1

#define UTF8_COMPUTE(Char, Mask, Len)               \
  if (Char < 128)                   \
    {                       \
      Len = 1;                      \
      Mask = 0x7f;                    \
    }                       \
  else if ((Char & 0xe0) == 0xc0)               \
    {                       \
      Len = 2;                      \
      Mask = 0x1f;                    \
    }                       \
  else if ((Char & 0xf0) == 0xe0)               \
    {                       \
      Len = 3;                      \
      Mask = 0x0f;                    \
    }                       \
  else if ((Char & 0xf8) == 0xf0)               \
    {                       \
      Len = 4;                      \
      Mask = 0x07;                    \
    }                       \
  else if ((Char & 0xfc) == 0xf8)               \
    {                       \
      Len = 5;                      \
      Mask = 0x03;                    \
    }                       \
  else if ((Char & 0xfe) == 0xfc)               \
    {                       \
      Len = 6;                      \
      Mask = 0x01;                    \
    }                       \
  else                        \
    Len = -1;

#define UTF8_LENGTH(Char)              \
  ((Char) < 0x80 ? 1 :                 \
   ((Char) < 0x800 ? 2 :               \
    ((Char) < 0x10000 ? 3 :            \
     ((Char) < 0x200000 ? 4 :          \
      ((Char) < 0x4000000 ? 5 : 6)))))


#define UTF8_GET(Result, Chars, Count, Mask, Len)           \
  (Result) = (Chars)[0] & (Mask);               \
  for ((Count) = 1; (Count) < (Len); ++(Count))             \
    {                       \
      if (((Chars)[(Count)] & 0xc0) != 0x80)              \
  {                     \
    (Result) = -1;                  \
    break;                    \
  }                     \
      (Result) <<= 6;                   \
      (Result) |= ((Chars)[(Count)] & 0x3f);              \
    }

#define UNICODE_VALID(Char)                   \
    ((Char) < 0x110000 &&                     \
     (((Char) & 0xFFFFF800) != 0xD800) &&     \
     ((Char) < 0xFDD0 || (Char) > 0xFDEF) &&  \
     ((Char) & 0xFFFE) != 0xFFFE)

int
ocaml_utf8_validate (const char  *str,
                     size_t      max_len,
                     const char  **end)
{

  const char *p;

  if (str == NULL)
    return FALSE;

  if (end)
    *end = str;

  p = str;

  while ((max_len < 0 || (p - str) < max_len) && *p)
    {
      int i, mask = 0, len;
      unsigned int result;
      unsigned char c = (unsigned char) *p;

      UTF8_COMPUTE (c, mask, len);

      if (len == -1)
        break;

      /* check that the expected number of bytes exists in str */
      if (max_len >= 0 &&
          ((max_len - (p - str)) < len))
        break;

      UTF8_GET (result, p, i, mask, len);

      if (UTF8_LENGTH (result) != len) /* Check for overlong UTF-8 */
  break;

      if (result == (unsigned int)-1)
        break;

      if (!UNICODE_VALID (result))
  break;

      p += len;
    }

  if (end)
    *end = p;

  /* See that we covered the entire length if a length was
   * passed in, or that we ended on a nul if not
   */
  if (max_len >= 0 &&
      p != (str + max_len))
    return FALSE;
  else if (max_len < 0 &&
           *p != '\0')
    return FALSE;
  else
    return TRUE;
}

WCHAR* convert_to_utf16(const char* str,
                        size_t len,
                        UINT codepage)
{
    WCHAR* outp;
    size_t outbuf_size;
    int chars;

    outbuf_size = len*2 + 8;

    outp = caml_stat_alloc(outbuf_size + 2);
    memset(outp,0,outbuf_size + 2);

    if (0 == len)
    {
        return outp;
    }

    chars = MultiByteToWideChar(codepage, 0, str, len, outp, outbuf_size);
    if (0 == chars)
    {
        free(outp);
        return NULL;
    }

    *(outp + chars) = (WCHAR)0;
    return outp;
}


char* convert_from_utf16(const WCHAR* str,
                         size_t len,
                         UINT codepage)
{
    char* outp;
    size_t outbuf_size;
    int chars;

    outbuf_size = len*2 + 8;

    outp = caml_stat_alloc(outbuf_size + 2);
    memset(outp,0,outbuf_size + 2);

    if (0 == len)
    {
        return outp;
    }

    chars = WideCharToMultiByte(codepage, 0, str, len, outp, outbuf_size, NULL, NULL);
    if (0 == chars)
    {
        free(outp);
        return NULL;
    }

    *(outp + chars) = '\0';
    return outp;
}

// -----------------------------------------------------------------------------

int is_valid_utf8(const char *s)
{
  return ocaml_utf8_validate(s,strlen(s),NULL);
}

WCHAR* ansi_to_utf16(const char * str)
{
  return convert_to_utf16(str,strlen(str),CP_ACP);
}

WCHAR* utf8_to_utf16(const char * str)
{
  return convert_to_utf16(str,strlen(str),CP_UTF8);
}

char* utf16_to_utf8(const WCHAR* str)
{
  return convert_from_utf16(str,wcslen(str)*sizeof(WCHAR),CP_UTF8);
}

WCHAR* to_utf16(const char* str)
{
  if(is_valid_utf8(str))
    return utf8_to_utf16(str);
  else
    return ansi_to_utf16(str);
}

value caml_copy_utf16(WCHAR* p)
{
  char* s = utf16_to_utf8(p);
  value v = caml_copy_string(s);
  caml_stat_free(s);
  return v;
}

// -----------------------------------------------------------------------------
