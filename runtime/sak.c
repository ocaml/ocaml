/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 David Allsopp, OCaml Labs, Cambridge.                  */
/*                                                                        */
/*   Copyright 2021 David Allsopp Ltd.                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Runtime Builder's Swiss Army Knife. This utility performs functions
   previously delegated to classic Unix utilities but which ultimately seem to
   cause more hassle for maintenance than the initial simplicity suggests.

   This tool is a memorial to the many hours and PRs spent chasing down strange
   locale issues, stray CR characters and fighting yet another incompatible
   implementation of sed or awk. */

/* Borrow the Unicode *_os definitions and T() macro from misc.h */
#define CAML_INTERNALS
#include "caml/misc.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

/* Operations
   - encode-C-utf8-literal and encode-C-utf16-literal. Used for the
     OCAML_STDLIB_DIR macro in runtime/build_config.h to ensure the LIBDIR make
     variable is correctly represented as a C string literal.

     `sak encode-C-utf8-literal /usr/local/lib` returns `"/usr/local/lib"`, in a
     format suitable for a Unix build.

     `sak encode-C-utf16-literal "C:\OCaml🐫\lib"` returns
     `L"C:\\OCaml\xd83d\xdc2b\\lib"`, a format suitable for a Windows build.
 */

static void usage(void)
{
  printf(
    "OCaml Build System Swiss Army Knife\n"
    "Usage: sak command\n"
    "Commands:\n"
    " * encode-C-utf8-literal path - encodes path as a C UTF-8 string literal\n"
    " * encode-C-utf16-literal path - encodes path as a C UTF-16 string "
      "literal\n"
  );
}

#define printable(c) ((c) >= 0x20 && (c) < 0x7f)

/* Some Unicode macros */

/* Whether a byte is a UTF-8 continuation byte */
#define U8C(x) (((x) & 0xc0) == 0x80)
/* Incorporate data from a continuation byte */
#define U8S(x,c) (((x) << 6) | ((c) & 0x3f))
/* Build a UTF-8 continuation byte (n = 0 for the least significant) */
#define U8B(uc,n) (0x80 | (((uc)>>(6*n)) & 0x3f))

/* Format strings for escaped UTF-8 and UTF-16 characters */
/* For UTF-16, also use the older \x notation for increased compatibility,
   rather than the newer \U */
#define F8 "\\x%02x"
#define F16 "\\x%04x"

/* Prints the (possibly multibyte) Unicode character c as Windows UTF-16
   Returns the number of bytes that composed the decoded character */
unsigned print_utf8_as_utf16(const char *c) {
  unsigned uc = 0, bytes = 0;

  /* Decode UTF8 */
  if ((c[0] & 0x80) == 0) {
    bytes = 1;
    uc = c[0];
  } else if ((c[0] & 0xe0) == 0xc0) {
    bytes = 2;
    assert(U8C(c[1]));
    uc = U8S(c[0] & 0x1f, c[1]);
  } else if ((c[0] & 0xf0) == 0xe0) {
    bytes = 3;
    assert(U8C(c[1]) && U8C(c[2]));
    uc = U8S(U8S(c[0] & 0xf, c[1]), c[2]);
  } else if ((c[0] & 0xf8) == 0xf0) {
    bytes = 4;
    assert(U8C(c[1]) && U8C(c[2]) && U8C(c[3]));
    uc = U8S(U8S(U8S(c[0] & 0x7, c[1]), c[2]), c[3]);
  }

  assert(bytes);

  /* Encode UTF16 */
  if (printable(uc)) {
    printf("%c", uc);
  } else {
    if (uc <= 0xd7ff || (uc >= 0xe000 && uc <= 0xffff)) {
      printf(F16, uc);
    } else {
      assert(uc >= 0x10000);    /* otherwise we are in the prohibited range */
      uc -= 0x10000;
      printf(F16 F16, 0xd800 | ((uc >> 10) & 0x3ff), 0xdc00 | (uc & 0x3ff));
    }
  }

  return bytes;
}

/* Prints the Unicode character c (possibly encoded as two wchar_t) as UTF-8
   Returns the number of wchar_t that composed the decoded character */
unsigned print_utf16_as_utf8(const wchar_t *c) {
  unsigned uc = 0, wchars = 0;

  /* Decode UTF-16 */
  if ((c[0] >= 0 && c[0] <= 0xd7ff) || (c[0] >= 0xe000 && c[0] <= 0xffff)) {
    wchars = 1;
    uc = c[0];
  } else {
    wchars = 2;
    assert(c[0] >= 0xd800 && c[0] <= 0xdbff && c[1] >= 0xdc00
           && c[1] <= 0xdfff);
    uc = ((c[0] & 0x3ff) << 10 | (c[1] & 0x3ff)) + 0x10000;
  }

  assert(wchars);

  /* Encode UTF-8 */
  /* All non-printable characters are encoded, this is easier to proof-read */
  if (printable(uc)) {
    printf("%c", uc);
  } else {
    if (uc < 0x80) {
      printf(F8, uc);
    } else if (uc < 0x800) {
      printf(F8 F8, 0xc0 | ((uc >> 6) & 0x1f), U8B(uc, 0));
    } else if (uc < 0x10000) {
      printf(F8 F8 F8, 0xe0 | ((uc >> 12) & 0xf), U8B(uc, 1), U8B(uc, 0));
    } else {
      assert(uc < 0x110000);
      printf(F8 F8 F8 F8, 0xf0 | ((uc >> 18) & 0x7), U8B(uc, 2), U8B(uc, 1),
             U8B(uc, 0));
    }
  }

  return wchars;
}

/* Converts the supplied path (UTF-8 on Unix and UCS-2ish on Windows) to a valid
   C string literal. On Windows, this is always a wchar_t* (L"..."). */
static void encode_C_literal(const char_os * path, bool utf8)
{
  char_os c;


  if(!utf8) putchar('L');
  putchar('"');

  while ((c = *path) != 0) {
    /* Escape \, ", \n and \r */
    if (c == '\\') {
      printf("\\\\");
    } else if (c == '"') {
      printf("\\\"");
    } else if (c == '\n') {
      printf("\\n");
    } else if (c == '\r') {
      printf("\\r");
    } else {
#ifndef _WIN32
      if (utf8) {
        /* On Unix, nothing else needs escaping */
        putchar(c);
      } else {
        path += print_utf8_as_utf16(path) - 1;
      }
#else
      if (utf8) {
        path += print_utf16_as_utf8(path) - 1;
      } else {
        /* On Windows, allow 7-bit printable characters to be displayed
           literally and escape everything else. */
        if (printable(c)) {
          putwchar(c);
        } else {
          printf(F16, c);
        }
      }
#endif
    }
    path++;
  }

  putchar('"');
}

int main_os(int argc, char_os **argv)
{
  if (argc == 3 && !strcmp_os(argv[1], T("encode-C-utf8-literal"))) {
    encode_C_literal(argv[2], true);
  } else if (argc == 3 && !strcmp_os(argv[1], T("encode-C-utf16-literal"))) {
    encode_C_literal(argv[2], false);
  } else {
    usage();
    return 1;
  }

  return 0;
}
