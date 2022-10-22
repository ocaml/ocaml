/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 David Allsopp, OCaml Labs, Cambridge.                  */
/*                                                                        */
/*   Copyright 2017 MetaStack Solutions Ltd.                              */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Need at least Windows Vista for WC_ERR_INVALID_CHARS */
#define _WIN32_WINNT 0x600
#define WINVER 0x600
#include <windows.h>

/* See corresponding values in runtime/win32.c */
static int windows_unicode_enabled = WINDOWS_UNICODE;
static int windows_unicode_strict = 1;

/* Adapted from runtime/win32.c */
int caml_win32_wide_char_to_multi_byte(const wchar_t *s, int slen,
                                       char *out, int outlen)
{
  int retcode;

  if (slen == 0)
    return 0;

  if (windows_unicode_enabled != 0)
    retcode =
      WideCharToMultiByte(CP_UTF8,
                          windows_unicode_strict ? WC_ERR_INVALID_CHARS : 0,
                          s, slen, out, outlen, NULL, NULL);
  else
    retcode =
      WideCharToMultiByte(CP_ACP, 0, s, slen, out, outlen, NULL, NULL);

  if (retcode == 0)
    return -1;

  return retcode;
}

char* caml_stat_strdup_of_utf16(const wchar_t *s)
{
  char *out = NULL;
  int retcode;

  retcode = caml_win32_wide_char_to_multi_byte(s, -1, NULL, 0);
  if (retcode >= 0) {
    out = (char *)malloc(retcode);
    caml_win32_wide_char_to_multi_byte(s, -1, out, retcode);
  }

  return out;
}
