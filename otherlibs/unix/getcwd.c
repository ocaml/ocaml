/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "unixsupport.h"

#if !defined (_WIN32) && !macintosh
#include <sys/param.h>
#endif

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 512
#endif
#endif

#ifdef HAS_GETCWD
#ifdef UTF16
#include "u8tou16.h"
#endif

CAMLprim value unix_getcwd(value unit)
{
#ifdef UTF16
  CAMLparam0 ();
  CAMLlocal1 (v);
  WCHAR buff[PATH_MAX*2];
  unsigned char* temp;
  if (_wgetcwd(buff, sizeof(buff)/sizeof(WCHAR)) == 0) uerror("getcwd", Nothing);
  temp=utf16_to_utf8(buff);
  v=copy_string(temp);
  free(temp);
  CAMLreturn (v);
#else
  char buff[PATH_MAX];
  if (getcwd(buff, sizeof(buff)) == 0) uerror("getcwd", Nothing);
  return copy_string(buff);
#endif
}

#else
#ifdef HAS_GETWD

CAMLprim value unix_getcwd(value unit)
{
  char buff[PATH_MAX];
  if (getwd(buff) == 0) uerror("getcwd", copy_string(buff));
  return copy_string(buff);
}

#else

CAMLprim value unix_getcwd(value unit)
{ invalid_argument("getcwd not implemented"); }

#endif
#endif
