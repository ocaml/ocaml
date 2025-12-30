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

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "caml/unixsupport.h"
#include <errno.h>
#include <pwd.h>

static value alloc_passwd_entry(const struct passwd *entry)
{
  CAMLparam0();
  CAMLlocal5(name, passwd, gecos, dir, shell);
  value res;

  name = caml_copy_string(entry->pw_name);
  passwd = caml_copy_string(entry->pw_passwd);
#if !defined(__BEOS__) && !defined(__ANDROID__)
  gecos = caml_copy_string(entry->pw_gecos);
#else
  gecos = caml_copy_string("");
#endif
  dir = caml_copy_string(entry->pw_dir);
  shell = caml_copy_string(entry->pw_shell);
  res = caml_alloc_small(7, 0);
  Field(res,0) = name;
  Field(res,1) = passwd;
  Field(res,2) = Val_int(entry->pw_uid);
  Field(res,3) = Val_int(entry->pw_gid);
  Field(res,4) = gecos;
  Field(res,5) = dir;
  Field(res,6) = shell;
  CAMLreturn(res);
}

/* Arbitrary limit to prevent allocating too much memory */
#define CAML_GETPW_R_SIZE_MAX (1024 * 64)

CAMLprim value caml_unix_getpwnam(value name)
{
  value res;
  struct passwd * entry;
  if (! caml_string_is_c_safe(name)) caml_raise_not_found();

#ifdef HAVE_GETPWNAM_R
  long initlen = sysconf(_SC_GETPW_R_SIZE_MAX);
  size_t len = initlen <= 0 ? /* default */ 1024 : (size_t) initlen;
  struct passwd result;
  char *buffer = caml_stat_alloc_noexc(len);
  if (buffer == NULL)
    caml_unix_error(ENOMEM, "getpwnam", Nothing);
  int e;
  while ((e = getpwnam_r(String_val(name), &result, buffer, len, &entry))
         == ERANGE) {
    len *= 2;
    char *newbuffer;
    if (len > CAML_GETPW_R_SIZE_MAX ||
        (newbuffer = caml_stat_resize_noexc(buffer, len)) == NULL) {
      caml_stat_free(buffer);
      caml_unix_error(ENOMEM, "getpwnam", Nothing);
    }
    buffer = newbuffer;
  }
  if (e != 0) {
    caml_stat_free(buffer);
    if (e == EINTR)
#else
  errno = 0;
  entry = getpwnam(String_val(name));
  if (entry == NULL) {
    if (errno == EINTR)
#endif
      caml_unix_error(EINTR, "getpwnam", Nothing);
    caml_raise_not_found();
  }
  res = alloc_passwd_entry(entry);
#if HAVE_GETPWNAM_R
  caml_stat_free(buffer);
#endif
  return res;
}

CAMLprim value caml_unix_getpwuid(value uid)
{
  value res;
  struct passwd * entry;

#ifdef HAVE_GETPWUID_R
  long initlen = sysconf(_SC_GETPW_R_SIZE_MAX);
  size_t len = initlen <= 0 ? /* default */ 1024 : (size_t) initlen;
  struct passwd result;
  char *buffer = caml_stat_alloc(len);
  if (buffer == NULL)
    caml_unix_error(ENOMEM, "getpwuid", Nothing);
  int e;
  while ((e = getpwuid_r(Int_val(uid), &result, buffer, len, &entry))
         == ERANGE) {
    len *= 2;
    char *newbuffer;
    if (len > CAML_GETPW_R_SIZE_MAX ||
        (newbuffer = caml_stat_resize_noexc(buffer, len)) == NULL) {
      caml_stat_free(buffer);
      caml_unix_error(ENOMEM, "getpwuid", Nothing);
    }
    buffer = newbuffer;
  }
  if (e != 0) {
    caml_stat_free(buffer);
    if (e == EINTR)
#else
  errno = 0;
  entry = getpwuid(Int_val(uid));
  if (entry == NULL) {
    if (errno == EINTR)
#endif
      caml_unix_error(EINTR, "getpwuid", Nothing);
    caml_raise_not_found();
  }
  res = alloc_passwd_entry(entry);
#if HAVE_GETPWUID_R
  caml_stat_free(buffer);
#endif
  return res;
}
