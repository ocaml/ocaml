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
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "caml/unixsupport.h"
#include <unistd.h>
#include <errno.h>
#include <grp.h>

static value alloc_group_entry(const struct group *entry)
{
  CAMLparam0();
  CAMLlocal3(name, pass, mem);
  value res;

  name = caml_copy_string(entry->gr_name);
  /* on some platforms, namely Android, gr_passwd can be NULL,
     hence this workaround */
  pass = caml_copy_string(entry->gr_passwd ? entry->gr_passwd : "");
  mem = caml_copy_string_array((const char**)entry->gr_mem);
  res = caml_alloc_small(4, 0);
  Field(res,0) = name;
  Field(res,1) = pass;
  Field(res,2) = Val_int(entry->gr_gid);
  Field(res,3) = mem;
  CAMLreturn(res);
}

/* Arbitrary limit to prevent allocating too much memory */
#define CAML_GETGR_R_SIZE_MAX (1024 * 64)

CAMLprim value caml_unix_getgrnam(value name)
{
  value res;
  struct group * entry;
  if (! caml_string_is_c_safe(name)) caml_raise_not_found();

#ifdef HAVE_GETGRNAM_R
  long initlen = sysconf(_SC_GETGR_R_SIZE_MAX);
  size_t len = initlen <= 0 ? /* default */ 1024 : (size_t) initlen;
  struct group result;
  char *buffer = caml_stat_alloc_noexc(len);
  if (buffer == NULL)
    caml_unix_error(ENOMEM, "getgrnam", Nothing);
  int e;
  while ((e = getgrnam_r(String_val(name), &result, buffer, len, &entry))
         == ERANGE) {
    len *= 2;
    char *newbuffer;
    if (len > CAML_GETGR_R_SIZE_MAX ||
        (newbuffer = caml_stat_resize_noexc(buffer, len)) == NULL) {
      caml_stat_free(buffer);
      caml_unix_error(ENOMEM, "getgrnam", Nothing);
    }
    buffer = newbuffer;
  }
  if (e != 0) {
    caml_stat_free(buffer);
    if (e == EINTR)
#else
  errno = 0;
  entry = getgrnam(String_val(name));
  if (entry == NULL) {
    if (errno == EINTR)
#endif
      caml_unix_error(EINTR, "getgrnam", Nothing);
    caml_raise_not_found();
  }
  res = alloc_group_entry(entry);
#if HAVE_GETGRNAM_R
  caml_stat_free(buffer);
#endif
  return res;
}

CAMLprim value caml_unix_getgrgid(value gid)
{
  value res;
  struct group * entry;

#ifdef HAVE_GETGRGID_R
  long initlen = sysconf(_SC_GETGR_R_SIZE_MAX);
  size_t len = initlen <= 0 ? /* default */ 1024 : (size_t) initlen;
  struct group result;
  char *buffer = caml_stat_alloc_noexc(len);
  if (buffer == NULL)
    caml_unix_error(ENOMEM, "getgrid", Nothing);
  int e;
  while ((e = getgrgid_r(Int_val(gid), &result, buffer, len, &entry))
         == ERANGE) {
    len *= 2;
    char *newbuffer;
    if (len > CAML_GETGR_R_SIZE_MAX ||
        (newbuffer = caml_stat_resize_noexc(buffer, len)) == NULL) {
      caml_stat_free(buffer);
      caml_unix_error(ENOMEM, "getgrid", Nothing);
    }
    buffer = newbuffer;
  }
  if (e != 0) {
    caml_stat_free(buffer);
    if (e == EINTR)
#else
  errno = 0;
  entry = getgrgid(Int_val(gid));
  if (entry == NULL) {
    if (errno == EINTR)
#endif
      caml_unix_error(EINTR, "getgrgid", Nothing);
    caml_raise_not_found();
  }
  res = alloc_group_entry(entry);
#if HAVE_GETGRGID_R
  caml_stat_free(buffer);
#endif
  return res;
}
