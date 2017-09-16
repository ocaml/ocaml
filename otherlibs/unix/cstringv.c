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

#define CAML_INTERNALS

#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

charnat ** cstringvect(value arg, char * cmdname)
{
  charnat ** res;
  mlsize_t size, i;

  size = Wosize_val(arg);
  for (i = 0; i < size; i++)
    if (! caml_string_is_c_safe(Field(arg, i)))
      unix_error(EINVAL, cmdname, Field(arg, i));
  res = (charnat **) caml_stat_alloc((size + 1) * sizeof(charnat *));
  for (i = 0; i < size; i++) res[i] = caml_stat_strdup_to_utf16(String_val(Field(arg, i)));
  res[size] = NULL;
  return res;
}

void cstringvect_free(charnat ** v)
{
  int i = 0;
  while (v[i]) caml_stat_free(v[i++]);
  caml_stat_free((char *)v);
}
