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

#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "unixsupport.h"

char ** cstringvect(value arg, char * cmdname)
{
  char ** res;
  mlsize_t size, i;

  size = Wosize_val(arg);
  for (i = 0; i < size; i++)
    if (! caml_string_is_c_safe(Field(arg, i)))
      unix_error(EINVAL, cmdname, Field(arg, i));
  res = (char **) caml_stat_alloc((size + 1) * sizeof(char *));
  for (i = 0; i < size; i++) res[i] = (char *)String_val(Field(arg, i));
  res[size] = NULL;
  return res;
}
