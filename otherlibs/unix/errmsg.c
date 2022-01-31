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
#include <string.h>

#define CAML_INTERNALS
#include <caml/alloc.h>
#include <caml/misc.h>
#include "unixsupport.h"

CAMLprim value unix_error_message(value err)
{
  int errnum = code_of_unix_error(err);
  return caml_copy_string(caml_strerror(errnum));
}
