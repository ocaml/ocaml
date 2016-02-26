/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*                         Alain Frisch, LexiFi                           */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include <stdio.h>

value stub1(void) {
  CAMLparam0();
  CAMLlocal1(x);
  printf("This is stub1!\n"); fflush(stdout);
  x = caml_copy_string("ABCDEF");
  CAMLreturn(x);
}
