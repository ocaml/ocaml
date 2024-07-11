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

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <stdio.h>

value factorial(value n){
  CAMLparam1(n);
  CAMLlocal1(s);

  static char buf[256];
  int x = 1;
  int m = Int_val(n);
  for (int i = 1; i <= m; i++) x *= i;
  sprintf(buf,"%i",x);
  s = caml_copy_string(buf);
  CAMLreturn (s);
}
