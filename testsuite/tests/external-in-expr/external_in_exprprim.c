/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1995 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "caml/mlvalues.h"
#include "caml/alloc.h"

intnat var_global = 42;

value foobar(value i){
  return Val_long(2+(Long_val(i)));
}

intnat foobar_unboxed(intnat i){
  return 2+i;
}


typedef value (*run_one1)(value);

value run_one(value f, value arg1){
  return ((run_one1) (Nativeint_val(f)))(arg1);
}

typedef intnat (*run_one_unbox1)(intnat);

value run_one_unbox(value f, value arg1){
  return (Val_long (((run_one_unbox1) (Nativeint_val(f)))(Long_val(arg1))));
}

value get_foobar_global(value unit){
  return caml_copy_nativeint((intnat) &foobar);
}

value get_var_global_boxed(value unit){
  return caml_copy_nativeint((intnat) &var_global);
}

intnat get_var_global(value unit){
  return (intnat) &var_global;
}

void shouldnotbecalled(void){
  exit(1);
}
