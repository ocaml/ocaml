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

#include <assert.h>
#include "caml/mlvalues.h"

void fp_backtrace(void);

value fp_backtrace_many_args(value a, value b, value c, value d, value e,
    value f, value g, value h, value i, value j, value k)
{
  assert(Int_val(a) == 1);
  assert(Int_val(b) == 2);
  assert(Int_val(c) == 3);
  assert(Int_val(d) == 4);
  assert(Int_val(e) == 5);
  assert(Int_val(f) == 6);
  assert(Int_val(g) == 7);
  assert(Int_val(h) == 8);
  assert(Int_val(i) == 9);
  assert(Int_val(j) == 10);
  assert(Int_val(k) == 11);

  fp_backtrace();

  return Val_unit;
}

value fp_bactrace_many_args_argv(value *argv, int argc)
{
  assert(argc == 11);

  return fp_backtrace_many_args(argv[0], argv[1], argv[2], argv[3], argv[4],
      argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
}
