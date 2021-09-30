/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_NAME_SPACE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

extern void filltab_(void);
extern void printtab_(float * data, int * dimx, int * dimy);
extern float ftab_[];

value fortran_filltab(value unit)
{
  filltab_();
  return caml_ba_alloc_dims(CAML_BA_FLOAT32 | CAML_BA_FORTRAN_LAYOUT,
                            2, ftab_, (intnat)8, (intnat)6);
}

value fortran_printtab(value ba)
{
  int dimx = Caml_ba_array_val(ba)->dim[0];
  int dimy = Caml_ba_array_val(ba)->dim[1];
  printtab_(Caml_ba_data_val(ba), &dimx, &dimy);
  return Val_unit;
}
