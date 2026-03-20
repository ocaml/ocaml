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

#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#define DIMX 6
#define DIMY 8

double ctab[DIMX][DIMY];

void filltab(void)
{
  for (int x = 0; x < DIMX; x++)
    for (int y = 0; y < DIMY; y++)
      ctab[x][y] = x * 100 + y;
}

void printtab(double tab[DIMX][DIMY])
{
  for (int x = 0; x < DIMX; x++) {
    printf("%3d", x);
    for (int y = 0; y < DIMY; y++)
      printf("  %6.1f", tab[x][y]);
    printf("\n");
  }
}

value c_filltab(value unit)
{
  filltab();
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable : 5287)
#endif
  return caml_ba_alloc_dims(CAML_BA_FLOAT64 | CAML_BA_C_LAYOUT,
                            2, ctab, (intnat)DIMX, (intnat)DIMY);
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif
}

value c_printtab(value ba)
{
  printtab(Caml_ba_data_val(ba));
  fflush(stdout);
  return Val_unit;
}
