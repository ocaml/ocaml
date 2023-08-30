/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         David Allsopp, Tarides                         */
/*                                                                        */
/*   Copyright 2023 David Allsopp Ltd.                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <math.h>

/* caml_fma lives in a unit of its own since it's compiled with hardware fma
   support on Windows, as there are no correct software implementations
   available. */

double caml_fma(double x, double y, double z)
{
  return fma(x, y, z);
}
