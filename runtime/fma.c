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

#if defined(_MSC_VER)
#include <windows.h>
#endif

#include <math.h>

/* caml_fma lives in a unit of its own since it's compiled with hardware fma
   support on Windows, as there are no correct software implementations
   available. */

double caml_fma(double x, double y, double z)
{
#ifdef _MSC_VER
  /* For Microsoft Visual Studio, we use cl's native SEH support to return nan
     on pre-Haswell / pre-Piledriver CPUs (zero-cost on x64; relatively low cost
     on x86). */
  __try {
#endif
    return fma(x, y, z);
#ifdef _MSC_VER
  } __except(GetExceptionCode() == EXCEPTION_ILLEGAL_INSTRUCTION
               ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH) {
    return NAN;
  }
#endif
}
