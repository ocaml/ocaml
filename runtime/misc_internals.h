/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         Antonin Decimo, Tarides                        */
/*                                                                        */
/*   Copyright 2025 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_MISC_INTERNALS_H
#define CAML_MISC_INTERNALS_H

/* Detection of available C features and compiler extensions */

#ifndef __has_feature
#define __has_feature(x) 0
#endif

#ifndef __has_extension
#define __has_extension __has_feature
#endif

#ifndef __has_include
#define __has_include(x) 0
#endif

/* C2y countof */

#if __has_include(<stdcountof.h>)
#include <stdcountof.h>
#elif __has_extension(c_countof)
#define countof _Countof
#else
#define countof(a) (sizeof(a)/sizeof(*(a)))
#endif

#endif  /* CAML_MISC_INTERNALS */
