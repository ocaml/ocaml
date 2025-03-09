/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         Antonin Decimo, Tarides                        */
/*                                                                        */
/*   Copyright 2024 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Definitions for compatibility with old identifiers. */

#ifndef CAML_COMPATIBILITY_H
#define CAML_COMPATIBILITY_H

#define HAS_STDINT_H 1 /* Deprecated since OCaml 5.3 */

/* HAS_NANOSECOND_STAT is deprecated since OCaml 5.3 */
#if defined(HAVE_STRUCT_STAT_ST_ATIM_TV_NSEC)
#  define HAS_NANOSECOND_STAT 1
#elif defined(HAVE_STRUCT_STAT_ST_ATIMESPEC_TV_NSEC)
#  define HAS_NANOSECOND_STAT 2
#elif defined(HAVE_STRUCT_STAT_ST_ATIMENSEC)
#  define HAS_NANOSECOND_STAT 3
#endif

#ifndef _WIN32
/* unistd.h is assumed to be available */
#define HAS_UNISTD 1
#endif

#endif  /* CAML_COMPATIBILITY_H */
