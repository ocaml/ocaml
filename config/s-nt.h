/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operating system dependencies, Intel x86 processors, Windows NT */

#define OCAML_OS_TYPE "Win32"

#undef BSD_SIGNALS
#define SUPPORT_DYNAMIC_LINKING 1
#define HAS_SOCKETS 1
#define HAS_IPV6 1
#if defined(__MINGW32__) || _MSC_VER >= 1600
#define HAS_STDINT_H 1
#endif
#ifdef __MINGW32__
#define HAS_UNISTD 1
#define HAS_DIRENT 1
#define HAS_REWINDDIR 1
#endif
#define HAS_GETCWD 1
#define HAS_UTIME 1
#ifdef __MINGW32__
#define HAS_TRUNCATE 1
#define HAS_NANOSLEEP 1
#endif
#define HAS_GETHOSTNAME 1
#ifdef __MINGW32__
#define HAS_GETTIMEOFDAY 1
#endif
#define HAS_MKTIME 1
#define HAS_PUTENV 1
#ifndef __MINGW32__
#define HAS_LOCALE_H 1
#define HAS_STRTOD_L 1
#endif
#ifdef __MINGW32__
#define HAS_MKSTEMP 1
#endif
#define HAS_NICE 1
#define HAS_EXECVPE 1
#define HAS_BROKEN_PRINTF 1
#define HAS_STRERROR 1
