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

#if defined(__MINGW32__) || _MSC_VER >= 1600
#define HAS_STDINT_H
#endif
#undef BSD_SIGNALS
#define HAS_STRERROR
#define HAS_SOCKETS
#define HAS_GETCWD
#define HAS_UTIME
#define HAS_DUP2
#define HAS_GETHOSTNAME
#define HAS_MKTIME
#define HAS_PUTENV
#ifndef __MINGW32__
#define HAS_LOCALE_H
#define HAS_STRTOD_L
#endif
#define HAS_BROKEN_PRINTF
#define HAS_IPV6
#define HAS_NICE
#define SUPPORT_DYNAMIC_LINKING
#define HAS_EXECVPE
#if defined(_MSC_VER) && _MSC_VER < 1300
#define LACKS_SANE_NAN
#define LACKS_VSCPRINTF
#endif
