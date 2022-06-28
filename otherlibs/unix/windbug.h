/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Contributed by Sylvain Le Gall for Lexifi                            */
/*                                                                        */
/*   Copyright 2008 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifdef DEBUG

#include <stdio.h>
#include <windows.h>

/* According to MSDN, MSVC supports the gcc ## operator (to deal with empty
   argument lists)
 */
#define DEBUG_PRINT(fmt, ...) \
  do \
  { \
    if (caml_win32_debug_test()) \
    { \
      fprintf(stderr, "DBUG (pid:%ld, tid: %ld): ", GetCurrentProcessId(), \
              GetCurrentThreadId()); \
      fprintf(stderr, fmt, ##__VA_ARGS__); \
      fprintf(stderr, "\n"); \
      fflush(stderr); \
    }; \
  } while(0)

/* Test if we are in dbug mode */
int  caml_win32_debug_test    (void);

#else

/* Visual Studio supports variadic macros in all versions from 2008 (CL 15). */
#define DEBUG_PRINT(fmt, ...)

#endif
