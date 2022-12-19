/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2009 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Win32 implementation of the "st" interface */

/* This is a code smell - why is mingw-w64 not affected?? */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#endif

Caml_inline void st_msleep(int msec)
{
  Sleep(msec);
}

#include "st_pthreads.h"

/* Signal handling -- none under Win32 */

value caml_thread_sigmask(value cmd, value sigs)
{
  caml_invalid_argument("Thread.sigmask not implemented");
  return Val_int(0);            /* not reached */
}

value caml_wait_signal(value sigs)
{
  caml_invalid_argument("Thread.wait_signal not implemented");
  return Val_int(0);            /* not reached */
}
