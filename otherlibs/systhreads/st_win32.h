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

#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0400
#include <windows.h>

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
