/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1995 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifdef WIN32
#include <processthreadsapi.h>
#include <windows.h>
#else
#define _GNU_SOURCE

#include <pthread.h>
#include <unistd.h>

#ifdef __FreeBSD__
#include <pthread_np.h>
#endif
#endif

#include "caml/misc.h"
#include "caml/mlvalues.h"

/* Set the current thread's name. */
CAMLprim value caml_thread_set_name(value name)
{
#ifdef WIN32
  SetThreadDescription(GetCurrentThreadId(), String_val(name));
#elif __APPLE__
  pthread_setname_np(String_val(name));
#else
  pthread_setname_np(pthread_self(), String_val(name));
#endif
  return Val_unit;
}
