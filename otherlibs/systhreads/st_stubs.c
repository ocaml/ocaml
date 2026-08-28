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

/* The systhreads C implementation lives in the runtime (runtime/threads.c)
   and is linked into every program unconditionally. This placeholder keeps
   the historical stub libraries (libthreads, libthreadsnat, dllthreads) in
   existence, so that link lines mentioning them (e.g. -cclib -lthreadsnat)
   continue to work. */

int caml_systhreads_stubs_moved_to_runtime = 0;
