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

/* Query the number of rows for the terminal window. 
   Used for error reporting from the toplevel. */

#define CAML_INTERNALS

#include "caml/io.h"
#include "caml/mlvalues.h"

#ifndef _WIN32
#include <sys/ioctl.h>
#endif

CAMLprim value caml_terminfo_rows(value vchan)
{
#ifdef TIOCGWINSZ
  struct winsize w;
  w.ws_row = -1;
  if (ioctl(Channel(vchan)->fd, TIOCGWINSZ, &w) == 0)
    return Val_int(w.ws_row);
  else
    return Val_int(-1);
#else
  return Val_int(-1);
#endif
}

