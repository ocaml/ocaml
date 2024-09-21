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

/* POSIX thread implementation of the "st" interface */

#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif

Caml_inline void st_msleep(int msec)
{
  struct timeval timeout = {0, msec * 1000};
  select(0, NULL, NULL, NULL, &timeout);
}

#include "st_pthreads.h"
