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

#define CAML_INTERNALS
#include "caml/misc.h"

#ifdef HAS_NANOSLEEP

#include <time.h>

typedef struct timespec st_timeout;

Caml_inline st_timeout st_timeout_of_msec(int msec)
{
  return (st_timeout){ .tv_sec = 0, .tv_nsec = msec * NSEC_PER_MSEC };
}

Caml_inline void st_msleep(const st_timeout *timeout)
{
  nanosleep(timeout, NULL);
}

#else

#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif

typedef struct timeval st_timeout;

Caml_inline st_timeout st_timeout_of_msec(int msec)
{
  return (st_timeout){ .tv_sec = 0, .tv_usec = msec * USEC_PER_MSEC };
}

Caml_inline void st_msleep(st_timeout *timeout)
{
  select(0, NULL, NULL, NULL, timeout);
}
#endif

#include "st_pthreads.h"
