/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"
#include <errno.h>

#ifdef HAS_GETPRIORITY

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

CAMLprim value unix_nice(value incr)
{
  int prio;
  errno = 0;
  prio = getpriority(PRIO_PROCESS, 0);
  if (prio == -1 && errno != 0)
    uerror("nice", Nothing);
  prio += Int_val(incr);
  if (setpriority(PRIO_PROCESS, 0, prio) == -1)
    uerror("nice", Nothing);
  return Val_int(prio);
}

#else

CAMLprim value unix_nice(value incr)
{
  int ret;
  errno = 0;
  ret = nice(Int_val(incr));
  if (ret == -1 && errno != 0) uerror("nice", Nothing);
  return Val_int(ret);
}

#endif
