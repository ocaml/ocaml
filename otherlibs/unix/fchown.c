/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_FCHMOD

value unix_fchown(fd, uid, gid)  /* ML */
     value fd, uid, gid;
{
  if (fchown(Int_val(fd), Int_val(uid), Int_val(gid)) == -1)
    uerror("fchown", Nothing);
  return Val_unit;
}

#else

value unix_fchown() { invalid_argument("fchown not implemented"); }
  
#endif
