/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <sys/types.h>
#include <sys/stat.h>
#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_FCHMOD

value unix_fchmod(value fd, value perm)      /* ML */
{
  if (fchmod(Int_val(fd), Int_val(perm)) == -1) uerror("fchmod", Nothing);
  return Val_unit;
}

#else

value unix_fchmod(value fd, value perm)
{ invalid_argument("fchmod not implemented"); }
  
#endif
