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

#include <mlvalues.h>
#include "unixsupport.h"

value unix_dup(value fd)               /* ML */
{
  HANDLE newh;
  if (! DuplicateHandle(GetCurrentProcess(), Handle_val(fd),
                        GetCurrentProcess(), &newh,
                        0L, TRUE, DUPLICATE_SAME_ACCESS)) {
    _dosmaperr(GetLastError());
    return -1;
  }
  return win_alloc_handle(newh);
}

