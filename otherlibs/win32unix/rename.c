/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Contributed by Tracy Camp, PolyServe Inc., <campt@polyserve.com>   */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdio.h>
#include <mlvalues.h>
#include "unixsupport.h"

CAMLprim value unix_rename(value path1, value path2)
{
  if (MoveFileEx(String_val(path1), String_val(path2),
		 MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH |
		 MOVEFILE_COPY_ALLOWED) == 0) {
    win32_maperr(GetLastError());
    uerror("rename", path1);
  }	
  return Val_unit;
}
