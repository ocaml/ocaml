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

#include <fail.h>
#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_SYMLINK

CAMLprim value unix_symlink(value path1, value path2)
{
  if (symlink(String_val(path1), String_val(path2)) == -1)
    uerror("symlink", path2);
  return Val_unit;
}

#else

CAMLprim value unix_symlink(value path1, value path2)
{ invalid_argument("symlink not implemented"); }

#endif
