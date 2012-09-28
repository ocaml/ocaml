/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: truncate.c 12858 2012-08-10 14:45:51Z maranget $ */

#include <sys/types.h>
#include <mlvalues.h>
#include <fail.h>
#include <io.h>
#include "unixsupport.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#ifdef HAS_TRUNCATE

CAMLprim value unix_truncate(value path, value len)
{
  if (truncate(String_val(path), Long_val(len)) == -1)
    uerror("truncate", path);
  return Val_unit;
}

CAMLprim value unix_truncate_64(value path, value len)
{
  if (truncate(String_val(path), File_offset_val(len)) == -1)
    uerror("truncate", path);
  return Val_unit;
}

#else

CAMLprim value unix_truncate(value path, value len)
{ invalid_argument("truncate not implemented"); }

CAMLprim value unix_truncate_64(value path, value len)
{ invalid_argument("truncate not implemented"); }

#endif
