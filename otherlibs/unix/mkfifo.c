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

#include <sys/types.h>
#include <sys/stat.h>
#include <fail.h>
#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_MKFIFO

CAMLprim value unix_mkfifo(value path, value mode)
{
  if (mkfifo(String_val(path), Int_val(mode)) == -1)
    uerror("mkfifo", path);
  return Val_unit;
}

#else

#include <sys/types.h>
#include <sys/stat.h>

#ifdef S_IFIFO

CAMLprim value unix_mkfifo(value path, value mode)
{
  if (mknod(String_val(path), (Int_val(mode) & 07777) | S_IFIFO, 0) == -1)
    uerror("mkfifo", path);
  return Val_unit;
}

#else

CAMLprim value unix_mkfifo(value path, value mode)
{
  invalid_argument("mkfifo not implemented");
}

#endif
#endif
