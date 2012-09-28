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

/* $Id: opendir.c 12858 2012-08-10 14:45:51Z maranget $ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

CAMLprim value unix_opendir(value path)
{
  DIR * d;
  value res;
  d = opendir(String_val(path));
  if (d == (DIR *) NULL) uerror("opendir", path);
  res = alloc_small(1, Abstract_tag);
  DIR_Val(res) = d;
  return res;
}
