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

value unix_mkdir(value path, value perm)     /* ML */
{
  if (mkdir(String_val(path), Int_val(perm)) == -1) uerror("mkdir", path);
  return Val_unit;
}
