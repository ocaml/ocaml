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

/* $Id: link.c 12858 2012-08-10 14:45:51Z maranget $ */

#include <mlvalues.h>
#include "unixsupport.h"

CAMLprim value unix_link(value path1, value path2)
{
  if (link(String_val(path1), String_val(path2)) == -1) uerror("link", path2);
  return Val_unit;
}
