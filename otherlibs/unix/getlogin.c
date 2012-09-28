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

/* $Id: getlogin.c 12858 2012-08-10 14:45:51Z maranget $ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"
#include <errno.h>

extern char * getlogin(void);

CAMLprim value unix_getlogin(value unit)
{
  char * name;
  name = getlogin();
  if (name == NULL) unix_error(ENOENT, "getlogin", Nothing);
  return copy_string(name);
}
