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

#include <errno.h>
#include <string.h>
#include <mlvalues.h>
#include <alloc.h>

extern int error_table[];

CAMLprim value unix_error_message(value err)
{
  int errnum;
  errnum = Is_block(err) ? Int_val(Field(err, 0)) : error_table[Int_val(err)];
  return copy_string(strerror(errnum));
}
