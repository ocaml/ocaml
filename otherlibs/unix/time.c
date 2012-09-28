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

/* $Id: time.c 12858 2012-08-10 14:45:51Z maranget $ */

#include <time.h>
#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

CAMLprim value unix_time(value unit)
{
  return copy_double((double) time((time_t *) NULL));
}
