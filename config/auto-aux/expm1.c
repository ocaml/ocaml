/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2011 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: longlong.c 4833 2002-05-25 08:33:26Z xleroy $ */

#include <math.h>

volatile double x;

int main(int argc, char **argv)
{
  x = 3.1415;
  x = expm1(x);
  x = log1p(x);
  return 0;
}
