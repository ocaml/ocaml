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

#include <stdio.h>
#include <string.h>

/* Check for the availability of "long long" type as per ISO C9X */

int main(int argc, char **argv)
{
  long long l;
  unsigned long long u;
  char buffer[64];

  if (sizeof(long long) != 8) return 1;
  l = 123456789123456789LL;
  buffer[0] = 0;
  sprintf(buffer, "%lld", l);
  if (strcmp(buffer, "123456789123456789") != 0) return 1;
  return 0;
}
