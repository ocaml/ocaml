/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "m.h"

#ifndef SIXTYFOUR
long intval = 0x41424344L;
char * bigendian = "ABCD";
char * littleendian = "DCBA";
#else
long intval = 0x4142434445464748L;
char * bigendian = "ABCDEFGH";
char * littleendian = "HGFEDCBA";
#endif

main()
{
  long n[2];
  char * p;

  n[0] = intval;
  n[1] = 0;
  p = (char *) n;
  if (strcmp(p, bigendian) == 0)
    exit(0);
  if (strcmp(p, littleendian) == 0)
    exit(1);
  exit(2);
}
