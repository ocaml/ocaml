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

#include <setjmp.h>

main()
{
  jmp_buf buf;
  int i;
  i = _setjmp(buf);
  if (i == 0) {
    _longjmp(buf, 12345);
  }
  exit (i != 12345);
}
