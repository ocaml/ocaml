/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <sys/types.h>
#include <sys/param.h>

#ifdef NGROUPS

int main(void)
{
  int gidset[NGROUPS];
  if (getgroups(NGROUPS, gidset) == -1) return 1;
  return 0;
}

#else

int main(void) { return 1; }

#endif
