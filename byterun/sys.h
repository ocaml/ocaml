/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _sys_
#define _sys_

#include "misc.h"

#define NO_ARG Val_int(0)
void sys_error (value);
void sys_init (char **);
value sys_exit (value);
char * searchpath (char * name);

#endif /* _sys_ */
