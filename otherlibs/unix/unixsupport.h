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

#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#define Nothing ((value) 0)

extern void unix_error P((int errcode, char * cmdname, value arg)) Noreturn;
extern void uerror P((char * cmdname, value arg)) Noreturn;

#define UNIX_BUFFER_SIZE 16384
