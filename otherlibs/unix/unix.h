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

#define Nothing ((value) 0)

#ifndef NULL
#ifdef __STDC__
#define NULL ((void *) 0)
#else
#define NULL ((char *) 0)
#endif
#endif

#ifdef __STDC__
extern void unix_error(int errcode, char * cmdname, value arg);
extern void uerror(char * cmdname, value arg);
#else
void unix_error();
void uerror();
#endif

