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

#define Nothing ((value) 0)

extern void unix_error P((int errcode, char * cmdname, value arg));
extern void uerror P((char * cmdname, value arg));
extern value unix_freeze_buffer P((value));
