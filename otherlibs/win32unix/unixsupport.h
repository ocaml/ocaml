/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdlib.h>
/* Include io.h in current dir, which is a copy of the system's io.h,
   not io.h from ../../byterun */
#include "io.h"
#include <direct.h>
#include <process.h>

#define Nothing ((value) 0)

extern void unix_error P((int errcode, char * cmdname, value arg));
extern void uerror P((char * cmdname, value arg));
extern value unix_freeze_buffer P((value));
