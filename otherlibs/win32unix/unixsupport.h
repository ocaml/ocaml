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

/* Namespace pollution!  One of the Win32 includes has a struct field
   called `Blue'... */
#undef Blue
#include <wtypes.h>
#include <winbase.h>
#include <stdlib.h>
/* Include io.h in current dir, which is a copy of the system's io.h,
   not io.h from ../../byterun */
#include "io.h"
#include <direct.h>
#include <process.h>

#define Handle_val(v) (*((HANDLE *)(v)))

extern value win_alloc_handle(HANDLE);

#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg);
extern void uerror (char * cmdname, value arg);
extern value unix_freeze_buffer (value);

#define UNIX_BUFFER_SIZE 16384
