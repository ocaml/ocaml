/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#define POSIX_SIGNALS
#define HAS_MEMMOVE
#define HAS_STRERROR
#define HAS_SOCKETS
#define HAS_SOCKLEN_T
#define HAS_UNISTD
#define HAS_DIRENT
#define HAS_REWINDDIR
#define HAS_GETCWD
#define HAS_UTIME
#define HAS_DUP2
#define HAS_TRUNCATE
#define HAS_SELECT
#define HAS_SYMLINK
#define HAS_GETHOSTNAME
#define HAS_GETTIMEOFDAY
#define HAS_MKTIME

#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

#define UNIX_BUFFER_SIZE 2048
