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

/* Operating system and standard library dependencies. */

/* 0. This is a Unix machine. */

#ifndef unix
#define unix
#endif

/* 1. For the runtime system. */

#define HAS_MEMMOVE

/* Define HAS_MEMMOVE if you have `memmove', and if
  `memmove' is guaranteed to work even on overlapping areas.
  That's the case if your library is ANSI C compliant. */

#define HAS_BCOPY

/* Define HAS_BCOPY if you have `bcopy', and if
   `bcopy' is guaranteed to work even on overlapping areas. */

/* If none of HAS_MEMMOVE and HAS_BCOPY is defined, then we'll
   use our own memory copy routine. It might be slightly slower than the
   routines provided in the standard library, but at least it is guaranteed
   to work. So, in doubt, don't define anything. */

#define BSD_SIGNALS

/* Define BSD_SIGNALS if signal handlers have the BSD semantics: the handler
   remains attached to the signal when the signal is received. Leave it
   undefined if signal handlers have the System V semantics: the signal
   resets the behavior to default. */

#define HAS_RENAME

/* Define HAS_RENAME if you have rename(). */

#define HAS_STRERROR

/* Define HAS_STRERROR if you have strerror(). */

/* 2. For the Unix library. */

#define HAS_SOCKETS

/* Define HAS_SOCKETS if you have BSD sockets. */

#define HAS_UNISTD

/* Define HAS_UNISTD if you have /usr/include/unistd.h. */

#define HAS_DIRENT

/* Define HAS_DIRENT if you have /usr/include/dirent.h and the result of
   readdir() is of type struct dirent *.
   Otherwise, we'll load /usr/include/sys/dir.h, and readdir() is expected to
   return a struct direct *. */

#define HAS_LOCKF

/* Define HAS_LOCKF if the library provides the lockf() function. */

#define HAS_MKFIFO

/* Define HAS_MKFIFO if the library provides the mkfifo() function. */

#define HAS_GETCWD
#define HAS_GETWD

/* Define HAS_GETCWD if the library provides the getcwd() function. */
/* Define HAS_GETWD if the library provides the getwd() function. */

#define HAS_GETPRIORITY

/* Define HAS_GETPRIORITY if the library provides getpriority() and
   setpriority(). Otherwise, we'll use nice(). */

#define HAS_UTIME
#define HAS_UTIMES

/* Define HAS_UTIME if you have /usr/include/utime.h and the library
   provides utime(). Define HAS_UTIMES if the library provides utimes(). */

#define HAS_DUP2

/* Define HAS_DUP2 if you have dup2(). */

#define HAS_FCHMOD

/* Define HAS_FCHMOD if you have fchmod() and fchown(). */

#define HAS_TRUNCATE

/* Define HAS_TRUNCATE if you have truncate() and
   ftruncate(). */

#define HAS_SELECT

/* Define HAS_SELECT if you have select(). */

#define HAS_SYS_SELECT_H

/* Define HAS_SYS_SELECT_H if /usr/include/sys/select.h exists
   and should be included before using select(). */

#define HAS_SYMLINK

/* Define HAS_SYMLINK if you have symlink() and readlink() and lstat(). */

#define HAS_WAIT3
#define HAS_WAITPID

/* Define HAS_WAIT3 if you have wait3().
   Define HAS_WAITPID if you have waitpid(). */

#define HAS_GETGROUPS

/* Define HAS_GETGROUPS if you have getgroups(). */

#define HAS_TERMIOS

/* Define HAS_TERMIOS if you have /usr/include/termios.h and it is
   Posix-compliant. */

#define HAS_ASYNC_IO

/* Define HAS_ASYNC_IO if BSD-style asynchronous I/O are supported
   (the process can request to be sent a SIGIO signal when a descriptor
   is ready for reading). */

#define HAS_SETITIMER

/* Define HAS_SETITIMER if you have setitimer(). */

#define HAS_GETHOSTNAME

/* Define HAS_GETHOSTNAME if you have gethostname(). */

#define HAS_UNAME

/* Define HAS_UNAME if you have uname(). */
