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

/* Operating system dependencies, Intel x86 processors, Windows NT */

#define OCAML_OS_TYPE "Windows NT"

#define HAS_MEMMOVE
#define BSD_SIGNALS
#define HAS_STRERROR
#define HAS_SOCKETS
#define HAS_GETCWD
#define HAS_UTIME
#define HAS_DUP2
#define HAS_GETHOSTNAME
#define HAS_MKTIME
