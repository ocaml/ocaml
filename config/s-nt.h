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

/* Operating system dependencies, Intel x86 processors, Windows NT */

#define OCAML_OS_TYPE "Win32"

#define HAS_MEMMOVE
#undef BSD_SIGNALS
#define HAS_STRERROR
#define HAS_SOCKETS
#define HAS_GETCWD
#define HAS_UTIME
#define HAS_DUP2
#define HAS_GETHOSTNAME
#define HAS_MKTIME
#define HAS_PUTENV
