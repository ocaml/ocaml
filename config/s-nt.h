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

/* Operating system dependencies, Intel x86 processors, Windows NT */

#define HAS_MEMMOVE
#define BSD_SIGNALS
#define HAS_STRERROR
#define HAS_GETCWD
#define HAS_DUP2
#define HAS_SELECT
#define HAS_GETHOSTNAME
