/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Processor dependencies */

#define ARCH_BIG_ENDIAN

/* Operating system and standard library dependencies. */

#define OCAML_OS_TYPE "MacOS"
#define HAS_MEMMOVE
#define HAS_BCOPY
#define HAS_STRERROR
#define HAS_GETCWD
#define HAS_UI

#if !powerc
#define THREADED_CODE
#endif
