/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
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

#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_SHORT 2

#if powerc
#define CPU_TYPE_STRING "PPC"
#else
#define CPU_TYPE_STRING "68k"
#define THREADED_CODE
#endif
