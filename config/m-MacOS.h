/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#define ARCH_BIG_ENDIAN

#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_SHORT 2

#if powerc
#define ARCH_INT64_TYPE long long
#define ARCH_UINT64_TYPE unsigned long long
#define ARCH_INT64_PRINTF_FORMAT "ll"
#endif

#if powerc
#define CPU_TYPE_STRING "PPC"
#else
#define CPU_TYPE_STRING "68k"
#define THREADED_CODE
#endif
