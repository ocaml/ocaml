/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_


#include "config.h"

/* Standard definitions */

#include <stddef.h>
#include <stdlib.h>

/* Basic types and constants */

typedef size_t asize_t;

#ifndef NULL
#define NULL 0
#endif

typedef char * addr;

#ifdef __GNUC__
/* Works only in GCC 2.5 and later */
#define Noreturn __attribute ((noreturn))
#else
#define Noreturn
#endif

/* Assertions */

#ifdef DEBUG
#define CAMLassert(x) if (!(x)) caml_failed_assert ( #x , __FILE__, __LINE__)
void caml_failed_assert (char *, char *, int) Noreturn;
#else
#define CAMLassert(x)
#endif

void fatal_error (char *) Noreturn;
void fatal_error_arg (char *, char *) Noreturn;

/* GC flags and messages */

extern unsigned long verb_gc;
void gc_message (int, char *, unsigned long);

/* Memory routines */

char *aligned_malloc (asize_t, int, void **);

#ifdef DEBUG
#ifdef ARCH_SIXTYFOUR
#define Debug_tag(x) (0xD700D7D7D700D6D7ul \
                      | ((unsigned long) (x) << 16) \
                      | ((unsigned long) (x) << 48))
#else
#define Debug_tag(x) (0xD700D6D7ul | ((unsigned long) (x) << 16))
#endif

/*
  00 -> free words in minor heap
  01 -> fields of free list blocks in major heap
  03 -> heap chunks deallocated by heap shrinking
  04 -> fields deallocated by obj_truncate
  10 -> uninitialised fields of minor objects
  11 -> uninitialised fields of major objects
  12 -> uninitialised words of stat_alloc blocks
  15 -> uninitialised words of aligned_malloc blocks
  85 -> filler bytes of aligned_malloc
*/
#define Debug_free_minor     Debug_tag (0x00)
#define Debug_free_major     Debug_tag (0x01)
#define Debug_free_shrink    Debug_tag (0x03)
#define Debug_free_truncate  Debug_tag (0x04)
#define Debug_uninit_minor   Debug_tag (0x10)
#define Debug_uninit_major   Debug_tag (0x11)
#define Debug_uninit_stat    Debug_tag (0x12)
#define Debug_uninit_align   Debug_tag (0x15)
#define Debug_filler_align   Debug_tag (0x85)
#endif


#ifndef CAML_AVOID_CONFLICTS
#define Assert CAMLassert
#endif


#endif /* _misc_ */
