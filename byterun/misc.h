/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
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
#define Assert(x) if (!(x)) failed_assert ( #x , __FILE__, __LINE__)
#else
#define Assert(x)
#endif

void failed_assert (char *, char *, int) Noreturn;
void fatal_error (char *) Noreturn;
void fatal_error_arg (char *, char *) Noreturn;

/* GC flags and messages */

extern int verb_gc;
void gc_message (int, char *, unsigned long);

/* Memory routines */

void memmov (char *, char *, unsigned long);
char *aligned_malloc (asize_t, int, void **);

#ifdef DEBUG
unsigned long not_random (void);
#endif


#endif /* _misc_ */
