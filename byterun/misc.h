/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_


#include "config.h"

/* Standard definitions */

#ifdef __STDC__
#include <stddef.h>
#include <stdlib.h>
#endif

/* Basic types and constants */

#ifdef __STDC__
typedef size_t asize_t;
#else
typedef int asize_t;
#endif

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
#ifdef __STDC__
#define Assert(x) if (!(x)) failed_assert ( #x , __FILE__, __LINE__)
#else
#ifndef __LINE__
#define __LINE__ 0
#endif
#ifndef __FILE__
#define __FILE__ "(?)"
#endif
#define Assert(x) if (!(x)) failed_assert ("(?)" , __FILE__, __LINE__)
#endif
#else
#define Assert(x)
#endif

void failed_assert (char *, char *, int) Noreturn;
void fatal_error (char *) Noreturn;
void fatal_error_arg (char *, char *) Noreturn;

/* GC flags and messages */

extern int verb_gc;
void gc_message (char *, unsigned long);

/* Memory routines */

void memmov (char *, char *, unsigned long);
char *aligned_malloc (asize_t, int, void **);

#ifdef DEBUG
unsigned long not_random (void);
#endif


#endif /* _misc_ */
