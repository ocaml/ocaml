/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _config_
#define _config_


#if !macintosh
#include "../config/m.h"
#include "../config/s.h"
#else
#include <m.h>
#include <s.h>
#endif

/* Types for signed chars, 16-bit integers, 32-bit integers, 64-bit integers */

typedef signed char schar;

typedef short int16;            /* FIXME -- not true on the Cray T3E */
typedef unsigned short uint16;  /* FIXME -- not true on the Cray T3E */

#if SIZEOF_INT == 4
typedef int int32;
typedef unsigned int uint32;
#elif SIZEOF_LONG == 4
typedef long int32;
typedef unsigned long uint32;
#elif SIZEOF_SHORT == 4
typedef short int32;
typedef unsigned short uint32;
#endif

#if defined(ARCH_INT64_TYPE) && defined(ARCH_UINT64_TYPE)
typedef ARCH_INT64_TYPE int64;
typedef ARCH_UINT64_TYPE uint64;
#else
/* Int64.t will not be supported, and operations over it are not defined,
   but we must define the types int64 and uint64 as 64-bit placeholders. */
typedef struct { uint32 a, b; } uint64;
typedef uint64 int64;
#endif


/* Library dependencies */

/* We use threaded code interpretation if the compiler provides labels
   as first-class values (GCC 2.x).
   Macintosh 68k also uses threaded code, with the assembly-language
   bytecode interpreter (THREADED_CODE defined in config/sm-Mac.h).
*/

#if defined(__GNUC__) && __GNUC__ >= 2 && !defined(DEBUG) && !defined (SHRINKED_GNUC)
#define THREADED_CODE
#endif


/* Do not change this definition. */
#define Page_size (1 << Page_log)

/* Memory model parameters */

/* The size of a page for memory management (in bytes) is [1 << Page_log].
   It must be a multiple of [sizeof (long)]. */
#define Page_log 12             /* A page is 4 kilobytes. */

/* Initial size of stack (bytes). */
#define Stack_size (4096 * sizeof(value))

/* Minimum free size of stack (bytes); below that, it is reallocated. */
#define Stack_threshold (256 * sizeof(value))

/* Default maximum size of the stack (words). */
#define Max_stack_def (256 * 1024)


/* Maximum size of a block allocated in the young generation (words). */
/* Must be > 4 */
#define Max_young_wosize 256


/* Minimum size of the minor zone (words).
   This must be at least [Max_young_wosize + 1]. */
#define Minor_heap_min 4096

/* Maximum size of the minor zone (words).
   Must be greater than or equal to [Minor_heap_min].
*/
#define Minor_heap_max (1 << 28)

/* Default size of the minor zone. (words)  */
#define Minor_heap_def 32768


/* Minimum size increment when growing the heap (words).
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_min (2 * Page_size / sizeof (value))

/* Maximum size of a contiguous piece of the heap (words).
   Must be greater than or equal to [Heap_chunk_min].
   Must be greater than or equal to [Bhsize_wosize (Max_wosize)]. */
#define Heap_chunk_max (Bhsize_wosize (Max_wosize))

/* Default size increment when growing the heap. (words)
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_def (62 * 1024)

/* Default initial size of the major heap (words);
   same constraints as for Heap_chunk_def. */
#define Init_heap_def (62 * 1024)


/* Default speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   total size of live objects. */
#define Percent_free_def 42

/* Default setting for the compacter: 300%
   (i.e. trigger the compacter when 3/4 of the heap is free) */
#define Max_percent_free_def 300


#endif /* _config_ */
