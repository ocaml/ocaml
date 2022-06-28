/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_CONFIG_H
#define CAML_CONFIG_H

/* CAML_NAME_SPACE was introduced in OCaml 3.08 to declare compatibility with
   the newly caml_-prefixed names of C runtime functions and to disable the
   definition of compatibility macros for the un-prefixed names. The
   compatibility layer was removed in OCaml 5.0, so CAML_NAME_SPACE is the
   default. */
#ifndef CAML_NAME_SPACE
#define CAML_NAME_SPACE
#endif

#include "m.h"

/* If supported, tell gcc that we can use 32-bit code addresses for
 * threaded code, unless we are compiled for a shared library (-fPIC option) */
#ifdef HAS_ARCH_CODE32
#ifndef __PIC__
#  define ARCH_CODE32
#endif /* __PIC__ */
#endif /* HAS_ARCH_CODE32 */

#define INT64_LITERAL(s) s ## LL

#if defined(_MSC_VER) && !defined(__cplusplus)
#define Caml_inline static __inline
#else
#define Caml_inline static inline
#endif

#include "s.h"

#ifndef CAML_CONFIG_H_NO_TYPEDEFS

#include <stddef.h>

#if defined(HAS_LOCALE_H) || defined(HAS_XLOCALE_H)
#define HAS_LOCALE
#endif

#ifdef HAS_STDINT_H
#include <stdint.h>
#endif

/* Disable the mingw-w64 *printf shims */
#if defined(CAML_INTERNALS) && defined(__MINGW32__)
  /* Headers may have already included <_mingw.h>, so #undef if necessary. */
  #ifdef __USE_MINGW_ANSI_STDIO
    #undef __USE_MINGW_ANSI_STDIO
  #endif
  /* <stdio.h> must either be #include'd before this header or
     __USE_MINGW_ANSI_STDIO needs to be 0 when <stdio.h> is processed. The final
     effect will be the same - stdio.h will define snprintf and misc.h will make
     snprintf a macro (referring to caml_snprintf). */
  #define __USE_MINGW_ANSI_STDIO 0
#endif

#if defined(__MINGW32__) || (defined(_MSC_VER) && _MSC_VER < 1800)
#define ARCH_SIZET_PRINTF_FORMAT "I"
#else
#define ARCH_SIZET_PRINTF_FORMAT "z"
#endif

/* Types for 32-bit integers, 64-bit integers, and
   native integers (as wide as a pointer type) */

#ifndef ARCH_INT32_TYPE
#if SIZEOF_INT == 4
#define ARCH_INT32_TYPE int
#define ARCH_UINT32_TYPE unsigned int
#define ARCH_INT32_PRINTF_FORMAT ""
#elif SIZEOF_LONG == 4
#define ARCH_INT32_TYPE long
#define ARCH_UINT32_TYPE unsigned long
#define ARCH_INT32_PRINTF_FORMAT "l"
#elif SIZEOF_SHORT == 4
#define ARCH_INT32_TYPE short
#define ARCH_UINT32_TYPE unsigned short
#define ARCH_INT32_PRINTF_FORMAT ""
#else
#error "No 32-bit integer type available"
#endif
#endif

#if defined(__MINGW32__) && !__USE_MINGW_ANSI_STDIO
  #define ARCH_INT64_TYPE long long
  #define ARCH_UINT64_TYPE unsigned long long
  #define ARCH_INT64_PRINTF_FORMAT "I64"
#elif defined(_MSC_VER)
  #define ARCH_INT64_TYPE __int64
  #define ARCH_UINT64_TYPE unsigned __int64
  #define ARCH_INT64_PRINTF_FORMAT "I64"
#else
  #if SIZEOF_LONG == 8
    #define ARCH_INT64_TYPE long
    #define ARCH_UINT64_TYPE unsigned long
    #define ARCH_INT64_PRINTF_FORMAT "l"
  #elif SIZEOF_LONGLONG == 8
    #define ARCH_INT64_TYPE long long
    #define ARCH_UINT64_TYPE unsigned long long
    #define ARCH_INT64_PRINTF_FORMAT "ll"
  #else
    #error "No 64-bit integer type available"
  #endif
#endif

#ifndef HAS_STDINT_H
/* Not a C99 compiler, typically MSVC.  Define the C99 types we use. */
typedef ARCH_INT32_TYPE int32_t;
typedef ARCH_UINT32_TYPE uint32_t;
typedef ARCH_INT64_TYPE int64_t;
typedef ARCH_UINT64_TYPE uint64_t;
#if SIZEOF_SHORT == 2
typedef short int16_t;
typedef unsigned short uint16_t;
#else
#error "No 16-bit integer type available"
#endif
typedef unsigned char uint8_t;
#endif

#if SIZEOF_PTR == SIZEOF_LONG
/* Standard models: ILP32 or I32LP64 */
typedef long intnat;
typedef unsigned long uintnat;
#define ARCH_INTNAT_PRINTF_FORMAT "l"
#elif SIZEOF_PTR == SIZEOF_INT
/* Hypothetical IP32L64 model */
typedef int intnat;
typedef unsigned int uintnat;
#define ARCH_INTNAT_PRINTF_FORMAT ""
#elif SIZEOF_PTR == 8
/* Win64 model: IL32P64 */
typedef int64_t intnat;
typedef uint64_t uintnat;
#define ARCH_INTNAT_PRINTF_FORMAT ARCH_INT64_PRINTF_FORMAT
#else
#error "No integer type available to represent pointers"
#endif

#define UINTNAT_MAX ((uintnat)-1)

#endif /* CAML_CONFIG_H_NO_TYPEDEFS */

/* Endianness of floats */

/* ARCH_FLOAT_ENDIANNESS encodes the byte order of doubles as follows:
   the value [0xabcdefgh] means that the least significant byte of the
   float is at byte offset [a], the next lsb at [b], ..., and the
   most significant byte at [h]. */

#if defined(__arm__) && !defined(__ARM_EABI__)
#define ARCH_FLOAT_ENDIANNESS 0x45670123
#elif defined(ARCH_BIG_ENDIAN)
#define ARCH_FLOAT_ENDIANNESS 0x76543210
#else
#define ARCH_FLOAT_ENDIANNESS 0x01234567
#endif


/* We use threaded code interpretation if the compiler provides labels
   as first-class values (GCC 2.x). */

#if defined(__GNUC__) && __GNUC__ >= 2 && !defined(DEBUG) \
    && !defined (SHRINKED_GNUC)
#define THREADED_CODE
#endif


/* Memory model parameters */

/* The size of a page for memory management (in bytes) is [1 << Page_log].
   [Page_size] must be a multiple of [sizeof (value)].
   [Page_log] must be be >= 8 and <= 20.
   Do not change the definition of [Page_size]. */
#define Page_log 12             /* A page is 4 kilobytes. */
#define Page_size (1 << Page_log)

/* Initial size of stack (bytes). */
#ifdef DEBUG
#define Stack_init_bsize (64 * sizeof(value))
#else
#define Stack_init_bsize (4096 * sizeof(value))
#endif

/* Minimum free size of stack (bytes); below that, it is reallocated. */
#define Stack_threshold_words 32
#define Stack_threshold (Stack_threshold_words * sizeof(value))

/* Number of words used in the control structure at the start of a stack
   (see fiber.h) */
#ifdef ARCH_SIXTYFOUR
#define Stack_ctx_words (6 + 1)
#else
#define Stack_ctx_words (6 + 2)
#endif

/* Default maximum size of the stack (words). */
/* (1 Gib for 64-bit platforms, 512 Mib for 32-bit platforms) */
#define Max_stack_def (128 * 1024 * 1024)


/* Maximum size of a block allocated in the young generation (words). */
/* Must be > 4 */
#define Max_young_wosize 256
#define Max_young_whsize (Whsize_wosize (Max_young_wosize))


/* Minimum size of the minor zone (words).
   This must be at least [Max_young_wosize + 1]. */
#define Minor_heap_min (Max_young_wosize + 1)

/* Default size of the minor zone. (words)  */
#define Minor_heap_def 262144

/* Minimum size increment when growing the heap (words).
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_min (15 * Page_size)


/* Default speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   total size of live objects. */
#define Percent_free_def 120

/* Default setting for the major GC slice smoothing window: 1
   (i.e. no smoothing)
*/
#define Major_window_def 1

/* Maximum size of the major GC slice smoothing window. */
#define Max_major_window 50

/* Default setting for the ratio of custom garbage to major heap size.
   Documented in gc.mli */
#define Custom_major_ratio_def 44

/* Default setting for the ratio of custom garbage to minor heap size.
   Documented in gc.mli */
#define Custom_minor_ratio_def 100

/* Default setting for maximum size of custom objects counted as garbage
   in the minor heap.
   Documented in gc.mli */
#define Custom_minor_max_bsz_def 8192

/* Default allocation policy. */
#define Allocation_policy_def caml_policy_best_fit

/* Default size of runtime_events ringbuffers, in words, in powers of two */
#define Default_runtime_events_log_wsize 16

#endif /* CAML_CONFIG_H */
