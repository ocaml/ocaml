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

/* Miscellaneous macros and variables. */

#ifndef CAML_MISC_H
#define CAML_MISC_H

#include "config.h"

/* Standard definitions */

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>

#include "camlatomic.h"

/* Detection of available C attributes and compiler extensions */

#ifndef __has_c_attribute
#define __has_c_attribute(x) 0
#endif

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

/* Deprecation warnings */

#if __has_attribute(deprecated) || defined(__GNUC__)
  /* Supported since at least GCC 3.1 */
  #define CAMLdeprecated_typedef(name, type) \
    typedef type name __attribute__ ((deprecated))
#elif defined(_MSC_VER)
  #define CAMLdeprecated_typedef(name, type) \
    typedef __declspec(deprecated) type name
#else
  #define CAMLdeprecated_typedef(name, type) typedef type name
#endif

#if defined(__GNUC__) || defined(__llvm__) || defined(_MSC_VER)

#define CAML_STRINGIFY(x) #x
#ifdef _MSC_VER
#define CAML_MAKEWARNING1(x) CAML_STRINGIFY(message(x))
#else
#define CAML_MAKEWARNING1(x) CAML_STRINGIFY(GCC warning x)
#endif
#define CAML_MAKEWARNING2(y) CAML_MAKEWARNING1(#y)
#define CAML_PREPROWARNING(x) _Pragma(CAML_MAKEWARNING2(x))
#define CAML_DEPRECATED(name1,name2) \
  CAML_PREPROWARNING(name1 is deprecated: use name2 instead)

#else

#define CAML_PREPROWARNING(msg)
#define CAML_DEPRECATED(name1,name2)

#endif

/* Basic types and constants */

typedef size_t asize_t;

#ifndef NULL
#define NULL 0
#endif

#ifdef CAML_INTERNALS
CAMLdeprecated_typedef(addr, char *);
#endif /* CAML_INTERNALS */

/* The CAMLnoret macro must be added as a modifier at the beginning of
   the function definition or declaration.  It must occur first,
   before "static", "extern", "CAMLexport", "CAMLextern".

   Noreturn, CAMLnoreturn_start and CAMLnoreturn_end are preserved for
   compatibility reasons.

   Note: CAMLnoreturn is a different macro defined in memory.h,
   to be used in function bodies rather than as a function attribute.
*/
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L || \
    defined(__cplusplus)
  #define CAMLnoret [[noreturn]]
#else
  #define CAMLnoret _Noreturn
#endif

#define CAMLnoreturn_start CAMLnoret
#define CAMLnoreturn_end
#ifdef __GNUC__
  #define Noreturn __attribute__ ((noreturn))
#else
  #define Noreturn
#endif

/* Manually preventing inlining */
#if defined(__GNUC__)
  #define Caml_noinline __attribute__ ((noinline))
#elif defined(_MSC_VER)
  #define Caml_noinline __declspec(noinline)
#else
  #define Caml_noinline
#endif

/* Export control (to mark primitives and to handle Windows DLL) */

#ifndef CAMLDLLIMPORT
  #if defined(SUPPORT_DYNAMIC_LINKING) && defined(ARCH_SIXTYFOUR) \
      && (defined(__CYGWIN__) || defined(__MINGW32__))
    #define CAMLDLLIMPORT __declspec(dllimport)
  #else
    #define CAMLDLLIMPORT
  #endif
#endif

#define CAMLexport
#define CAMLprim
#define CAMLextern CAMLDLLIMPORT extern

/* Weak function definitions that can be overridden by external libs */
/* Conservatively restricted to ELF and MacOSX platforms */
#if defined(__GNUC__) && (defined (__ELF__) || defined(__APPLE__))
#define CAMLweakdef __attribute__((weak))
#else
#define CAMLweakdef
#endif

/* Alignment is necessary for domain_state.h, since the code generated */
/* by ocamlopt makes direct references into the domain state structure,*/
/* which is stored in a register on many platforms. For this to work, */
/* we need to be able to compute the exact offset of each member. */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L || \
    defined(__cplusplus)
#define CAMLalign(n) alignas(n)
#else
#define CAMLalign(n) _Alignas(n)
#endif

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L || \
    defined(__cplusplus)
#define CAMLthread_local thread_local
#else
#define CAMLthread_local _Thread_local
#endif

/* Prefetching */

#ifdef CAML_INTERNALS
#if (__has_builtin(__builtin_prefetch) || defined(__GNUC__)) && \
    (defined(__i386__) || defined(__x86_64__) || \
     defined(_M_IX86) || defined(_M_AMD64))
#define caml_prefetch(p) __builtin_prefetch((p), 1, 3)
/* 1 = intent to write; 3 = all cache levels */
#elif defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_AMD64))
#include <intrin.h>
#define caml_prefetch(p) _mm_prefetch((char const *) p, _MM_HINT_T0)
/* PreFetchCacheLine(PF_TEMPORAL_LEVEL_1, p) */
#else
#define caml_prefetch(p)
#endif
#endif /* CAML_INTERNALS */

/* CAMLunused is preserved for compatibility reasons.
   Instead of the legacy GCC/Clang-only
     CAMLunused foo;
   you should prefer
     CAMLunused_start foo CAMLunused_end;
   which supports both GCC/Clang and MSVC.
*/
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L || \
    defined(__cplusplus) && __cplusplus >= 201703L
  #define CAMLunused [[maybe_unused]]
  #define CAMLunused_start CAMLunused
  #define CAMLunused_end
#elif __has_attribute(unused) || defined(__GNUC__)
  #define CAMLunused __attribute__ ((unused))
  #define CAMLunused_start CAMLunused
  #define CAMLunused_end
#elif defined(_MSC_VER)
  #define CAMLunused_start __pragma( warning (push) )   \
          __pragma( warning (disable:4189 ) )
  #define CAMLunused_end __pragma( warning (pop))
  #define CAMLunused
#else
  #define CAMLunused_start
  #define CAMLunused_end
  #define CAMLunused
#endif

#ifdef CAML_INTERNALS
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L || \
    defined(__cplusplus) && __cplusplus >= 201703L
  #define fallthrough [[fallthrough]]
#elif __has_attribute(fallthrough)
  #define fallthrough __attribute__ ((fallthrough))
#else
  #define fallthrough ((void) 0)
#endif
#endif /* CAML_INTERNALS */

/* Function attributes to give hints to the C compiler about optimizations and
   static analysis. Intended for functions allocating and deallocating memory,
   or opening and closing resources.
   https://clang.llvm.org/docs/AttributeReference.html#function-attributes
   https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gcc/Common-Function-Attributes.html
*/
#if __has_attribute(malloc) && __has_attribute(warn_unused_result) &&   \
    __has_attribute(alloc_size) && __has_attribute(alloc_align) &&      \
    __has_attribute(returns_nonnull) &&                                 \
    ((defined(__GNUC__) && __GNUC__ >= 14) || /* false-positives in GCC<14 */ \
     defined(__llvm__))

/* Indicates that a particular function always returns a non-null pointer. */
#define CAMLreturns_nonnull() __attribute__ ((returns_nonnull))

/* [CAMLrealloc(n)] indicates that the function is [realloc]-like, and implies
   that the [n]-th argument number equals the number of available bytes at the
   returned pointer. */
#define CAMLrealloc(alloc_size_N,...)                           \
  __attribute__ ((warn_unused_result,alloc_size(alloc_size_N)))

/* [CAMLalloc(dealloc, p)] indicates that the function allocates a resource,
   which must be deallocated by passing it as the [p]-th argument of the
   function [dealloc]. */
#if defined(__GNUC__) && !defined(__llvm__)
#define CAMLalloc(deallocator,ptr_index,...)                            \
  __attribute__ ((malloc,malloc(deallocator,ptr_index),warn_unused_result))
#else
#define CAMLalloc(deallocator,ptr_index,...)    \
  __attribute__ ((malloc,warn_unused_result))
#endif

/* [CAMLmalloc(dealloc, p, n)] indicates that the function is [malloc]-like, and
   implies that it allocates a memory block whose size is set by the function's
   [n]-th argument, and which must be deallocated by passing it as the [p]-th
   argument of the function [dealloc]. */
#define CAMLmalloc(deallocator,ptr_index,alloc_size_N,...)      \
  CAMLalloc(deallocator,ptr_index)                              \
    __attribute__ ((alloc_size(alloc_size_N)))

/* [CAMLcalloc(dealloc, p, n, m)] indicates that the function is [calloc]-like,
   and implies that it allocates a memory block whose size is set by the product
   of the function's [n]-th and [m]-th arguments, and which must be deallocated
   by passing it as the [p]-th argument of the function [dealloc]. */
#define CAMLcalloc(deallocator,ptr_index,alloc_size_N,alloc_size_M,...) \
  CAMLalloc(deallocator,ptr_index)                                      \
    __attribute__ ((alloc_size(alloc_size_N,alloc_size_M)))

/* [CAMLaligned_alloc(dealloc, p, n, a)] indicates that the function is
   [aligned_alloc]-like, and implies that it allocates a memory block whose size
   is set by the function's [n]-th argument, aligned on a boundary given by the
   function's [a]-th argument, and which must be deallocated by passing it as
   the [p]-th argument of the function [dealloc]. */
#define CAMLaligned_alloc(deallocator,ptr_index,alloc_size_N,alloc_align_,...) \
  CAMLmalloc(deallocator,ptr_index,alloc_size_N)                        \
    __attribute__ ((alloc_align(alloc_align_)))

#else
#define CAMLreturns_nonnull()
#define CAMLrealloc(...)
#define CAMLalloc(...)
#define CAMLmalloc(...)
#define CAMLcalloc(...)
#define CAMLaligned_alloc(...)
#endif

/* GC timing hooks. These can be assigned by the user. These hooks
   must not allocate, change any heap value, nor call OCaml code. They
   can obtain the domain id with Caml_state->id. These functions must
   be reentrant. */
#ifndef __cplusplus
typedef void (*caml_timing_hook) (void);
extern _Atomic caml_timing_hook caml_major_slice_begin_hook;
extern _Atomic caml_timing_hook caml_major_slice_end_hook;
extern _Atomic caml_timing_hook caml_minor_gc_begin_hook;
extern _Atomic caml_timing_hook caml_minor_gc_end_hook;
extern _Atomic caml_timing_hook caml_finalise_begin_hook;
extern _Atomic caml_timing_hook caml_finalise_end_hook;
extern _Atomic caml_timing_hook caml_domain_terminated_hook;
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef CAML_INTERNALS

#ifndef __cplusplus
Caml_inline void call_timing_hook(_Atomic caml_timing_hook * a)
{
  caml_timing_hook h = atomic_load_explicit(a, memory_order_relaxed);
  if (h != NULL) (*h)();
}
#endif

#endif /* CAML_INTERNALS */

/* Windows Unicode support (rest below - char_os is needed earlier) */

#ifdef _WIN32
typedef wchar_t char_os;
#else
typedef char char_os;
#endif

/* Assertions */

#ifdef DEBUG

#ifdef UNICODE
/* See https://msdn.microsoft.com/ja-jp/library/b0084kay(v=vs.71).aspx
   It's not clear why this isn't so obviously documented, as it doesn't
   seem to have been superseded by a more sensible mechanism! */
#define CAML_WIDEN_STRING_LITERAL2(x) L##x
#define CAML_WIDEN_STRING_LITERAL(x) CAML_WIDEN_STRING_LITERAL2(x)
#define __OSFILE__ CAML_WIDEN_STRING_LITERAL(__FILE__)
#else
#define __OSFILE__ __FILE__
#endif

/* Although caml_failed_assert never returns, it is not marked as such.
   This prevents the C compiler optimising away all of the useful context
   from the callsite, making debuggers able to see it. */
#define CAMLassert(x) \
  (CAMLlikely(x) ? (void) 0 : caml_failed_assert ( #x , __OSFILE__, __LINE__))
CAMLextern void caml_failed_assert (const char *, const char_os *, int)
#if defined(__has_feature)
  /* However, we do inform clang-analyzer that this function never returns,
     since that improves analysis without breaking debugging */
  #if __has_feature(attribute_analyzer_noreturn)
    __attribute__((analyzer_noreturn))
  #endif
#endif
;
#else
#define CAMLassert(x) ((void) 0)
#endif

#if __has_builtin(__builtin_trap) || defined(__GNUC__)
  CAMLnoret Caml_inline void caml_abort(void) {
    __builtin_trap();
  }
#elif defined(_MSC_VER)
  #include <intrin.h>
  CAMLnoret Caml_inline void caml_abort(void) {
    __fastfail(7 /* FAST_FAIL_FATAL_APP_EXIT */);
  }
#else
  CAMLnoret Caml_inline void caml_abort(void) {
    abort();
  }
#endif

#ifdef DEBUG
CAMLnoret CAMLextern void caml_debug_abort(const char_os *, int);
#define CAMLunreachable() (caml_debug_abort(__OSFILE__, __LINE__))
#else
#define CAMLunreachable() (caml_abort())
#endif

#if __has_builtin(__builtin_expect) || defined(__GNUC__)
#define CAMLlikely(e)   __builtin_expect(!!(e), 1)
#define CAMLunlikely(e) __builtin_expect(!!(e), 0)
#else
#define CAMLlikely(e) (e)
#define CAMLunlikely(e) (e)
#endif

#ifdef CAML_INTERNALS

/* GC status assertions.

   CAMLnoalloc at the start of a block means that the GC must not be
   invoked during the block. */
#if (__has_attribute(cleanup) && __has_attribute(unused) || defined(__GNUC__)) \
    && defined(DEBUG)
int caml_noalloc_begin(void);
void caml_noalloc_end(int*);
void caml_alloc_point_here(void);
#define CAMLnoalloc                          \
  int caml__noalloc                          \
  __attribute__((cleanup(caml_noalloc_end),unused)) \
    = caml_noalloc_begin()
#define CAMLalloc_point_here (caml_alloc_point_here())
#else
#define CAMLnoalloc
#define CAMLalloc_point_here ((void)0)
#endif

#define Is_power_of_2(x) ((x) > 0 && ((x) & ((x) - 1)) == 0)

#endif

/* This hook is called when a fatal error occurs in the OCaml
   runtime. It is given arguments to be passed to the [vprintf]-like
   functions in order to synthesize the error message.
   If it returns, the runtime calls [abort()].

   If it is [NULL], the error message is printed on stderr and then
   [abort()] is called.

   This function must be reentrant. */
#ifndef __cplusplus
typedef void (*fatal_error_hook) (const char *msg, va_list args);
extern _Atomic fatal_error_hook caml_fatal_error_hook;
#endif

CAMLnoret CAMLextern void caml_fatal_error (const char *, ...)
#if __has_attribute(format) || defined(__GNUC__)
  __attribute__ ((format (printf, 1, 2)))
#endif
;

/* Integer arithmetic with overflow detection.
   The functions return 0 if no overflow, 1 if overflow.
   The result of the operation is always stored at [*res].
   If no overflow is reported, this is the exact result.
   If overflow is reported, this is the exact result modulo 2 to the word size.
*/

Caml_inline int caml_uadd_overflow(uintnat a, uintnat b, uintnat * res)
{
#if __has_builtin(__builtin_add_overflow) || defined(__GNUC__) && __GNUC__ >= 5
  return __builtin_add_overflow(a, b, res);
#else
  uintnat c = a + b;
  *res = c;
  return c < a;
#endif
}

Caml_inline int caml_usub_overflow(uintnat a, uintnat b, uintnat * res)
{
#if __has_builtin(__builtin_sub_overflow) || defined(__GNUC__) && __GNUC__ >= 5
  return __builtin_sub_overflow(a, b, res);
#else
  uintnat c = a - b;
  *res = c;
  return a < b;
#endif
}

#if __has_builtin(__builtin_mul_overflow) || defined(__GNUC__) && __GNUC__ >= 5
Caml_inline int caml_umul_overflow(uintnat a, uintnat b, uintnat * res)
{
  return __builtin_mul_overflow(a, b, res);
}
#else
extern int caml_umul_overflow(uintnat a, uintnat b, uintnat * res);
#endif

#ifdef CAML_INTERNALS

/* Rounding */

Caml_inline uintnat caml_round_up(uintnat value, uintnat align) {
  CAMLassert(Is_power_of_2(align));
  return (value + align - 1) & ~(align - 1);
}

#endif

/* From floats.c */
extern double caml_log1p(double);

/* Windows Unicode support */

#ifdef _WIN32

#ifdef CAML_INTERNALS
#define T(x) L ## x

#define main_os wmain
#endif

#define access_os _waccess
#define open_os _wopen
#define stat_os _wstati64
#define unlink_os _wunlink
#define rename_os caml_win32_rename
#define chdir_os _wchdir
#define mkdir_os(path, perm) ((void) (perm), _wmkdir(path))
#define getcwd_os _wgetcwd
#define system_os _wsystem
#define rmdir_os _wrmdir
#define putenv_os _wputenv
#define chmod_os _wchmod
#define execv_os _wexecv
#define execve_os _wexecve
#define execvp_os _wexecvp
#define execvpe_os _wexecvpe
#define strcmp_os wcscmp
#define strlen_os wcslen
#define sscanf_os swscanf
#define strcpy_os wcscpy
#define mktemp_os _wmktemp
#define fopen_os _wfopen

#define clock_os caml_win32_clock

#define caml_stat_strdup_os caml_stat_wcsdup
#define caml_stat_strdup_noexc_os caml_stat_wcsdup_noexc
#define caml_stat_strconcat_os caml_stat_wcsconcat

#define caml_stat_strdup_to_os caml_stat_strdup_to_utf16
#define caml_stat_strdup_noexc_to_os caml_stat_strdup_noexc_to_utf16
#define caml_stat_strdup_of_os caml_stat_strdup_of_utf16
#define caml_stat_strdup_noexc_of_os caml_stat_strdup_noexc_of_utf16
#define caml_stat_char_array_to_os caml_stat_char_array_to_utf16
#define caml_stat_char_array_of_os caml_stat_char_array_of_utf16
#define caml_copy_string_of_os caml_copy_string_of_utf16

#else /* _WIN32 */

#ifdef CAML_INTERNALS
#define T(x) x

#define main_os main
#endif

#define access_os access
#define open_os open
#define stat_os stat
#define unlink_os unlink
#define rename_os rename
#define chdir_os chdir
#define mkdir_os mkdir
#define getcwd_os getcwd
#define system_os system
#define rmdir_os rmdir
#define putenv_os putenv
#define chmod_os chmod
#define execv_os execv
#define execve_os execve
#define execvp_os execvp
#define execvpe_os execvpe
#define strcmp_os strcmp
#define strlen_os strlen
#define sscanf_os sscanf
#define strcpy_os strcpy
#define mktemp_os mktemp
#define fopen_os fopen

#define clock_os clock

#define caml_stat_strdup_os caml_stat_strdup
#define caml_stat_strdup_noexc_os caml_stat_strdup_noexc
#define caml_stat_strconcat_os caml_stat_strconcat

#define caml_stat_strdup_to_os caml_stat_strdup
#define caml_stat_strdup_noexc_to_os caml_stat_strdup_noexc
#define caml_stat_strdup_of_os caml_stat_strdup
#define caml_stat_strdup_noexc_of_os caml_stat_strdup_noexc
#define caml_stat_char_array_to_os caml_stat_memdup
#define caml_stat_char_array_of_os caml_stat_memdup
#define caml_copy_string_of_os caml_copy_string

#endif /* _WIN32 */

/* Wrapper for Windows unlink */
#ifdef _WIN32
#define caml_unlink caml_win32_unlink
#else
#define caml_unlink unlink_os
#endif


/* Data structures */

struct ext_table {
  int size;
  int capacity;
  void ** contents;
};

extern void caml_ext_table_init(struct ext_table * tbl, int init_capa);
extern int caml_ext_table_add(struct ext_table * tbl, void * data);
extern int caml_ext_table_add_noexc(struct ext_table * tbl, void * data);
extern void caml_ext_table_remove(struct ext_table * tbl, void * data);
extern void caml_ext_table_free(struct ext_table * tbl, int free_entries);
extern void caml_ext_table_clear(struct ext_table * tbl, int free_entries);

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */
CAMLextern int caml_read_directory(char_os * dirname,
                                   struct ext_table * contents);

/* Deprecated aliases */
#define caml_aligned_malloc \
   CAML_DEPRECATED("caml_aligned_malloc", "caml_stat_alloc_aligned_noexc") \
   caml_stat_alloc_aligned_noexc
#define caml_strdup \
   CAML_DEPRECATED("caml_strdup", "caml_stat_strdup") \
   caml_stat_strdup
#define caml_strconcat \
   CAML_DEPRECATED("caml_strconcat", "caml_stat_strconcat") \
   caml_stat_strconcat

#ifdef CAML_INTERNALS

/* runtime message flags. Settable with v= in OCAMLRUNPARAM */

extern atomic_uintnat caml_verb_gc;

/* Bits which may be set in caml_verb_gc. The quotations are from the
 * OCaml manual. */

/* "Start and end of major GC cycle" (unused) */
#define CAML_GC_MSG_MAJOR           0x0001
/* "Minor collection and major GC slice" (unused) */
#define CAML_GC_MSG_MINOR           0x0002
/* "Growing and shrinking of the heap" */
#define CAML_GC_MSG_HEAPSIZE        0x0004
/* "Resizing of stacks and memory manager tables" */
#define CAML_GC_MSG_STACKSIZE       0x0008
/* "Heap compaction" (unused) */
#define CAML_GC_MSG_COMPACT         0x0010
/* "Change of GC parameters" */
#define CAML_GC_MSG_PARAMS          0x0020
/* "Computation of major GC slice size" */
#define CAML_GC_MSG_SLICESIZE       0x0040
/* "Calling of finalization functions" */
#define CAML_GC_MSG_FINALIZE        0x0080
/* "Startup messages" */
#define CAML_GC_MSG_STARTUP         0x0100
/* "Computation of compaction-triggering condition" (unused) */
#define CAML_GC_MSG_COMPACT_TRIGGER 0x0200
/* "Output GC statistics at program exit" */
#define CAML_GC_MSG_STATS           0x0400
/* "GC debugging messages */
#define CAML_GC_MSG_DEBUG           0x0800
/* "Address space reservation changes" */
#define CAML_GC_MSG_ADDRSPACE       0x1000

/* Default set of messages when runtime invoked with -v */

#define CAML_GC_MSG_VERBOSE (CAML_GC_MSG_MAJOR     | \
                             CAML_GC_MSG_HEAPSIZE  | \
                             CAML_GC_MSG_STACKSIZE | \
                             CAML_GC_MSG_COMPACT   | \
                             CAML_GC_MSG_PARAMS)

/* Use to control messages which should be output at any non-zero verbosity */

#define CAML_GC_MSG_ANY (-1)

/* output message if caml_verb_gc includes any bits in `category`. */

void caml_gc_message (int category, const char *, ...)
#if __has_attribute(format) || defined(__GNUC__)
  __attribute__ ((format (printf, 2, 3)))
#endif
;

/* Short-hand for calls to `caml_gc_message` */

#define CAML_GC_MESSAGE(category, ...) \
    caml_gc_message(CAML_GC_MSG_ ## category, __VA_ARGS__)

/* Output message if CAML_GC_MSG_DEBUG is set */

void caml_gc_log (const char *, ...)
#if __has_attribute(format) || defined(__GNUC__)
  __attribute__ ((format (printf, 1, 2)))
#endif
;

/* Runtime warnings */
extern uintnat caml_runtime_warnings;
int caml_runtime_warnings_active(void);

#ifdef DEBUG
#ifdef ARCH_SIXTYFOUR
#define Debug_tag(x) (0xD700D7D7D700D6D8ull \
                      | ((uintnat) (x) << 16) \
                      | ((uintnat) (x) << 48))
#define Is_debug_tag(x) (((x) & 0xff00ffffff00ffffull) == 0xD700D7D7D700D6D8ull)
#else
#define Debug_tag(x) (0xD700D6D8ul | ((uintnat) (x) << 16))
#define Is_debug_tag(x) (((x) & 0xff00fffful) == 0xD700D6D8ul)
#endif /* ARCH_SIXTYFOUR */

/*
  00 -> free words in minor heap
  01 -> fields of free list blocks in major heap
  03 -> heap chunks deallocated by heap shrinking
  04 -> fields deallocated by caml_obj_truncate: obsolete
  05 -> unused child pointers in large free blocks
  10 -> uninitialised fields of minor objects
  11 -> uninitialised fields of major objects
  15 -> uninitialised words of [caml_stat_alloc_aligned] blocks
  85 -> filler bytes of [caml_stat_alloc_aligned]
  99 -> the magic prefix of a memory block allocated by [caml_stat_alloc]

  special case (byte by byte):
  D7 -> uninitialised words of [caml_stat_alloc] blocks
*/
#define Debug_free_minor     Debug_tag (0x00)
#define Debug_free_major     Debug_tag (0x01)
#define Debug_free_shrink    Debug_tag (0x03)
#define Debug_free_truncate  Debug_tag (0x04) /* obsolete */
#define Debug_free_unused    Debug_tag (0x05)
#define Debug_uninit_minor   Debug_tag (0x10)
#define Debug_uninit_major   Debug_tag (0x11)
#define Debug_uninit_align   Debug_tag (0x15)
#define Debug_filler_align   Debug_tag (0x85)
#define Debug_pool_magic     Debug_tag (0x99)

#define Debug_uninit_stat    0xD7

#endif /* DEBUG */


/* snprintf emulation for Win32 */

#ifdef _WIN32
#ifndef _UCRT
extern int caml_snprintf(char * buf, size_t size, const char * format, ...);
#define snprintf caml_snprintf
#endif

CAMLextern int caml_snwprintf(wchar_t * buf,
                          size_t size,
                          const wchar_t * format, ...);
#define snprintf_os caml_snwprintf
#else
#define snprintf_os snprintf
#endif

/* Macro used to deactivate address sanitizer on some functions. */
#define CAMLno_asan
/* `__has_feature` is present in Clang and recent GCCs (14 and later). Older
   GCCs define `__SANITIZE_ADDRESS__`. In addition, starting from version 14
   GCC supports the Clang-originating syntax `no_sanitize("address")`.
   This should select the right attribute in all circumstances. */
#if defined(__has_feature)
#  if __has_feature(address_sanitizer)
#    undef CAMLno_asan
#    define CAMLno_asan __attribute__((no_sanitize("address")))
#  endif
#else
#  if defined(__SANITIZE_ADDRESS__)
#    undef CAMLno_asan
#    define CAMLno_asan __attribute__((no_sanitize_address))
#  endif
#endif

/* Generate a named symbol that is unique within the current macro expansion */
#define CAML_GENSYM_3(name, l) caml__##name##_##l
#define CAML_GENSYM_2(name, l) CAML_GENSYM_3(name, l)
#define CAML_GENSYM(name) CAML_GENSYM_2(name, __LINE__)

#endif /* CAML_INTERNALS */

/* The [backtrace_slot] type represents values stored in
 * [Caml_state->backtrace_buffer].  In bytecode, it is the same as a
 * [code_t], in native code it is either a [frame_descr *] or a [debuginfo],
 * depending on the second-lowest bit.  In any case, the lowest bit must
 * be 0.
 * The representation doesn't matter for code outside [backtrace_{byt,nat}.c],
 * so it is just exposed as a [void *].
 */
typedef void * backtrace_slot;

#ifdef __cplusplus
}
#endif

#endif /* CAML_MISC_H */
