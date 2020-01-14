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

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "config.h"

/* Standard definitions */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

/* Basic types and constants */

typedef size_t asize_t;

#ifndef NULL
#define NULL 0
#endif

#if defined(__GNUC__) || defined(__clang__)
  /* Supported since at least GCC 3.1 */
  #define CAMLdeprecated_typedef(name, type) \
    typedef type name __attribute ((deprecated))
#elif _MSC_VER >= 1310
  /* NB deprecated("message") only supported from _MSC_VER >= 1400 */
  #define CAMLdeprecated_typedef(name, type) \
    typedef __declspec(deprecated) type name
#else
  #define CAMLdeprecated_typedef(name, type) typedef type name
#endif

#ifdef CAML_INTERNALS
CAMLdeprecated_typedef(addr, char *);
#endif /* CAML_INTERNALS */

/* Noreturn is preserved for compatibility reasons.
   Instead of the legacy GCC/Clang-only
     foo Noreturn;
   you should prefer
     CAMLnoreturn_start foo CAMLnoreturn_end;
   which supports both GCC/Clang and MSVC.

   Note: CAMLnoreturn is a different macro defined in memory.h,
   to be used in function bodies rather than as a prototype attribute.
*/
#ifdef __GNUC__
  /* Works only in GCC 2.5 and later */
  #define CAMLnoreturn_start
  #define CAMLnoreturn_end __attribute__ ((noreturn))
  #define Noreturn __attribute__ ((noreturn))
#elif _MSC_VER >= 1500
  #define CAMLnoreturn_start __declspec(noreturn)
  #define CAMLnoreturn_end
  #define Noreturn
#else
  #define CAMLnoreturn_start
  #define CAMLnoreturn_end
  #define Noreturn
#endif



/* Export control (to mark primitives and to handle Windows DLL) */

#define CAMLexport
#define CAMLprim
#define CAMLextern extern

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
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
#define CAMLalign(n) _Alignas(n)
#elif defined(SUPPORTS_ALIGNED_ATTRIBUTE)
#define CAMLalign(n) __attribute__((aligned(n)))
#elif _MSC_VER >= 1500
#define CAMLalign(n) __declspec(align(n))
#else
#error "How do I align values on this platform?"
#endif

/* CAMLunused is preserved for compatibility reasons.
   Instead of the legacy GCC/Clang-only
     CAMLunused foo;
   you should prefer
     CAMLunused_start foo CAMLunused_end;
   which supports both GCC/Clang and MSVC.
*/
#if defined(__GNUC__) && (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ > 7))
  #define CAMLunused_start __attribute__ ((unused))
  #define CAMLunused_end
  #define CAMLunused __attribute__ ((unused))
#elif _MSC_VER >= 1500
  #define CAMLunused_start  __pragma( warning (push) )           \
    __pragma( warning (disable:4189 ) )
  #define CAMLunused_end __pragma( warning (pop))
  #define CAMLunused
#else
  #define CAMLunused_start
  #define CAMLunused_end
  #define CAMLunused
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* GC timing hooks. These can be assigned by the user. These hooks
   must not allocate, change any heap value, nor call OCaml code.
*/
typedef void (*caml_timing_hook) (void);
extern caml_timing_hook caml_major_slice_begin_hook, caml_major_slice_end_hook;
extern caml_timing_hook caml_minor_gc_begin_hook, caml_minor_gc_end_hook;
extern caml_timing_hook caml_finalise_begin_hook, caml_finalise_end_hook;

#define CAML_STATIC_ASSERT_3(b, l) \
  CAMLunused_start \
    CAMLextern char static_assertion_failure_line_##l[(b) ? 1 : -1] \
  CAMLunused_end

#define CAML_STATIC_ASSERT_2(b, l) CAML_STATIC_ASSERT_3(b, l)
#define CAML_STATIC_ASSERT(b) CAML_STATIC_ASSERT_2(b, __LINE__)

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

#define CAMLassert(x) \
  ((x) ? (void) 0 : caml_failed_assert ( #x , __OSFILE__, __LINE__))
CAMLnoreturn_start
CAMLextern void caml_failed_assert (char *, char_os *, int)
CAMLnoreturn_end;
#else
#define CAMLassert(x) ((void) 0)
#endif

/* This hook is called when a fatal error occurs in the OCaml
   runtime. It is given arguments to be passed to the [vprintf]-like
   functions in order to synthetize the error message.
   If it returns, the runtime calls [abort()].

   If it is [NULL], the error message is printed on stderr and then
   [abort()] is called. */
extern void (*caml_fatal_error_hook) (char *msg, va_list args);

CAMLnoreturn_start
CAMLextern void caml_fatal_error (char *, ...)
#ifdef __GNUC__
  __attribute__ ((format (printf, 1, 2)))
#endif
CAMLnoreturn_end;

/* Detection of available C built-in functions, the Clang way. */

#ifdef __has_builtin
#define Caml_has_builtin(x) __has_builtin(x)
#else
#define Caml_has_builtin(x) 0
#endif

/* Integer arithmetic with overflow detection.
   The functions return 0 if no overflow, 1 if overflow.
   The result of the operation is always stored at [*res].
   If no overflow is reported, this is the exact result.
   If overflow is reported, this is the exact result modulo 2 to the word size.
*/

static inline int caml_uadd_overflow(uintnat a, uintnat b, uintnat * res)
{
#if __GNUC__ >= 5 || Caml_has_builtin(__builtin_add_overflow)
  return __builtin_add_overflow(a, b, res);
#else
  uintnat c = a + b;
  *res = c;
  return c < a;
#endif
}

static inline int caml_usub_overflow(uintnat a, uintnat b, uintnat * res)
{
#if __GNUC__ >= 5 || Caml_has_builtin(__builtin_sub_overflow)
  return __builtin_sub_overflow(a, b, res);
#else
  uintnat c = a - b;
  *res = c;
  return a < b;
#endif
}

#if __GNUC__ >= 5 || Caml_has_builtin(__builtin_mul_overflow)
static inline int caml_umul_overflow(uintnat a, uintnat b, uintnat * res)
{
  return __builtin_mul_overflow(a, b, res);
}
#else
extern int caml_umul_overflow(uintnat a, uintnat b, uintnat * res);
#endif

/* From floats.c */
extern double caml_log1p(double);

/* Windows Unicode support */

#ifdef _WIN32

#ifdef CAML_INTERNALS
#define T(x) L ## x
#endif

#define access_os _waccess
#define open_os _wopen
#define stat_os _wstati64
#define unlink_os _wunlink
#define rename_os caml_win32_rename
#define chdir_os _wchdir
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

#define caml_stat_strdup_os caml_stat_wcsdup
#define caml_stat_strconcat_os caml_stat_wcsconcat

#define caml_stat_strdup_to_os caml_stat_strdup_to_utf16
#define caml_stat_strdup_of_os caml_stat_strdup_of_utf16
#define caml_copy_string_of_os caml_copy_string_of_utf16

#else /* _WIN32 */

#ifdef CAML_INTERNALS
#define T(x) x
#endif

#define access_os access
#define open_os open
#define stat_os stat
#define unlink_os unlink
#define rename_os rename
#define chdir_os chdir
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

#define caml_stat_strdup_os caml_stat_strdup
#define caml_stat_strconcat_os caml_stat_strconcat

#define caml_stat_strdup_to_os caml_stat_strdup
#define caml_stat_strdup_of_os caml_stat_strdup
#define caml_copy_string_of_os caml_copy_string

#endif /* _WIN32 */


/* Data structures */

struct ext_table {
  int size;
  int capacity;
  void ** contents;
};

extern void caml_ext_table_init(struct ext_table * tbl, int init_capa);
extern int caml_ext_table_add(struct ext_table * tbl, void * data);
extern void caml_ext_table_remove(struct ext_table * tbl, void * data);
extern void caml_ext_table_free(struct ext_table * tbl, int free_entries);
extern void caml_ext_table_clear(struct ext_table * tbl, int free_entries);

CAMLextern int caml_read_directory(char_os * dirname,
                                   struct ext_table * contents);

/* Deprecated aliases */
#define caml_aligned_malloc caml_stat_alloc_aligned_noexc
#define caml_strdup caml_stat_strdup
#define caml_strconcat caml_stat_strconcat

#ifdef CAML_INTERNALS

/* GC flags and messages */

extern uintnat caml_verb_gc;
void caml_gc_message (int, char *, ...)
#ifdef __GNUC__
  __attribute__ ((format (printf, 2, 3)))
#endif
;

/* Runtime warnings */
extern uintnat caml_runtime_warnings;
int caml_runtime_warnings_active(void);

#ifdef DEBUG
#ifdef ARCH_SIXTYFOUR
#define Debug_tag(x) (INT64_LITERAL(0xD700D7D7D700D6D7u) \
                      | ((uintnat) (x) << 16) \
                      | ((uintnat) (x) << 48))
#else
#define Debug_tag(x) (0xD700D6D7ul | ((uintnat) (x) << 16))
#endif /* ARCH_SIXTYFOUR */

/*
  00 -> free words in minor heap
  01 -> fields of free list blocks in major heap
  03 -> heap chunks deallocated by heap shrinking
  04 -> fields deallocated by [caml_obj_truncate]
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
#define Debug_free_truncate  Debug_tag (0x04)
#define Debug_free_unused    Debug_tag (0x05)
#define Debug_uninit_minor   Debug_tag (0x10)
#define Debug_uninit_major   Debug_tag (0x11)
#define Debug_uninit_align   Debug_tag (0x15)
#define Debug_filler_align   Debug_tag (0x85)
#define Debug_pool_magic     Debug_tag (0x99)

#define Debug_uninit_stat    0xD7

/* Note: the first argument is in fact a [value] but we don't have this
   type available yet because we can't include [mlvalues.h] in this file.
*/
extern void caml_set_fields (intnat v, uintnat, uintnat);
#endif /* DEBUG */


/* snprintf emulation for Win32 */

#if defined(_WIN32) && !defined(_UCRT)
extern int caml_snprintf(char * buf, size_t size, const char * format, ...);
#define snprintf caml_snprintf
#endif

#ifdef CAML_INSTR
/* Timers and counters for GC latency profiling (Linux-only) */

#include <time.h>
#include <stdio.h>

extern intnat caml_instr_starttime, caml_instr_stoptime;

struct caml_instr_block {
  struct timespec ts[10];
  char *tag[10];
  int index;
  struct caml_instr_block *next;
};

extern struct caml_instr_block *caml_instr_log;

/* Declare a timer/counter name. [t] must be a new variable name. */
#define CAML_INSTR_DECLARE(t)                                       \
  struct caml_instr_block *t = NULL

/* Allocate the data block for a given name.
   [t] must have been declared with [CAML_INSTR_DECLARE]. */
#define CAML_INSTR_ALLOC(t) do{                                             \
    if (Caml_state_field(stat_minor_collections) >= caml_instr_starttime    \
        && Caml_state_field(stat_minor_collections) < caml_instr_stoptime){ \
      t = caml_stat_alloc_noexc (sizeof (struct caml_instr_block));         \
      t->index = 0;                                                         \
      t->tag[0] = "";                                                       \
      t->next = caml_instr_log;                                             \
      caml_instr_log = t;                                                   \
    }                                                                       \
  }while(0)

/* Allocate the data block and start the timer.
   [t] must have been declared with [CAML_INSTR_DECLARE]
   and allocated with [CAML_INSTR_ALLOC]. */
#define CAML_INSTR_START(t, msg) do{                                \
    if (t != NULL){                                                 \
      t->tag[0] = msg;                                              \
      clock_gettime (CLOCK_REALTIME, &(t->ts[0]));                  \
    }                                                               \
  }while(0)

/* Declare a timer, allocate its data, and start it.
   [t] must be a new variable name. */
#define CAML_INSTR_SETUP(t, msg)                                    \
  CAML_INSTR_DECLARE (t);                                           \
  CAML_INSTR_ALLOC (t);                                             \
  CAML_INSTR_START (t, msg)

/* Record an intermediate time within a given timer.
   [t] must have been declared, allocated, and started. */
#define CAML_INSTR_TIME(t, msg) do{                                 \
    if (t != NULL){                                                 \
      ++ t->index;                                                  \
      t->tag[t->index] = (msg);                                     \
      clock_gettime (CLOCK_REALTIME, &(t->ts[t->index]));           \
    }                                                               \
  }while(0)

/* Record an integer data point.
   If [msg] ends with # it will be interpreted as an integer-valued event.
   If it ends with @ it will be interpreted as an event counter.
*/
#define CAML_INSTR_INT(msg, data) do{                               \
    CAML_INSTR_SETUP (__caml_tmp, "");                              \
    if (__caml_tmp != NULL){                                        \
      __caml_tmp->index = 1;                                        \
      __caml_tmp->tag[1] = msg;                                     \
      __caml_tmp->ts[1].tv_sec = 0;                                 \
      __caml_tmp->ts[1].tv_nsec = (data);                           \
    }                                                               \
  }while(0)

/* This function is called at the start of the program to set up
   the data for the above macros.
*/
extern void caml_instr_init (void);

/* This function is automatically called by the runtime to output
   the collected data to the dump file. */
extern void caml_instr_atexit (void);

#else /* CAML_INSTR */

#define CAML_INSTR_DECLARE(t) /**/
#define CAML_INSTR_ALLOC(t) /**/
#define CAML_INSTR_START(t, name) /**/
#define CAML_INSTR_SETUP(t, name) /**/
#define CAML_INSTR_TIME(t, msg) /**/
#define CAML_INSTR_INT(msg, c) /**/
#define caml_instr_init() /**/
#define caml_instr_atexit() /**/

#endif /* CAML_INSTR */

/* Macro used to deactivate thread and address sanitizers on some
   functions. */
#define CAMLno_tsan
#define CAMLno_asan
#if defined(__has_feature)
#  if __has_feature(thread_sanitizer)
#    undef CAMLno_tsan
#    define CAMLno_tsan __attribute__((no_sanitize("thread")))
#  endif
#  if __has_feature(address_sanitizer)
#    undef CAMLno_asan
#    define CAMLno_asan __attribute__((no_sanitize("address")))
#  endif
#endif

/* A table of all code fragments (main program and dynlinked modules) */
struct code_fragment {
  char *code_start;
  char *code_end;
  unsigned char digest[16];
  char digest_computed;
};

extern struct ext_table caml_code_fragments_table;

int caml_find_code_fragment(char *pc, int *index, struct code_fragment **cf);

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
