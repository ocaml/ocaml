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

/* Basic types and constants */

typedef size_t asize_t;

#ifndef NULL
#define NULL 0
#endif

/* <private> */
typedef char * addr;
/* </private> */

/* Noreturn is preserved for compatibility reasons.
   Instead of the legacy GCC/Clang-only
     foo Noreturn;
   you should prefer
     CAMLnoreturn_start foo CAMLnoreturn_end;
   which supports both GCC/Clang and MSVC.

   Note: CAMLnoreturn is a different macro defined in memory.h,
   to be used in function bodies rather than  aprototype attribute.
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

/* Weak function definitions that can be overriden by external libs */
/* Conservatively restricted to ELF and MacOSX platforms */
#if defined(__GNUC__) && (defined (__ELF__) || defined(__APPLE__))
#define CAMLweakdef __attribute__((weak))
#else
#define CAMLweakdef
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* GC timing hooks. These can be assigned by the user.
   [caml_minor_gc_begin_hook] must not allocate nor change any heap value.
   The others can allocate and even call back to OCaml code.
*/
typedef void (*caml_timing_hook) (void);
extern caml_timing_hook caml_major_slice_begin_hook, caml_major_slice_end_hook;
extern caml_timing_hook caml_minor_gc_begin_hook, caml_minor_gc_end_hook;
extern caml_timing_hook caml_finalise_begin_hook, caml_finalise_end_hook;

/* Assertions */

#ifdef DEBUG
#define CAMLassert(x) \
  ((x) ? (void) 0 : caml_failed_assert ( #x , __FILE__, __LINE__))
CAMLnoreturn_start
CAMLextern int caml_failed_assert (char *, char *, int)
CAMLnoreturn_end;
#else
#define CAMLassert(x) ((void) 0)
#endif

CAMLnoreturn_start
CAMLextern void caml_fatal_error (char *msg)
CAMLnoreturn_end;

CAMLnoreturn_start
CAMLextern void caml_fatal_error_arg (char *fmt, char *arg)
CAMLnoreturn_end;

CAMLnoreturn_start
CAMLextern void caml_fatal_error_arg2 (char *fmt1, char *arg1,
                                       char *fmt2, char *arg2)
CAMLnoreturn_end;

/* Safe string operations */

CAMLextern char * caml_strdup(const char * s);
CAMLextern char * caml_strconcat(int n, ...); /* n args of const char * type */

/* <private> */

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

/* GC flags and messages */

extern uintnat caml_verb_gc;
void caml_gc_message (int, char *, uintnat);

/* Runtime warnings */
extern uintnat caml_runtime_warnings;
int caml_runtime_warnings_active(void);

/* Memory routines */

char *caml_aligned_malloc (asize_t bsize, int, void **);

#ifdef DEBUG
#ifdef ARCH_SIXTYFOUR
#define Debug_tag(x) (0xD700D7D7D700D6D7ul \
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
  10 -> uninitialised fields of minor objects
  11 -> uninitialised fields of major objects
  15 -> uninitialised words of [caml_aligned_malloc] blocks
  85 -> filler bytes of [caml_aligned_malloc]

  special case (byte by byte):
  D7 -> uninitialised words of [caml_stat_alloc] blocks
*/
#define Debug_free_minor     Debug_tag (0x00)
#define Debug_free_major     Debug_tag (0x01)
#define Debug_free_shrink    Debug_tag (0x03)
#define Debug_free_truncate  Debug_tag (0x04)
#define Debug_uninit_minor   Debug_tag (0x10)
#define Debug_uninit_major   Debug_tag (0x11)
#define Debug_uninit_align   Debug_tag (0x15)
#define Debug_filler_align   Debug_tag (0x85)

#define Debug_uninit_stat    0xD7

/* Note: the first argument is in fact a [value] but we don't have this
   type available yet because we can't include [mlvalues.h] in this file.
*/
extern void caml_set_fields (intnat v, unsigned long, unsigned long);
#endif /* DEBUG */


#ifndef CAML_AVOID_CONFLICTS
#define Assert CAMLassert
#endif

/* snprintf emulation for Win32 */

#if defined(_WIN32) && !defined(_UCRT)
extern int caml_snprintf(char * buf, size_t size, const char * format, ...);
#define snprintf caml_snprintf
#endif

#ifdef CAML_INSTR
/* Timers and counters for GC latency profiling (Linux-only) */

#include <time.h>
#include <stdio.h>

extern intnat caml_stat_minor_collections;
extern intnat CAML_INSTR_STARTTIME, CAML_INSTR_STOPTIME;

struct CAML_INSTR_BLOCK {
  struct timespec ts[10];
  char *tag[10];
  int index;
  struct CAML_INSTR_BLOCK *next;
};

extern struct CAML_INSTR_BLOCK *CAML_INSTR_LOG;

/* Declare a timer/counter name. [t] must be a new variable name. */
#define CAML_INSTR_DECLARE(t)                                       \
  struct CAML_INSTR_BLOCK *t = NULL

/* Allocate the data block for a given name.
   [t] must have been declared with [CAML_INSTR_DECLARE]. */
#define CAML_INSTR_ALLOC(t) do{                                     \
    if (caml_stat_minor_collections >= CAML_INSTR_STARTTIME         \
        && caml_stat_minor_collections < CAML_INSTR_STOPTIME){      \
      t = malloc (sizeof (struct CAML_INSTR_BLOCK));                \
      t->index = 0;                                                 \
      t->tag[0] = "";                                               \
      t->next = CAML_INSTR_LOG;                                     \
      CAML_INSTR_LOG = t;                                           \
    }                                                               \
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
extern void CAML_INSTR_INIT (void);

/* This function is automatically called by the runtime to output
   the collected data to the dump file. */
extern void CAML_INSTR_ATEXIT (void);

#else /* CAML_INSTR */

#define CAML_INSTR_DECLARE(t) /**/
#define CAML_INSTR_ALLOC(t) /**/
#define CAML_INSTR_START(t, name) /**/
#define CAML_INSTR_SETUP(t, name) /**/
#define CAML_INSTR_TIME(t, msg) /**/
#define CAML_INSTR_INT(msg, c) /**/
#define CAML_INSTR_INIT() /**/
#define CAML_INSTR_ATEXIT() /**/

#endif /* CAML_INSTR */

/* </private> */

#ifdef __cplusplus
}
#endif

#endif /* CAML_MISC_H */
