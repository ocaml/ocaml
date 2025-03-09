/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*               Fabrice Buoro and Olivier Nicole, Tarides                */
/*                                                                        */
/*   Copyright 2023 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_TSAN_H
#define CAML_TSAN_H

#ifdef __cplusplus
extern "C" {
#endif

/* Macro used to deactivate thread sanitizer on some functions. */
#define CAMLno_tsan
/* `__has_feature` is present in Clang and recent GCCs (14 and later). Older
   GCCs define `__SANITIZE_THREAD__`. In addition, starting from version 14
   GCC supports the Clang-originating syntax `no_sanitize("thread")`.
   With Clang, `no_sanitize("thread")` does not seem to disable instrumentation
   entirely, so we need to use the stronger, Clang-specific attribute
   `disable_sanitizer_instrumentation`.
   This should select the right attribute in all circumstances. */
#if defined(__has_feature)
#  if __has_feature(thread_sanitizer)
#    undef CAMLno_tsan
#    if defined(__has_attribute)
#      if __has_attribute(disable_sanitizer_instrumentation)
#        define CAMLno_tsan \
           __attribute__((disable_sanitizer_instrumentation))
#      else
#        define CAMLno_tsan __attribute__((no_sanitize("thread")))
#      endif
#    else
#      define CAMLno_tsan __attribute__((no_sanitize("thread")))
#    endif
#  endif
#else
#  if defined(__SANITIZE_THREAD__)
#    undef CAMLno_tsan
#    define CAMLno_tsan __attribute__((no_sanitize_thread))
#  endif
#endif

/* TSan records a release operation on encountering ANNOTATE_HAPPENS_BEFORE
 * and similarly an acquire operation on encountering ANNOTATE_HAPPENS_AFTER.
   These annotations are used to eliminate false positives. */
#define CAML_TSAN_ANNOTATE_HAPPENS_BEFORE(addr)
#define CAML_TSAN_ANNOTATE_HAPPENS_AFTER(addr)

#if defined(WITH_THREAD_SANITIZER)
#  undef CAML_TSAN_ANNOTATE_HAPPENS_BEFORE
#  undef CAML_TSAN_ANNOTATE_HAPPENS_AFTER

#  define CAML_TSAN_ANNOTATE_HAPPENS_BEFORE(addr)              \
  AnnotateHappensBefore(__FILE__, __LINE__, (void *)(addr));
#  define CAML_TSAN_ANNOTATE_HAPPENS_AFTER(addr)               \
  AnnotateHappensAfter(__FILE__, __LINE__, (void *)(addr));

extern void AnnotateHappensBefore(const char *f, int l, void *addr);
extern void AnnotateHappensAfter(const char *f, int l, void *addr);
#endif

/* Macro used to un-instrument some functions of the runtime for performance
   reasons, except if TSAN_INSTRUMENT_ALL is set. */
#if defined(TSAN_INSTRUMENT_ALL)
#  define CAMLno_tsan_for_perf
#else
#  define CAMLno_tsan_for_perf CAMLno_tsan
#endif

#ifdef __cplusplus
}
#endif

#ifdef CAML_INTERNALS

#include "mlvalues.h"

struct stack_info;

CAMLextern void caml_tsan_exit_on_raise(uintnat pc, char* sp, char* trapsp);
CAMLextern void caml_tsan_exit_on_raise_c(char* limit);

CAMLextern void caml_tsan_exit_on_perform(uintnat pc, char* sp);
CAMLextern void caml_tsan_entry_on_resume(uintnat pc, char* sp,
    struct stack_info const* stack);

extern void __tsan_func_exit(void*);
extern void __tsan_func_entry(void*);
void __tsan_write8(void *location);

#endif /* CAML_INTERNALS */

#endif /* CAML_TSAN_H */
