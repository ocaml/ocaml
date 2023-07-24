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

/* Macro used to deactivate thread sanitizer on some functions. */
#define CAMLreally_no_tsan
/* __has_feature is Clang-specific, but GCC defines __SANITIZE_ADDRESS__ and
 * __SANITIZE_THREAD__. */
#if defined(__has_feature)
#  if __has_feature(thread_sanitizer)
#    undef CAMLreally_no_tsan
#    if defined(__has_attribute)
#      if __has_attribute(disable_sanitizer_instrumentation)
#        define CAMLreally_no_tsan \
           __attribute__((disable_sanitizer_instrumentation))
#      else
#        define CAMLreally_no_tsan __attribute__((no_sanitize("thread")))
#      endif
#    else
#      define CAMLreally_no_tsan __attribute__((no_sanitize("thread")))
#    endif
#  endif
#else
#  if __SANITIZE_THREAD__
#    undef CAMLreally_no_tsan
#    define CAMLreally_no_tsan __attribute__((no_sanitize_thread))
#  endif
#endif

/* Macro used to deactivate ThreadSanitizer on some functions, but only in
   ThreadSanitizer-enabled installations of OCaml. This has two functions:
   removing some ThreadSanitizer warnings from the runtime in user programs on
   a switch configured with --enable-tsan, and manually instrumenting some
   functions, which requires disabling built-in instrumentation (see
   [caml_modify]). This macro has no effect when OCaml is configured without
   --enable-tsan, so that compiler developers can still detect bugs in these
   functions using ThreadSanitizer. */
#define CAMLno_tsan
#if defined(WITH_THREAD_SANITIZER)
#  undef CAMLno_tsan
#  define CAMLno_tsan CAMLreally_no_tsan
#endif


#ifdef CAML_INTERNALS

#include "caml/mlvalues.h"

struct stack_info;

CAMLextern void caml_tsan_exit_on_raise_c(char* limit);

CAMLextern void caml_tsan_exit_on_perform(uintnat pc, char* sp);
CAMLextern void caml_tsan_entry_on_resume(uintnat pc, char* sp,
    struct stack_info const* stack);

extern void __tsan_func_exit(void*);
extern void __tsan_func_entry(void*);
void __tsan_write8(void *location);


#endif /* CAML_INTERNALS */

#endif /* CAML_TSAN_H */
