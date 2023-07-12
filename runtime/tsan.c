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

#define CAML_INTERNALS

#ifdef NATIVE_CODE

#ifndef DEBUG
#undef TSAN_DEBUG
#endif

#define UNW_LOCAL_ONLY
#include <libunwind.h>

#include "caml/tsan.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/frame_descriptors.h"
#include "caml/fiber.h"
#include "caml/domain_state.h"
#include "caml/stack.h"
#include "caml/config.h"
#ifdef TSAN_DEBUG
#include <execinfo.h> /* For backtrace_symbols */
#endif


/* Thread Sanitizer (TSan) provides backtraces for both events that cause a
   data race (i.e. read/write or write/write). One backtrace corresponds to the
   code position at the current memory event that caused the race, while the
   other backtrace corresponds to the other event that happened in the past.

   In order for TSan to provide this past event backtrace, each function entry
   must call `__tsan_func_entry` and each function exit must call
   `__tsan_func_exit`.
   The Cmm instrumentation pass is responsible for adding these entry/exit
   calls to TSan, so that TSan is able to re-create the backtrace on every
   memory access it's notified of.

   But exceptions and effects make the execution flow no longer linear: the
   code jumps from an exception raising point to the matching exception
   handler, or from a computation to the matching effect handler when
   performing an effect, and the other way around when resuming it. This
   prevents the `__tsan_func_exit` calls, inserted at the end of functions by
   the Cmm instrumentation pass, from being executed.

   It is important to keep TSan entries and exits balanced to avoid
   underflow/overflow on the TSan internal buffer used for reconstructing
   events backtrace.
   The runtime is responsible for emiting `__tsan_func_exit` call for each
   function "aborted" while raising an exception or performing an effect. They
   match the corresponding `__tsan_func_entry` call inserted by the Cmm
   instrumentation pass.
   Similarly, when a continuation is resumed, the execution flow jumps back
   into the computation, and the runtime must call `__tsan_func_entry` for each
   function that is re-entered.

   Functions that don't create a stack frame (i.e. functions entered into using
   the `jmp` instruction in `amd64.S') don't need to call `__tsan_func_entry`
   and `__tsan_func_exit`.

   1. Exceptions

     1.1 From OCaml

     Both `caml_raise_exn` and `caml_tsan_raise_notrace_exn` need to call
     `caml_tsan_exit_on_raise` to issue calls to `__tsan_func_exit` for each
     OCaml function exited by the exception. The process should be repeated
     when re-raising until the appropriate exception handler is found.

     1.2 From C

     Similarly, raising an exception from C using `caml_raise_exception` must
     be preceded by a call to `caml_tsan_exit_on_raise_c` to issue calls to
     `__tsan_func_exit` for each C function left in the current stack chunk.
     A distinct function needs to be used for C because, although both
     functions work by unwinding the stack, they use different mechanisms.
     OCaml stack chunks are traversed using the information from frame
     descriptors, while C stack frames (for which there are no frame
     descriptors) are traversed using the libunwind library.

   2. Effects

   Similary to exceptions, when `perform` is called `__tsan_func_exit` must be
   called for every function on the current fiber. The process can be repeated
   for every fiber's parents, because of `caml_repeform` until reaching the
   effect handler.
   When `resume` is called, the runtime must call `__tsan_func_entry` for every
   function in every fiber between the effect handler and the actual
   computation. */

Caml_inline void caml_tsan_debug_log_pc(const char* msg, uintnat pc)
{
#ifdef TSAN_DEBUG
    char **sym_names = backtrace_symbols((void **)&pc, 1);
    fprintf(stderr, "%s %s\n", msg, sym_names[0]);
    free(sym_names);
#else
    (void)msg; (void)pc;
#endif
}

/* This function is called by `caml_raise_exn` or `caml_tsan_raise_notrace_exn`
 from an OCaml stack.
 - [pc] is the program counter where `caml_raise_exn` would return, i.e. the
 next instruction after `caml_raise_exn` in the function that raised the
 exception.
 - [sp] is the stack pointer at the raising point, i.e. pointing just before
  [pc].
 - [trapsp] is the address of the next exception handler.

 This function iterates over every function stack frame between [sp] and
 [trapsp], calling `__tsan_func_exit` for each function. */
void caml_tsan_exit_on_raise(uintnat pc, char* sp, char* trapsp)
{
  caml_domain_state* domain_state = Caml_state;
  caml_frame_descrs fds = caml_get_frame_descrs();
  uintnat next_pc = pc;

  /* iterate on each frame  */
  while (1) {
    frame_descr* descr = caml_next_frame_descriptor(fds, &next_pc, &sp,
        domain_state->current_stack);

    if (descr == NULL) {
      return;
    }

    /* Stop when we reach the current exception handler */
    if (sp > trapsp) {
      break;
    }

    caml_tsan_debug_log_pc("forced__tsan_func_exit for", pc);
    __tsan_func_exit(NULL);
    pc = next_pc;
  }
}

/* This function must be called before `caml_raise_exception` on a C stack.
 - [limit] is the end of the current stack chunk.

 This function iterates over every function stack frame between the current
 stack pointer and [limit] using libunwind and calls `__tsan_func_exit` for
 each function. */
void caml_tsan_exit_on_raise_c(char* limit)
{
  unw_context_t uc;
  unw_cursor_t cursor;
  unw_word_t sp;
#ifdef TSAN_DEBUG
  unw_word_t prev_pc;
#endif
  int ret;

  ret = unw_getcontext(&uc);
  if (ret != 0)
    caml_fatal_error("unw_getcontext failed with code %d", ret);
  ret = unw_init_local(&cursor, &uc);
  if (ret != 0)
    caml_fatal_error("unw_init_local failed with code %d", ret);

  while (1) {
#ifdef TSAN_DEBUG
    if (unw_get_reg(&cursor, UNW_REG_IP, &prev_pc) < 0) {
      caml_fatal_error("unw_get_reg IP failed with code %d", ret);
    }
#endif

    ret = unw_step(&cursor);
    if (ret < 0) {
      caml_fatal_error("unw_step failed with code %d", ret);
    } else if (ret == 0) {
      /* No more frames */
      break;
    }

    ret = unw_get_reg(&cursor, UNW_REG_SP, &sp);
    if (ret != 0)
      caml_fatal_error("unw_get_reg SP failed with code %d", ret);
#ifdef TSAN_DEBUG
    caml_tsan_debug_log_pc("forced__tsan_func_exit for", prev_pc);
#endif
    __tsan_func_exit(NULL);

    if ((char*)sp >= limit) {
      break;
    }
  }
}

/* This function iterates on each stack frame of the current fiber. This is
   sufficient, since when the top of the stack is reached, the runtime switches
   to the parent fiber, and re-performs; as a consequence, this function will
   be called again.
   - [pc] is the program counter where `caml_(re)perform` will return.
   - [sp] is the stack pointer at the perform point. */
void caml_tsan_exit_on_perform(uintnat pc, char* sp)
{
  struct stack_info* stack = Caml_state->current_stack;
  caml_frame_descrs fds = caml_get_frame_descrs();
  uintnat next_pc = pc;

  /* iterate on each frame  */
  while (1) {
    frame_descr* descr = caml_next_frame_descriptor(fds, &next_pc, &sp, stack);

    caml_tsan_debug_log_pc("forced__tsan_func_exit for", pc);
    __tsan_func_exit(NULL);

    if (descr == NULL) {
      break;
    }
    pc = next_pc;
  }
}

/* This function is executed after switching to the deeper fiber, but before
   the linked list of fibers from the current one to the handler's has been
   restored by restoring the parent link to the handler's stack. As a
   consequence, this function simply iterates on each stack frame, following
   links to parent fibers, until that link is NULL. This way, it performs a
   `__tsan_func_entry` for each stack frame between the current and the
   handler's stack.
   We use non-tail recursion to call `__tsan_func_entry` in the reverse order
   of iteration.
   - [pc] is the program counter where `caml_perform` was called.
   - [sp] is the stack pointer at the perform point. */
CAMLreally_no_tsan void caml_tsan_entry_on_resume(uintnat pc, char* sp,
    struct stack_info const* stack)
{
  caml_frame_descrs fds = caml_get_frame_descrs();
  uintnat next_pc = pc;

  caml_next_frame_descriptor(fds, &next_pc, &sp, (struct stack_info*)stack);
  if (next_pc == 0) {
    stack = stack->handler->parent;
    if (!stack) {
      return;
    }

    char* p = (char*)stack->sp;
#ifdef WITH_FRAME_POINTERS
    p += sizeof(value); /* Would not work on POWER (but POWER is not supported
                           by TSan anyway) */
#endif
    next_pc = *(uintnat*)p;
    sp = p + sizeof(value);
  }

  caml_tsan_entry_on_resume(next_pc, sp, stack);
  caml_tsan_debug_log_pc("forced__tsan_func_entry for", pc);
  __tsan_func_entry((void*)next_pc);
}

#endif // NATIVE_CODE


#include "caml/mlvalues.h"
#include <stdbool.h>

Caml_inline bool is_aligned(void *ptr, size_t byte_count)
{
  return (uintptr_t)ptr % byte_count == 0;
}

#include <stdint.h>

extern uint8_t __tsan_atomic8_load(void*, int);
extern uint16_t __tsan_atomic16_load(void*, int);
extern uint32_t __tsan_atomic32_load(void*, int);
extern uint64_t __tsan_atomic64_load(void*, int);
extern unsigned __int128 __tsan_atomic128_load(void*, int);

/* In the OCaml runtime, volatile reads are used instead of relaxed atomic
   loads on values that are shared with OCaml code, for backward compatibility
   and performance reasons (see #10992). To avoid this practice causing false
   positives with TSan, we make it so that TSan consider these reads as relaxed
   atomic loads. Volatile stores are still seen as plain stores. */
#define DEFINE_TSAN_VOLATILE_READ_WRITE(size, bitsize)                         \
                                                                               \
extern void __tsan_read##size(void*);                                          \
extern void __tsan_write##size(void*);                                         \
                                                                               \
CAMLreally_no_tsan void __tsan_volatile_read##size(void *ptr)                  \
{                                                                              \
  const bool is_atomic = size <= sizeof(long long) &&                          \
             is_aligned(ptr, 8);                                               \
  if (is_atomic)                                                               \
    __tsan_atomic##bitsize##_load(ptr, memory_order_relaxed);                  \
  else                                                                         \
    __tsan_read##size(ptr);                                                    \
}                                                                              \
CAMLreally_no_tsan void __tsan_unaligned_volatile_read##size(void *ptr)        \
{                                                                              \
  __tsan_volatile_read##size(ptr);                                             \
}                                                                              \
CAMLreally_no_tsan void __tsan_volatile_write##size(void *ptr)                 \
{                                                                              \
  __tsan_write##size(ptr);                                                     \
}                                                                              \
CAMLreally_no_tsan void __tsan_unaligned_volatile_write##size(void *ptr)       \
{                                                                              \
  __tsan_volatile_write##size(ptr);                                            \
}

DEFINE_TSAN_VOLATILE_READ_WRITE(1, 8);
DEFINE_TSAN_VOLATILE_READ_WRITE(2, 16);
DEFINE_TSAN_VOLATILE_READ_WRITE(4, 32);
DEFINE_TSAN_VOLATILE_READ_WRITE(8, 64);
DEFINE_TSAN_VOLATILE_READ_WRITE(16, 128);
