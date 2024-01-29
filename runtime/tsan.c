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
   The runtime is responsible for emitting `__tsan_func_exit` call for each
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

     Exception raising must be preceded by a call to `caml_tsan_exit_on_raise`
     to issue calls to `__tsan_func_exit` for each OCaml function exited by the
     exception. The process should be repeated when re-raising until the
     appropriate exception handler is found.

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

   Similarly to exceptions, when `perform` is called `__tsan_func_exit` must be
   called for every function on the current fiber. The process can be repeated
   for every fiber's parents, because of `caml_repeform` until reaching the
   effect handler.
   When `resume` is called, the runtime must call `__tsan_func_entry` for every
   function in every fiber between the effect handler and the actual
   computation.

   3. Memory model considerations

   TSan is designed to detect data races as defined by the C11 memory model.
   Yet we are using it to detect data races in OCaml programs, and even in a
   mix of OCaml and C code (e.g. between the program and the runtime). To
   accomplish this, we use a carefully chosen mapping of OCaml memory accesses
   to C11 accesses.

   For each type of OCaml memory access on the left, we signal to TSan the
   operations on the right:

   OCaml access      | C11 equivalent          | TSan view
   ------------------|-------------------------|--------------------------------
   Atomic load       | fence(acquire)          | __tsan_atomic64_load(seq_cst)
                     | atomic_load(seq_cst)    |
   ------------------|-------------------------|--------------------------------
   Atomic store      | fence(acquire)          | __tsan_atomic_exchange(seq_cst)
                     | atomic_exchange(seq_cst)|
                     | fence(release)          |
   ------------------|-------------------------|--------------------------------
   Non-atomic load   | atomic_load(relaxed)    | __tsan_read8()
   ------------------|-------------------------|--------------------------------
   Non-atomic store  | fence(acquire)          | __tsan_write8()
   (assignment, int) | atomic_store(release)   |
   ------------------|-------------------------|--------------------------------
   Non-atomic store  | fence(acquire)          | __tsan_write8()
   (assignment, ptr) | atomic_store(release)   |
                     | fence(release)          |
   ------------------|-------------------------|--------------------------------
   Non-atomic store  | plain store             | Not instrumented
   (initialization)  |                         |
   ------------------|-------------------------|--------------------------------
   Non-atomic store  | plain store             | __tsan_writeN()
   (unaligned size)  |                         |
   ------------------|-------------------------|--------------------------------

   This mapping dictates which instrumentation calls are generated by the
   compiler, but also how some functions of the runtime are instrumented: for
   example, `caml_modify` is instrumented with a call to `__tsan_write8`,
   whereas `caml_initialize` is purposefully not instrumented.

   3.1. False negatives

   There should be no false negatives, i.e., all data races (in the OCaml
   sense) on visited code paths should be detected (modulo TSan limitations
   such as the finite history of memory accesses).

   3.2. False positives

   The mapping that we use should not incur any false positives in pure OCaml
   code, i.e., all data races reported between two accesses from OCaml are
   true data races (in the OCaml sense of the term).

   In mixed C-OCaml code, rare false positives may occur in the following cases:

   - A value is initialized from C without using `caml_initialize` (allowed by
     the FFI rules on the condition that the GC does not run between the
     allocation and the end of initialization) and a conflicting access is made
     from OCaml after publication to other threads. There should be no data
     race thanks to data dependency (see [MMOC] coment in memory.c), but TSan
     does not take data dependencies into account.
   - A field is accessed from C with `Field`, or more generally using a
     `volatile value *` or a relaxed atomic access, and that field is modified
     concurrently by OCaml code. Because `caml_modify` is instrumented as a
     plain write for proper detection of OCaml races, this case is seen as a
     data race.

   3.3. volatile accesses

   We consider volatile accesses in C to behave like relaxed atomic accesses
   from the point of view of data races (see [MMOC] comment in memory.c). It is
   not trivial to explain that to TSan. Fortunately, both GCC and Clang have an
   option to distinguish volatile writes in a custom way, by instrumenting them
   with a call to a symbol that is left to us to implement. This option has been
   introduced to support KCSan, the kernel concurrency sanitizer of the Linux
   kernel. However, as KCSan is very different from TSan, volatile accesses are
   instrumented in a way that makes it difficult to handle volatile writes the
   way we would like. While gcc/clang replace atomic read/write operations with
   TSan calls that update TSan's internal state and perform the actual memory
   operation themselves, volatile read/write operations are merely decorated
   with a TSan call, and then the actual operation is performed.

   More details and examples can be found in PR #12681.

   Our current make-do solution is that `__tsan_volatile_readN` performs a
   dummy call to `__tsan_atomic64_load`, which is sufficient for TSan to view
   them as relaxed loads; and `__tsan_volatile_writeN` performs a dummy
   fetch_add of zero. */


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
    if (descr == NULL) {
      break;
    }

    caml_tsan_debug_log_pc("forced__tsan_func_exit for", pc);
    __tsan_func_exit(NULL);

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
CAMLno_tsan void caml_tsan_entry_on_resume(uintnat pc, char* sp,
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

    sp = First_frame(stack->sp);
    next_pc = Saved_return_address(sp);
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

/* Make TSan see word-aligned volatile accesses as relaxed atomic accesses.
   Refer to the detailed comments at the beginning of this file. */
#define DEFINE_TSAN_VOLATILE_READ_WRITE(size, bitsize)                         \
                                                                               \
extern void __tsan_read##size(void*);                                          \
extern void __tsan_write##size(void*);                                         \
                                                                               \
extern uint##bitsize##_t __tsan_atomic##bitsize##_load(void*, int);            \
extern uint##bitsize##_t __tsan_atomic##bitsize##_fetch_add(                   \
    volatile void*, uint##bitsize##_t, memory_order);                          \
                                                                               \
CAMLno_tsan void __tsan_volatile_read##size(void *ptr)                         \
{                                                                              \
  const bool is_atomic = size <= sizeof(long long) && is_aligned(ptr, 8);      \
  if (is_atomic)                                                               \
    __tsan_atomic##bitsize##_load(ptr, memory_order_relaxed);                  \
  else                                                                         \
    __tsan_read##size(ptr);                                                    \
}                                                                              \
CAMLno_tsan void __tsan_unaligned_volatile_read##size(void *ptr)               \
{                                                                              \
  __tsan_volatile_read##size(ptr);                                             \
}                                                                              \
CAMLno_tsan void __tsan_volatile_write##size(void *ptr)                        \
{                                                                              \
  const bool is_atomic = size <= sizeof(long long) && is_aligned(ptr, 8);      \
  if (is_atomic) {                                                             \
    /* Signal a relaxed atomic store to TSan. We don't have access to the      \
       actual value written so we do a fetch_add of 0 which has the effect of  \
       signaling a relaxed store without changing the value. */                \
    __tsan_atomic##bitsize##_fetch_add(ptr, 0, memory_order_relaxed);          \
  } else                                                                       \
    __tsan_write##size(ptr);                                                   \
}                                                                              \
CAMLno_tsan void __tsan_unaligned_volatile_write##size(void *ptr)              \
{                                                                              \
  __tsan_volatile_write##size(ptr);                                            \
}

DEFINE_TSAN_VOLATILE_READ_WRITE(1, 8);
DEFINE_TSAN_VOLATILE_READ_WRITE(2, 16);
DEFINE_TSAN_VOLATILE_READ_WRITE(4, 32);
DEFINE_TSAN_VOLATILE_READ_WRITE(8, 64);

/* We do not treat accesses to 128-bit (a.k.a. 16-byte) values as atomic, since
   it is dubious that they can be treated as such. Still, the functions below
   are needed because, without them, building a C library for OCaml with TSan
   enabled will fail at the linking step with an unresolved symbol error if it
   contains volatile accesses to 128-bit values. It is better to have 128-bit
   volatiles behave silently like plain 128-bit values. */

extern void __tsan_read16(void*);
extern void __tsan_write16(void*);

CAMLno_tsan void __tsan_volatile_read16(void *ptr)
{
    __tsan_read16(ptr);
}
CAMLno_tsan void __tsan_unaligned_volatile_read16(void *ptr)
{
  __tsan_read16(ptr);
}
CAMLno_tsan void __tsan_volatile_write16(void *ptr)
{
    __tsan_write16(ptr);
}
CAMLno_tsan void __tsan_unaligned_volatile_write16(void *ptr)
{
  __tsan_write16(ptr);
}
