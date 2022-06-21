/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Tom Kelly, OCaml Labs Consultancy                    */
/*                Stephen Dolan, University of Cambridge                  */
/*                                                                        */
/*   Copyright 2021 Indian Institute of Technology, Madras                */
/*   Copyright 2021 OCaml Labs Consultancy                                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_FIBER_H
#define CAML_FIBER_H

#ifdef CAML_INTERNALS

#include "misc.h"
#include "mlvalues.h"
#include "roots.h"

struct stack_info;

/* stack_handler describes the state for using fibers as part of effects */
struct stack_handler {
  value handle_value;
  value handle_exn;
  value handle_effect;
  struct stack_info* parent; /* parent OCaml stack if any */
};

/* stack_info describes the OCaml stack. It is used for:
 *  - storing information about the OCaml stack allowing it to be switched
 *  - accessing the stack_handler for the stack to handle effects
 *  - handling a freelist of OCaml stacks in a stack_cache
 */
struct stack_info {
#ifdef NATIVE_CODE
  void* sp;            /* stack pointer of the OCaml stack when suspended */
  void* exception_ptr; /* exception pointer of OCaml stack when suspended */
#else
  value* sp;
  value* exception_ptr;
#endif
  struct stack_handler* handler; /* effect handling state for the fiber */

  /* [cache_bucket] is an index into the [Caml_state->stack_cache] array if
   * this size is pooled. If unpooled, it is [-1].
   *
   * Stacks may be unpooled if either the stack size is not 2**N multiple of
   * [caml_fiber_wsz] or the stack is bigger than pooled sizes. */
  int cache_bucket;
  size_t size; /* only used when USE_MMAP_MAP_STACK is defined */
  uintnat magic;
  int64_t id;
};

CAML_STATIC_ASSERT(sizeof(struct stack_info) ==
                   Stack_ctx_words * sizeof(value));
#define Stack_base(stk) ((value*)(stk + 1))
#define Stack_threshold_ptr(stk) \
  (Stack_base(stk) + Stack_threshold / sizeof(value))
#define Stack_high(stk) (value*)stk->handler

#define Stack_handle_value(stk) (stk)->handler->handle_value
#define Stack_handle_exception(stk) (stk)->handler->handle_exn
#define Stack_handle_effect(stk) (stk)->handler->handle_effect
#define Stack_parent(stk) (stk)->handler->parent

/* Stack layout for native code. Stack grows downwards.
 *
 * +------------------------+
 * |  struct stack_handler  |
 * +------------------------+ <--- Stack_high
 * |     caml_runstack      |
 * +------------------------+
 * |                        |
 * .      OCaml frames      . <--- sp
 * |                        |
 * +------------------------+ <--- Stack_threshold
 * |                        |
 * .        Red Zone        .
 * |                        |
 * +------------------------+ <--- Stack_base
 * |   struct stack_info    |
 * +------------------------+ <--- Caml_state->current_stack
 */

/* This structure is used for storing the OCaml return pointer when
 * transitioning from an OCaml stack to a C stack at a C call. When an OCaml
 * stack is reallocated, this linked list is walked to update the OCaml stack
 * pointers. It is also used for DWARF backtraces. */
struct c_stack_link {
  /* The reference to the OCaml stack */
  struct stack_info* stack;
  /* OCaml return address */
  void* sp;
  struct c_stack_link* prev;
};

/* Overview of the stack switching primitives for effects
 *
 * For an understanding of effect handlers in OCaml please see:
 *  Retrofitting Effect Handlers onto OCaml, KC Sivaramakrishnan, et al.
 *  PLDI 2021
 *
 *  Native code
 *  -----------
 *
 * In native compilation the stack switching primitives Prunstack,
 * Pperform, Preperform and Presume make use of corresponding functions
 * implemented in the assembly files for an architecture (such as
 * runtime/amd64.S).
 *
 * A continuation object represents a suspended OCaml stack. It contains
 * the stack pointer tagged as an integer to avoid being followed by the GC.
 * In the code the tagged pointer can be referred to as a 'fiber':
 *     fiber := Val_ptr(stack)
 *
 * caml_runstack new_stack function argument
 *  caml_runstack launches a function (with an argument) in a new OCaml
 *  stack. It switches execution from the parent OCaml stack to the fresh
 *  stack and installs an exception handler. On return the new OCaml stack
 *  is freed, the stack is restored to the parent OCaml stack and the
 *  handle_value/handle_exn function is executed on the parent OCaml stack.
 *
 * caml_perform effect continuation
 *  caml_perform captures the current OCaml stack in the continuation object
 *  provided and raises the effect by switching to the parent OCaml stack and
 *  then executing the handle_effect function. Should there be no parent OCaml
 *  stack then the Unhandled exception is raised.
 *
 * caml_reperform effect continuation last_fiber
 *  caml_reperform is used to walk up the parent OCaml stacks to execute the
 *  next effect handler installed in the chain. This function is implemented
 *  by setting up the required registers then jumping into caml_perform which
 *  does the switch to the parent and execution of the handle_effect function.
 *
 * caml_resume new_fiber function argument
 *  caml_resume resumes execution on new_fiber by making the current stack
 *  the parent of the new_fiber and then switching to the stack for new_fiber.
 *  The function with argument is then executed on the new stack. Care is taken
 *  to check if the new_fiber argument has already been resumed and so is null.
 *
 *
 *  Bytecode
 *  --------
 *
 * In bytecode compilation the primitives are mapped to effect instructions and
 * some changes are made to the bytecode interpreter on every function return
 * and exception raise. In particular:
 *
 *  Presume | Prunstack -> RESUME (& RESUMETERM if a tail call)
 *   RESUME checks that the stack is valid (a NULL stack indicates a
 *   continuation that has already been resumed). The stacks are then switched
 *   with the old stack becoming the parent of the new stack. Care is taken
 *   to setup the exception handler for the new stack. Execution continues
 *   on the new OCaml stack with the passed function and argument.
 *
 *  Pperform -> PERFORM
 *   PERFORM captures the current stack in a continuation object it allocates.
 *   The parent stack is then switched to and the handle_effect function for
 *   the parent stack is executed. If no parent stack exists then the
 *   Unhandled exception is raised.
 *
 *  Preperform -> REPERFORMTERM
 *   REPERFORMTERM is used to walk up the parent OCaml stacks to execute the
 *   next effect handler installed in the chain. The instruction takes care to
 *   switch back to the continuation stack to raise the Unhandled exception in
 *   in the case no parent is left. Otherwise the instruction switches to the
 *   parent stack and executes the handle_effect function for that parent stack.
 *
 *  Special return handling:
 *   There is special handling on every function return (see do_return of
 *   interp.c). This handling allows the completion of a child stack to be
 *   detected. On completion of a child stack, the child stack is freed and
 *   control returns to the parent stack to execute the handle_value function.
 *
 *  Special exception handling:
 *   When an exception is raised (see raise_notrace of interp.c), the trap
 *   offset is checked. If there are no more exceptions in this stack and a
 *   parent stack exists, then the child stack is freed and the
 *   handle_exception function is executed on the parent stack.
 */

/* The table of global identifiers */
extern value caml_global_data;

#define Trap_pc(tp) (((code_t *)(tp))[0])
#define Trap_link(tp) ((tp)[1])

struct stack_info** caml_alloc_stack_cache (void);
CAMLextern struct stack_info* caml_alloc_main_stack (uintnat init_wsize);

void caml_scan_stack(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* v_gc_regs);

/* try to grow the stack until at least required_size words are available.
   returns nonzero on success */
CAMLextern int caml_try_realloc_stack (asize_t required_wsize);
CAMLextern uintnat caml_get_init_stack_wsize(void);
void caml_change_max_stack_size (uintnat new_max_wsize);
void caml_maybe_expand_stack(void);
CAMLextern void caml_free_stack(struct stack_info* stk);

#ifdef NATIVE_CODE
void caml_get_stack_sp_pc (struct stack_info* stack,
                           char** sp /* out */, uintnat* pc /* out */);
#endif

value caml_continuation_use (value cont);

/* Replace the stack of a continuation that was previously removed
   with caml_continuation_use. The GC must not be allowed to run
   between continuation_use and continuation_replace.
   Used for cloning continuations and continuation backtraces. */
void caml_continuation_replace(value cont, struct stack_info* stack);

#endif /* CAML_INTERNALS */

#endif /* CAML_FIBER_H */
