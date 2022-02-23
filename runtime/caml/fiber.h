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

  /* [size_bucket] is a pointer to a bucket in Caml->stack_cache if this
   * size is pooled. If unpooled, it is NULL.
   *
   * Stacks may be unpooled if either the stack size is not 2**N multiple of
   * [caml_fiber_wsz] or the stack is bigger than pooled sizes. */
  struct stack_info** size_bucket;
  size_t size; /* only used when USE_MMAP_MAP_STACK is defined */
  uintnat magic;
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

/* Overview of the native stack switching primitives for effects
 *
 * For an understanding of effect handlers in OCaml please see:
 *  Retrofitting Effect Handlers onto OCaml, KC Sivaramakrishnan, et al.
 *  PLDI 2021
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
 */

/* The table of global identifiers */
extern value caml_global_data;

#define Trap_pc(tp) (((code_t *)(tp))[0])
#define Trap_link(tp) ((tp)[1])

struct stack_info** caml_alloc_stack_cache (void);
CAMLextern struct stack_info* caml_alloc_main_stack (uintnat init_size);
void caml_scan_stack(scanning_action f, void* fdata,
                     struct stack_info* stack, value* v_gc_regs);
/* try to grow the stack until at least required_size words are available.
   returns nonzero on success */
int caml_try_realloc_stack (asize_t required_size);
void caml_change_max_stack_size (uintnat new_max_size);
void caml_maybe_expand_stack(void);
CAMLextern void caml_free_stack(struct stack_info* stk);

#ifdef NATIVE_CODE
void caml_get_stack_sp_pc (struct stack_info* stack,
                           char** sp /* out */, uintnat* pc /* out */);
#endif

value caml_continuation_use (value cont);

/* Replace the stack of a continuation that was previouly removed
   with caml_continuation_use. The GC must not be allowed to run
   between continuation_use and continuation_replace.
   Used for cloning continuations and continuation backtraces. */
void caml_continuation_replace(value cont, struct stack_info* stack);

#endif /* CAML_INTERNALS */

#endif /* CAML_FIBER_H */
