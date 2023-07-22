/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Raising exceptions from C. */

#include <stdio.h>
#include <signal.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/signals.h"
#include "caml/stack.h"
#include "caml/roots.h"
#include "caml/callback.h"
#include "caml/signals.h"
#include "caml/tsan.h"

/* The globals holding predefined exceptions */

typedef value caml_generated_constant[1];

extern caml_generated_constant
  caml_exn_Out_of_memory,
  caml_exn_Sys_error,
  caml_exn_Failure,
  caml_exn_Invalid_argument,
  caml_exn_End_of_file,
  caml_exn_Division_by_zero,
  caml_exn_Not_found,
  caml_exn_Match_failure,
  caml_exn_Sys_blocked_io,
  caml_exn_Stack_overflow,
  caml_exn_Assert_failure,
  caml_exn_Undefined_recursive_module;

/* Exception raising */

extern CAMLnoret
void caml_raise_exception (caml_domain_state* state, value bucket);

void caml_raise(value v)
{
  Caml_check_caml_state();
  char* exception_pointer;

  Unlock_exn();

  CAMLassert(!Is_exception_result(v));

  // avoid calling caml_raise recursively
  v = caml_process_pending_actions_with_root_exn(v);
  if (Is_exception_result(v))
    v = Extract_exception(v);

  exception_pointer = (char*)Caml_state->c_stack;

  if (exception_pointer == NULL) {
    caml_terminate_signals();
    caml_fatal_uncaught_exception(v);
  }

  while (Caml_state->local_roots != NULL &&
         (char *) Caml_state->local_roots < exception_pointer) {
    Caml_state->local_roots = Caml_state->local_roots->next;
  }

#if defined(WITH_THREAD_SANITIZER)
  caml_tsan_exit_on_raise_c(exception_pointer);
#endif

  caml_raise_exception(Caml_state, v);
}

value caml_failwith_exn(char const *msg)
{
  return caml_with_string_exn((value) caml_exn_Failure, msg);
}

value caml_failwith_value_exn(value msg)
{
  return caml_with_arg_exn((value) caml_exn_Failure, msg);
}

value caml_invalid_argument_exn(char const *msg)
{
  return caml_with_string_exn((value) caml_exn_Invalid_argument, msg);
}

value caml_invalid_argument_value_exn(value msg)
{
  return caml_with_arg_exn((value) caml_exn_Invalid_argument, msg);
}

value caml_out_of_memory_exn(void)
{
  return Make_exception_result((value) caml_exn_Out_of_memory);
}

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan
value caml_stack_overflow_exn(void)
{
  return Make_exception_result((value) caml_exn_Stack_overflow);
}

value caml_sys_error_exn(value msg)
{
  return caml_with_arg_exn((value) caml_exn_Sys_error, msg);
}

value caml_end_of_file_exn(void)
{
  return Make_exception_result((value) caml_exn_End_of_file);
}

value caml_zero_divide_exn(void)
{
  return Make_exception_result((value) caml_exn_Division_by_zero);
}

value caml_not_found_exn(void)
{
  return Make_exception_result((value) caml_exn_Not_found);
}

value caml_sys_blocked_io_exn(void)
{
  return Make_exception_result((value) caml_exn_Sys_blocked_io);
}

/* We use a pre-allocated exception because we can't
   do a GC before the exception is raised (lack of stack descriptors
   for the ccall to [caml_array_bound_error]).  */
value caml_array_bound_error_exn(void)
{
  static atomic_uintnat exn_cache = ATOMIC_UINTNAT_INIT(0);
  const value* exn = (const value*)atomic_load_acquire(&exn_cache);
  if (!exn) {
    exn = caml_named_value("Pervasives.array_bound_error");
    if (!exn) {
      fprintf(stderr, "Fatal error: exception "
        "Invalid_argument(\"index out of bounds\")\n");
      exit(2);
    }
    atomic_store_release(&exn_cache, (uintnat)exn);
  }
  return Make_exception_result(*exn);
}

void caml_array_bound_error_asm(void)
{
#if defined(WITH_THREAD_SANITIZER)
  char* exception_pointer = (char*)Caml_state->c_stack;
  caml_tsan_exit_on_raise_c(exception_pointer);
#endif

  /* This exception is raised directly from ocamlopt-compiled OCaml,
     not C, so we jump directly to the OCaml handler (and avoid GC) */
  value exn = caml_array_bound_error_exn();
  caml_raise_exception(Caml_state, Extract_exception(exn));
}

int caml_is_special_exception(value exn) {
  return exn == (value) caml_exn_Match_failure
    || exn == (value) caml_exn_Assert_failure
    || exn == (value) caml_exn_Undefined_recursive_module;
}
