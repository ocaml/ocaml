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

CAMLnoret extern
void caml_raise_exception (caml_domain_state* state, value bucket);

void caml_raise(value v)
{
  Caml_check_caml_state();
  char* exception_pointer;
  CAMLassert(!Is_exception_result(v));

  caml_channel_cleanup_on_raise();

  caml_result result = caml_process_pending_actions_with_root_res(v);
  /* If the result is a value, we want to assign it to [v].
     If the result is an exception, we want to raise it instead of [v].
     The line below does both these things at once. */
  v = result.data;

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

value caml_exception_failure(char const *msg)
{
  return caml_exception_with_string((value)caml_exn_Failure, msg);
}

value caml_exception_failure_value(value msg)
{
  return caml_exception_with_arg((value)caml_exn_Failure, msg);
}

value caml_exception_invalid_argument(char const *msg)
{
  return caml_exception_with_string((value)caml_exn_Invalid_argument, msg);
}

value caml_exception_invalid_argument_value(value msg)
{
  return caml_exception_with_arg((value)caml_exn_Invalid_argument, msg);
}

value caml_exception_out_of_memory(void)
{
  return (value)caml_exn_Out_of_memory;
}

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan
value caml_exception_stack_overflow(void)
{
  return (value)caml_exn_Stack_overflow;
}

value caml_exception_sys_error(value msg)
{
  return caml_exception_with_arg((value)caml_exn_Sys_error, msg);
}

value caml_exception_end_of_file(void)
{
  return (value)caml_exn_End_of_file;
}

value caml_exception_zero_divide(void)
{
  return (value)caml_exn_Division_by_zero;
}

value caml_exception_not_found(void)
{
  return (value)caml_exn_Not_found;
}

value caml_exception_sys_blocked_io(void)
{
  return (value)caml_exn_Sys_blocked_io;
}

/* We use a pre-allocated exception because we can't
   do a GC before the exception is raised (lack of stack descriptors
   for the ccall to [caml_array_bound_error]).  */
value caml_exception_array_bound_error(void)
{
  static _Atomic(const value *) exn_cache = NULL;
  const value *exn = atomic_load_acquire(&exn_cache);
  if (!exn) {
    exn = caml_named_value("Pervasives.array_bound_error");
    if (!exn) {
      fprintf(stderr, "Fatal error: exception "
        "Invalid_argument(\"index out of bounds\")\n");
      exit(2);
    }
    atomic_store_release(&exn_cache, exn);
  }
  return *exn;
}

void caml_array_bound_error_asm(void)
{
#if defined(WITH_THREAD_SANITIZER)
  char* exception_pointer = (char*)Caml_state->c_stack;
  caml_tsan_exit_on_raise_c(exception_pointer);
#endif

  /* This exception is raised directly from ocamlopt-compiled OCaml,
     not C, so we jump directly to the OCaml handler (and avoid GC) */
  caml_raise_exception(Caml_state, caml_exception_array_bound_error());
}

int caml_is_special_exception(value exn) {
  return exn == (value) caml_exn_Match_failure
    || exn == (value) caml_exn_Assert_failure
    || exn == (value) caml_exn_Undefined_recursive_module;
}
