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
#include "caml/domain.h"
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

CAMLnoreturn_start
  extern void caml_raise_exception (caml_domain_state* state, value bucket)
CAMLnoreturn_end;

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan
void caml_raise(value v)
{
  Unlock_exn();
  if (Caml_state->exception_pointer == NULL) caml_fatal_uncaught_exception(v);

  while (Caml_state->local_roots != NULL &&
         (char *) Caml_state->local_roots < Caml_state->exception_pointer) {
    Caml_state->local_roots = Caml_state->local_roots->next;
  }

  caml_raise_exception(Caml_state, v);
}

value caml_failwith_exn(char const *msg)
{
  return caml_raise_with_string_exn((value) caml_exn_Failure, msg);
}

value caml_failwith_value_exn(value msg)
{
  return caml_raise_with_arg_exn((value) caml_exn_Failure, msg);
}

value caml_invalid_argument_exn(char const *msg)
{
  return caml_raise_with_string_exn((value) caml_exn_Invalid_argument, msg);
}

value caml_invalid_argument_value_exn(value msg)
{
  return caml_raise_with_arg_exn((value) caml_exn_Invalid_argument, msg);
}

value caml_raise_out_of_memory_exn(void)
{
  return Make_exception_result((value) caml_exn_Out_of_memory);
}

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan
value caml_raise_stack_overflow_exn(void)
{
  return Make_exception_result((value) caml_exn_Stack_overflow);
}

value caml_raise_sys_error_exn(value msg)
{
  return caml_raise_with_arg_exn((value) caml_exn_Sys_error, msg);
}

value caml_raise_end_of_file_exn(void)
{
  return Make_exception_result((value) caml_exn_End_of_file);
}

value caml_raise_zero_divide_exn(void)
{
  return Make_exception_result((value) caml_exn_Division_by_zero);
}

value caml_raise_not_found_exn(void)
{
  return Make_exception_result((value) caml_exn_Not_found);
}

value caml_raise_sys_blocked_io_exn(void)
{
  return Make_exception_result((value) caml_exn_Sys_blocked_io);
}

/* We use a pre-allocated exception because we can't
   do a GC before the exception is raised (lack of stack descriptors
   for the ccall to [caml_array_bound_error]).  */

static const value * caml_array_bound_error_exception = NULL;

value caml_array_bound_error_exn(void)
{
  if (caml_array_bound_error_exception == NULL) {
    caml_array_bound_error_exception =
      caml_named_value("Pervasives.array_bound_error");
    if (caml_array_bound_error_exception == NULL) {
      fprintf(stderr, "Fatal error: exception "
                      "Invalid_argument(\"index out of bounds\")\n");
      exit(2);
    }
  }
  return Make_exception_result(*caml_array_bound_error_exception);
}

int caml_is_special_exception(value exn) {
  return exn == (value) caml_exn_Match_failure
    || exn == (value) caml_exn_Assert_failure
    || exn == (value) caml_exn_Undefined_recursive_module;
}
