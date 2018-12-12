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

/* The globals holding predefined exceptions */

typedef caml_value caml_generated_constant[1];

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
  extern void caml_raise_exception (caml_value bucket)
CAMLnoreturn_end;

char * caml_exception_pointer = NULL;

void caml_raise(caml_value v)
{
  Unlock_exn();
  if (caml_exception_pointer == NULL) caml_fatal_uncaught_exception(v);

  while (caml_local_roots != NULL &&
         (char *) caml_local_roots < caml_exception_pointer) {
    caml_local_roots = caml_local_roots->next;
  }

  caml_raise_exception(v);
}

void caml_raise_constant(caml_value tag)
{
  caml_raise(tag);
}

void caml_raise_with_arg(caml_value tag, caml_value arg)
{
  CAMLparam2 (tag, arg);
  CAMLlocal1 (bucket);

  bucket = caml_alloc_small (2, 0);
  Field(bucket, 0) = tag;
  Field(bucket, 1) = arg;
  caml_raise(bucket);
  CAMLnoreturn;
}

void caml_raise_with_args(caml_value tag, int nargs, caml_value args[])
{
  CAMLparam1 (tag);
  CAMLxparamN (args, nargs);
  caml_value bucket;
  int i;

  CAMLassert(1 + nargs <= Max_young_wosize);
  bucket = caml_alloc_small (1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
  caml_raise(bucket);
  CAMLnoreturn;
}

void caml_raise_with_string(caml_value tag, char const *msg)
{
  CAMLparam1(tag);
  caml_value v_msg = caml_copy_string(msg);
  caml_raise_with_arg(tag, v_msg);
  CAMLnoreturn;
}

void caml_failwith (char const *msg)
{
  caml_raise_with_string((caml_value) caml_exn_Failure, msg);
}

void caml_failwith_value (caml_value msg)
{
  caml_raise_with_arg((caml_value) caml_exn_Failure, msg);
}

void caml_invalid_argument (char const *msg)
{
  caml_raise_with_string((caml_value) caml_exn_Invalid_argument, msg);
}

void caml_invalid_argument_value (caml_value msg)
{
  caml_raise_with_arg((caml_value) caml_exn_Invalid_argument, msg);
}

void caml_raise_out_of_memory(void)
{
  caml_raise_constant((caml_value) caml_exn_Out_of_memory);
}

void caml_raise_stack_overflow(void)
{
  caml_raise_constant((caml_value) caml_exn_Stack_overflow);
}

void caml_raise_sys_error(caml_value msg)
{
  caml_raise_with_arg((caml_value) caml_exn_Sys_error, msg);
}

void caml_raise_end_of_file(void)
{
  caml_raise_constant((caml_value) caml_exn_End_of_file);
}

void caml_raise_zero_divide(void)
{
  caml_raise_constant((caml_value) caml_exn_Division_by_zero);
}

void caml_raise_not_found(void)
{
  caml_raise_constant((caml_value) caml_exn_Not_found);
}

void caml_raise_sys_blocked_io(void)
{
  caml_raise_constant((caml_value) caml_exn_Sys_blocked_io);
}

/* We use a pre-allocated exception because we can't
   do a GC before the exception is raised (lack of stack descriptors
   for the ccall to [caml_array_bound_error]).  */

static caml_value * caml_array_bound_error_exn = NULL;

void caml_array_bound_error(void)
{
  if (caml_array_bound_error_exn == NULL) {
    caml_array_bound_error_exn =
      caml_named_value("Pervasives.array_bound_error");
    if (caml_array_bound_error_exn == NULL) {
      fprintf(stderr, "Fatal error: exception "
                      "Invalid_argument(\"index out of bounds\")\n");
      exit(2);
    }
  }
  caml_raise(*caml_array_bound_error_exn);
}

int caml_is_special_exception(caml_value exn) {
  return exn == (caml_value) caml_exn_Match_failure
    || exn == (caml_value) caml_exn_Assert_failure
    || exn == (caml_value) caml_exn_Undefined_recursive_module;
}
