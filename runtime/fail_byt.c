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
#include <stdlib.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/signals.h"
#include "caml/fiber.h"

CAMLexport void caml_raise(value v)
{
  Unlock_exn();
  CAMLassert(!Is_exception_result(v));

  // avoid calling caml_raise recursively
  v = caml_process_pending_actions_with_root_exn(v);
  if (Is_exception_result(v))
    v = Extract_exception(v);

  if (Caml_state->external_raise == NULL) {
    caml_terminate_signals();
    caml_fatal_uncaught_exception(v);
  }
  *Caml_state->external_raise->exn_bucket = v;

  Caml_state->local_roots = Caml_state->external_raise->local_roots;

  siglongjmp(Caml_state->external_raise->jmp->buf, 1);
}

CAMLexport void caml_raise_constant(value tag)
{
  caml_raise(tag);
}

CAMLexport void caml_raise_with_arg(value tag, value arg)
{
  CAMLparam2 (tag, arg);
  CAMLlocal1 (bucket);

  bucket = caml_alloc_small (2, 0);
  Field(bucket, 0) = tag;
  Field(bucket, 1) = arg;
  caml_raise(bucket);
  CAMLnoreturn;
}

CAMLexport void caml_raise_with_args(value tag, int nargs, value args[])
{
  CAMLparam1 (tag);
  CAMLxparamN (args, nargs);
  value bucket;
  int i;

  CAMLassert(1 + nargs <= Max_young_wosize);
  bucket = caml_alloc_small (1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
  caml_raise(bucket);
  CAMLnoreturn;
}

CAMLexport void caml_raise_with_string(value tag, char const *msg)
{
  CAMLparam1(tag);
  value v_msg = caml_copy_string(msg);
  caml_raise_with_arg(tag, v_msg);
  CAMLnoreturn;
}

/* PR#5115: Built-in exceptions can be triggered by input_value
   while reading the initial value of [caml_global_data].

   We check against this issue here in runtime/fail_byt.c instead of
   runtime/intern.c. Having the check here means that these calls will
   be slightly slower for all bytecode programs (not just the calls
   coming from intern). Because intern.c is shared between the bytecode and
   the native runtimes, putting checks there would slow do input_value for
   natively-compiled programs that do not need these checks.
*/
static void check_global_data(char const *exception_name)
{
  if (caml_global_data == 0 || !Is_block(caml_global_data)) {
    fprintf(stderr, "Fatal error: exception %s during initialisation\n",
            exception_name);
    exit(2);
  }
}

static void check_global_data_param(char const *exception_name, char const *msg)
{
  if (caml_global_data == 0 || !Is_block(caml_global_data)) {
    fprintf(stderr, "Fatal error: exception %s(\"%s\")\n", exception_name, msg);
    exit(2);
  }
}

Caml_inline value caml_get_failwith_tag (char const *msg)
{
  check_global_data_param("Failure", msg);
  return Field(caml_global_data, FAILURE_EXN);
}

CAMLexport void caml_failwith (char const *msg)
{
  caml_raise_with_string(caml_get_failwith_tag(msg), msg);
}

CAMLexport void caml_failwith_value (value msg)
{
  CAMLparam1(msg);
  value tag = caml_get_failwith_tag(String_val(msg));
  caml_raise_with_arg(tag, msg);
  CAMLnoreturn;
}

Caml_inline value caml_get_invalid_argument_tag (char const *msg)
{
  check_global_data_param("Invalid_argument", msg);
  return Field(caml_global_data, INVALID_EXN);
}

CAMLexport void caml_invalid_argument (char const *msg)
{
  caml_raise_with_string(caml_get_invalid_argument_tag(msg), msg);
}

CAMLexport void caml_invalid_argument_value (value msg)
{
  CAMLparam1(msg);
  value tag = caml_get_invalid_argument_tag(String_val(msg));
  caml_raise_with_arg(tag, msg);
  CAMLnoreturn;
}

CAMLexport void caml_array_bound_error(void)
{
  caml_invalid_argument("index out of bounds");
}

CAMLexport void caml_raise_out_of_memory(void)
{
  check_global_data("Out_of_memory");
  caml_raise_constant(Field(caml_global_data, OUT_OF_MEMORY_EXN));
}

CAMLexport void caml_raise_stack_overflow(void)
{
  check_global_data("Stack_overflow");
  caml_raise_constant(Field(caml_global_data, STACK_OVERFLOW_EXN));
}

CAMLexport void caml_raise_sys_error(value msg)
{
  check_global_data_param("Sys_error", String_val(msg));
  caml_raise_with_arg(Field(caml_global_data, SYS_ERROR_EXN), msg);
}

CAMLexport void caml_raise_end_of_file(void)
{
  check_global_data("End_of_file");
  caml_raise_constant(Field(caml_global_data, END_OF_FILE_EXN));
}

CAMLexport void caml_raise_zero_divide(void)
{
  check_global_data("Division_by_zero");
  caml_raise_constant(Field(caml_global_data, ZERO_DIVIDE_EXN));
}

CAMLexport void caml_raise_not_found(void)
{
  check_global_data("Not_found");
  caml_raise_constant(Field(caml_global_data, NOT_FOUND_EXN));
}

CAMLexport void caml_raise_sys_blocked_io(void)
{
  check_global_data("Sys_blocked_io");
  caml_raise_constant(Field(caml_global_data, SYS_BLOCKED_IO));
}

CAMLexport void caml_raise_continuation_already_taken(void)
{
  check_global_data("Continuation_already_taken");
  caml_raise_constant(Field(caml_global_data, CONTINUATION_ALREADY_TAKEN_EXN));
}

CAMLexport value caml_raise_if_exception(value res)
{
  if (Is_exception_result(res)) caml_raise(Extract_exception(res));
  return res;
}

int caml_is_special_exception(value exn) {
  /* this function is only used in caml_format_exception to produce
     a more readable textual representation of some exceptions. It is
     better to fall back to the general, less readable representation
     than to abort with a fatal error as above. */

  value f;

  if (caml_global_data == 0 || !Is_block(caml_global_data)) {
    return 0;
  }

  f = caml_global_data;
  return exn == Field(f, MATCH_FAILURE_EXN)
      || exn == Field(f, ASSERT_FAILURE_EXN)
      || exn == Field(f, UNDEFINED_RECURSIVE_MODULE_EXN);
}
