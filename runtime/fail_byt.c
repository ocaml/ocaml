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
  Caml_check_caml_state();
  CAMLassert(!Is_exception_result(v));

  caml_channel_cleanup_on_raise();

  caml_result result = caml_process_pending_actions_with_root_res(v);
  /* If the result is a value, we want to assign it to [v].
     If the result is an exception, we want to raise it instead of [v].
     The line below does both these things at once. */
  v = result.data;

  if (Caml_state->external_raise == NULL) {
    caml_terminate_signals();
    caml_fatal_uncaught_exception(v);
  }
  *Caml_state->external_raise->exn_bucket = v;

  Caml_state->local_roots = Caml_state->external_raise->local_roots;

  siglongjmp(Caml_state->external_raise->jmp->buf, 1);
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
  if (!Is_block(caml_global_data)) {
    fprintf(stderr, "Fatal error: exception %s during initialisation\n",
            exception_name);
    exit(2);
  }
}

static void check_global_data_param(char const *exception_name, char const *msg)
{
  if (!Is_block(caml_global_data)) {
    fprintf(stderr, "Fatal error: exception %s(\"%s\")\n", exception_name, msg);
    exit(2);
  }
}

Caml_inline value caml_get_failwith_tag(char const *msg)
{
  check_global_data_param("Failure", msg);
  return Field(caml_global_data, FAILURE_EXN);
}

CAMLexport value caml_exception_failure(char const *msg)
{
  return caml_exception_with_string(caml_get_failwith_tag(msg), msg);
}

CAMLexport value caml_exception_failure_value(value msg)
{
  CAMLparam1(msg);
  value tag = caml_get_failwith_tag(String_val(msg));
  CAMLreturn(caml_exception_with_arg(tag, msg));
}

Caml_inline value caml_get_invalid_argument_tag(char const *msg)
{
  check_global_data_param("Invalid_argument", msg);
  return Field(caml_global_data, INVALID_EXN);
}

CAMLexport value caml_exception_invalid_argument(char const *msg)
{
  return caml_exception_with_string(caml_get_invalid_argument_tag(msg), msg);
}

CAMLexport value caml_exception_invalid_argument_value(value msg)
{
  CAMLparam1(msg);
  value tag = caml_get_invalid_argument_tag(String_val(msg));
  CAMLreturn(caml_exception_with_arg(tag, msg));
}

CAMLexport value caml_exception_array_bound_error(void)
{
  return caml_exception_invalid_argument("index out of bounds");
}

CAMLexport value caml_exception_out_of_memory(void)
{
  check_global_data("Out_of_memory");
  return Field(caml_global_data, OUT_OF_MEMORY_EXN);
}

CAMLexport value caml_exception_stack_overflow(void)
{
  check_global_data("Stack_overflow");
  return Field(caml_global_data, STACK_OVERFLOW_EXN);
}

CAMLexport value caml_exception_sys_error(value msg)
{
  check_global_data_param("Sys_error", String_val(msg));
  return caml_exception_with_arg(Field(caml_global_data, SYS_ERROR_EXN), msg);
}

CAMLexport value caml_exception_end_of_file(void)
{
  check_global_data("End_of_file");
  return Field(caml_global_data, END_OF_FILE_EXN);
}

CAMLexport value caml_exception_zero_divide(void)
{
  check_global_data("Division_by_zero");
  return Field(caml_global_data, ZERO_DIVIDE_EXN);
}

CAMLexport value caml_exception_not_found(void)
{
  check_global_data("Not_found");
  return Field(caml_global_data, NOT_FOUND_EXN);
}

CAMLexport value caml_exception_sys_blocked_io(void)
{
  check_global_data("Sys_blocked_io");
  return Field(caml_global_data, SYS_BLOCKED_IO);
}

int caml_is_special_exception(value exn) {
  /* this function is only used in caml_format_exception to produce
     a more readable textual representation of some exceptions. It is
     better to fall back to the general, less readable representation
     than to abort with a fatal error as above. */

  value f;

  if (!Is_block(caml_global_data)) {
    return 0;
  }

  f = caml_global_data;
  return exn == Field(f, MATCH_FAILURE_EXN)
      || exn == Field(f, ASSERT_FAILURE_EXN)
      || exn == Field(f, UNDEFINED_RECURSIVE_MODULE_EXN);
}
