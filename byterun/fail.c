/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Raising exceptions from C. */

#include <stdio.h>
#include <stdlib.h>
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "gc.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "printexc.h"
#include "signals.h"
#include "stacks.h"

CAMLexport struct longjmp_buffer * caml_external_raise = NULL;
value caml_exn_bucket;

CAMLexport void caml_raise(value v)
{
  Unlock_exn();
  caml_exn_bucket = v;
  if (caml_external_raise == NULL) caml_fatal_uncaught_exception(v);
  siglongjmp(caml_external_raise->buf, 1);
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
  Init_field(bucket, 0, tag);
  Init_field(bucket, 1, arg);
  caml_raise(bucket);
  CAMLnoreturn;
}

CAMLexport void caml_raise_with_args(value tag, int nargs, value args[])
{
  CAMLparam1 (tag);
  CAMLxparamN (args, nargs);
  value bucket;
  int i;

  Assert(1 + nargs <= Max_young_wosize);
  bucket = caml_alloc_small (1 + nargs, 0);
  Init_field(bucket, 0, tag);
  for (i = 0; i < nargs; i++) Init_field(bucket, 1 + i, args[i]);
  caml_raise(bucket);
  CAMLnoreturn;
}

CAMLexport void caml_raise_with_string(value tag, char const *msg)
{
  CAMLparam1 (tag);
  CAMLlocal1 (vmsg);

  vmsg = caml_copy_string(msg);
  caml_raise_with_arg(tag, vmsg);
  CAMLnoreturn;
}

/* PR#5115: Failure and Invalid_argument can be triggered by
   input_value while reading the initial value of [caml_global_data]. */

static value get_exception(int exn, const char* exn_name) 
{
  if (caml_global_data == 0 || !Is_block(caml_read_root(caml_global_data))) {
    fprintf(stderr, "Fatal error %s during initialisation\n", exn_name);
    exit(2);
  }
  return Field(caml_read_root(caml_global_data), exn);
}

#define GET_EXCEPTION(exn) get_exception(exn, #exn)

CAMLexport void caml_failwith (char const *msg)
{
  caml_raise_with_string(GET_EXCEPTION(FAILURE_EXN), msg);
}

CAMLexport void caml_invalid_argument (char const *msg)
{
  caml_raise_with_string(GET_EXCEPTION(INVALID_EXN), msg);
}

CAMLexport void caml_array_bound_error(void)
{
  caml_invalid_argument("index out of bounds");
}

CAMLexport void caml_raise_out_of_memory(void)
{
  caml_raise_constant(GET_EXCEPTION(OUT_OF_MEMORY_EXN));
}

CAMLexport void caml_raise_stack_overflow(void)
{
  caml_raise_constant(GET_EXCEPTION(STACK_OVERFLOW_EXN));
}

CAMLexport void caml_raise_sys_error(value msg)
{
  caml_raise_with_arg(GET_EXCEPTION(SYS_ERROR_EXN), msg);
}

CAMLexport void caml_raise_end_of_file(void)
{
  caml_raise_constant(GET_EXCEPTION(END_OF_FILE_EXN));
}

CAMLexport void caml_raise_zero_divide(void)
{
  caml_raise_constant(GET_EXCEPTION(ZERO_DIVIDE_EXN));
}

CAMLexport void caml_raise_not_found(void)
{
  caml_raise_constant(GET_EXCEPTION(NOT_FOUND_EXN));
}

CAMLexport void caml_raise_sys_blocked_io(void)
{
  caml_raise_constant(GET_EXCEPTION(SYS_BLOCKED_IO));
}

int caml_is_special_exception(value exn) {
  return exn == GET_EXCEPTION(MATCH_FAILURE_EXN)
    || exn == GET_EXCEPTION(ASSERT_FAILURE_EXN)
    || exn == GET_EXCEPTION(UNDEFINED_RECURSIVE_MODULE_EXN);
}
