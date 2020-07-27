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

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan
CAMLexport void caml_raise_constant(value tag)
{
  caml_raise(tag);
}

CAMLexport value caml_raise_with_arg_exn(value tag, value arg)
{
  CAMLparam2 (tag, arg);
  CAMLlocal1 (bucket);

  bucket = caml_alloc_small (2, 0);
  Field(bucket, 0) = tag;
  Field(bucket, 1) = arg;
  CAMLreturn(Make_exception_result(bucket));
}

CAMLexport value caml_raise_with_args_exn(value tag, int nargs, value args[])
{
  CAMLparam1 (tag);
  CAMLxparamN (args, nargs);
  value bucket;
  int i;

  CAMLassert(1 + nargs <= Max_young_wosize);
  bucket = caml_alloc_small (1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
  CAMLreturn(Make_exception_result(bucket));
}

CAMLexport value caml_raise_with_string_exn(value tag, char const *msg)
{
  CAMLparam1(tag);
  value v_msg = caml_copy_string(msg);
  CAMLreturn(caml_raise_with_arg_exn(tag, v_msg));
}

CAMLexport value caml_raise_if_exception(value val)
{
  if (Is_exception_result(val)) caml_raise(Extract_exception(val));
  return val;
}

CAMLnoreturn_start
Caml_inline void raise_encoded(value)
CAMLnoreturn_end;

Caml_inline void raise_encoded(value exn)
{
  CAMLassert(Is_exception_result(exn));
  caml_raise(Extract_exception(exn));
}

CAMLexport void caml_raise_with_arg(value tag, value arg)
{
  raise_encoded(caml_raise_with_arg_exn(tag, arg));
}

CAMLexport void caml_raise_with_args(value tag, int nargs, value arg[])
{
  raise_encoded(caml_raise_with_args_exn(tag, nargs, arg));
}

CAMLexport void caml_raise_with_string(value tag, char const * msg)
{
  raise_encoded(caml_raise_with_string_exn(tag, msg));
}

CAMLexport void caml_failwith(char const *msg)
{
  raise_encoded(caml_failwith_exn(msg));
}

CAMLexport void caml_failwith_value(value msg)
{
  raise_encoded(caml_failwith_value_exn(msg));
}

CAMLexport void caml_invalid_argument(char const *msg)
{
  raise_encoded(caml_invalid_argument_exn(msg));
}

CAMLexport void caml_invalid_argument_value(value msg)
{
  raise_encoded(caml_invalid_argument_value_exn(msg));
}

CAMLexport void caml_raise_out_of_memory(void)
{
  raise_encoded(caml_raise_out_of_memory_exn());
}

CAMLexport void caml_raise_stack_overflow(void)
{
  raise_encoded(caml_raise_stack_overflow_exn());
}

CAMLexport void caml_raise_sys_error(value msg)
{
  raise_encoded(caml_raise_sys_error_exn(msg));
}

CAMLexport void caml_raise_end_of_file(void)
{
  raise_encoded(caml_raise_end_of_file_exn());
}

CAMLexport void caml_raise_zero_divide(void)
{
  raise_encoded(caml_raise_zero_divide_exn());
}

CAMLexport void caml_raise_not_found(void)
{
  raise_encoded(caml_raise_not_found_exn());
}

CAMLexport void caml_array_bound_error(void)
{
  raise_encoded(caml_array_bound_error_exn());
}

CAMLexport void caml_raise_sys_blocked_io(void)
{
  raise_encoded(caml_raise_sys_blocked_io_exn());
}
