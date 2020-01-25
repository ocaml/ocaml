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

#define Assert_is_tag(tag)                                  \
  (CAMLassert(Is_block(tag) && Tag_val(tag) == Object_tag))

CAMLexport value caml_exception_constant(value tag)
{
  Assert_is_tag(tag);
  return tag;
}

CAMLexport value caml_exception_with_arg(value tag, value arg)
{
  CAMLparam2 (tag, arg);
  CAMLlocal1 (bucket);
  Assert_is_tag(tag);

  bucket = caml_alloc_small (2, 0);
  Field(bucket, 0) = tag;
  Field(bucket, 1) = arg;
  CAMLreturn(bucket);
}

CAMLexport value caml_exception_with_args(value tag, int nargs, value args[])
{
  CAMLparam1 (tag);
  CAMLxparamN (args, nargs);
  Assert_is_tag(tag);

  value bucket;
  int i;

  CAMLassert(1 + nargs <= Max_young_wosize);
  bucket = caml_alloc_small (1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
  CAMLreturn(bucket);
}

CAMLexport value caml_exception_with_string(value tag, char const *msg)
{
  CAMLparam1(tag);
  value v_msg = caml_copy_string(msg);
  CAMLreturn(caml_exception_with_arg(tag, v_msg));
}


/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan
CAMLexport void caml_raise_constant(value tag)
{
  caml_raise(caml_exception_constant(tag));
}

CAMLexport void caml_raise_with_arg(value tag, value arg)
{
  caml_raise(caml_exception_with_arg(tag, arg));
}

CAMLexport void caml_raise_with_args(value tag, int nargs, value arg[])
{
  caml_raise(caml_exception_with_args(tag, nargs, arg));
}

CAMLexport void caml_raise_with_string(value tag, char const * msg)
{
  caml_raise(caml_exception_with_string(tag, msg));
}

CAMLexport void caml_failwith(char const *msg)
{
  caml_raise(caml_exception_failure(msg));
}

CAMLexport void caml_failwith_value(value msg)
{
  caml_raise(caml_exception_failure_value(msg));
}

CAMLexport void caml_invalid_argument(char const *msg)
{
  caml_raise(caml_exception_invalid_argument(msg));
}

CAMLexport void caml_invalid_argument_value(value msg)
{
  caml_raise(caml_exception_invalid_argument_value(msg));
}

CAMLexport void caml_raise_out_of_memory(void)
{
  caml_raise(caml_exception_out_of_memory());
}

CAMLexport void caml_raise_stack_overflow(void)
{
  caml_raise(caml_exception_stack_overflow());
}

CAMLexport void caml_raise_sys_error(value msg)
{
  caml_raise(caml_exception_sys_error(msg));
}

CAMLexport void caml_raise_end_of_file(void)
{
  caml_raise(caml_exception_end_of_file());
}

CAMLexport void caml_raise_zero_divide(void)
{
  caml_raise(caml_exception_zero_divide());
}

CAMLexport void caml_raise_not_found(void)
{
  caml_raise(caml_exception_not_found());
}

CAMLexport void caml_array_bound_error(void)
{
  caml_raise(caml_exception_array_bound_error());
}

CAMLexport void caml_raise_sys_blocked_io(void)
{
  caml_raise(caml_exception_sys_blocked_io());
}

CAMLexport value caml_raise_if_exception(value val)
{
  if (Is_exception_result(val)) caml_raise(Extract_exception(val));
  return val;
}
