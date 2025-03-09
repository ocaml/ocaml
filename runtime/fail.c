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

#define Assert_is_exn_constructor(v)                                    \
  (CAMLassert(Is_block(v)), CAMLassert(Tag_val(v) == Object_tag))

CAMLexport value caml_exception_constant(value exn_constr)
{
  Assert_is_exn_constructor(exn_constr);
  return exn_constr;
}

CAMLexport value caml_exception_with_arg(value exn_constr, value arg)
{
  CAMLparam2 (exn_constr, arg);
  CAMLlocal1 (bucket);
  Assert_is_exn_constructor(exn_constr);

  bucket = caml_alloc_small (2, 0);
  Field(bucket, 0) = exn_constr;
  Field(bucket, 1) = arg;
  CAMLreturn(bucket);
}

CAMLexport value caml_exception_with_args(value exn_constr,
                                          int nargs, value args[])
{
  CAMLparam1 (exn_constr);
  CAMLxparamN (args, nargs);
  Assert_is_exn_constructor(exn_constr);

  value bucket;

  CAMLassert(1 + nargs <= Max_young_wosize);
  bucket = caml_alloc_small (1 + nargs, 0);
  Field(bucket, 0) = exn_constr;
  for (int i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
  CAMLreturn(bucket);
}

CAMLexport value caml_exception_with_string(value exn_constr, char const *msg)
{
  CAMLparam1(exn_constr);
  value v_msg = caml_copy_string(msg);
  CAMLreturn(caml_exception_with_arg(exn_constr, v_msg));
}


/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan
CAMLexport void caml_raise_constant(value exn_constr)
{
  caml_raise(caml_exception_constant(exn_constr));
}

CAMLexport void caml_raise_with_arg(value exn_constr, value arg)
{
  caml_raise(caml_exception_with_arg(exn_constr, arg));
}

CAMLexport void caml_raise_with_args(value exn_constr, int nargs, value arg[])
{
  caml_raise(caml_exception_with_args(exn_constr, nargs, arg));
}

CAMLexport void caml_raise_with_string(value exn_constr, char const * msg)
{
  caml_raise(caml_exception_with_string(exn_constr, msg));
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
