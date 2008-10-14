/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Raising exceptions from C. */

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
  CAMLparam1 (tag);
  CAMLlocal1 (bucket);

  bucket = caml_alloc_small (1, 0);
  Field(bucket, 0) = tag;
  caml_raise(bucket);
  CAMLnoreturn;
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

  Assert(1 + nargs <= Max_young_wosize);
  bucket = caml_alloc_small (1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
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

CAMLexport void caml_failwith (char const *msg)
{
  caml_raise_with_string(Field(caml_global_data, FAILURE_EXN), msg);
}

CAMLexport void caml_invalid_argument (char const *msg)
{
  caml_raise_with_string(Field(caml_global_data, INVALID_EXN), msg);
}

CAMLexport void caml_array_bound_error(void)
{
  caml_invalid_argument("index out of bounds");
}

/* Problem: we can't use [caml_raise_constant], because it allocates and
   we're out of memory... Here, we allocate statically the exn bucket
   for [Out_of_memory]. */

static struct {
  header_t hdr;
  value exn;
} out_of_memory_bucket = { 0, 0 };

CAMLexport void caml_raise_out_of_memory(void)
{
  if (out_of_memory_bucket.exn == 0)
    caml_fatal_error
      ("Fatal error: out of memory while raising Out_of_memory\n");
  caml_raise((value) &(out_of_memory_bucket.exn));
}

CAMLexport void caml_raise_stack_overflow(void)
{
  caml_raise_constant(Field(caml_global_data, STACK_OVERFLOW_EXN));
}

CAMLexport void caml_raise_sys_error(value msg)
{
  caml_raise_with_arg(Field(caml_global_data, SYS_ERROR_EXN), msg);
}

CAMLexport void caml_raise_end_of_file(void)
{
  caml_raise_constant(Field(caml_global_data, END_OF_FILE_EXN));
}

CAMLexport void caml_raise_zero_divide(void)
{
  caml_raise_constant(Field(caml_global_data, ZERO_DIVIDE_EXN));
}

CAMLexport void caml_raise_not_found(void)
{
  caml_raise_constant(Field(caml_global_data, NOT_FOUND_EXN));
}

CAMLexport void caml_raise_sys_blocked_io(void)
{
  caml_raise_constant(Field(caml_global_data, SYS_BLOCKED_IO));
}

/* Initialization of statically-allocated exception buckets */

void caml_init_exceptions(void)
{
  out_of_memory_bucket.hdr = Make_header(1, 0, Caml_white);
  out_of_memory_bucket.exn = Field(caml_global_data, OUT_OF_MEMORY_EXN);
  caml_register_global_root(&out_of_memory_bucket.exn);
}
