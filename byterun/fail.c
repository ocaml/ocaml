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

struct longjmp_buffer * external_raise = NULL;
value exn_bucket;

CAMLexport void mlraise(value v)
{
#ifdef DEBUG
  extern int volatile async_signal_mode;  /* from signals.c */
  Assert(! async_signal_mode);
#endif
  Unlock_exn();
  exn_bucket = v;
  if (external_raise == NULL) fatal_uncaught_exception(v);
  siglongjmp(external_raise->buf, 1);
}

CAMLexport void raise_constant(value tag)
{
  CAMLparam1 (tag);
  CAMLlocal1 (bucket);

  bucket = caml_alloc_small (1, 0);
  Field(bucket, 0) = tag;
  mlraise(bucket);
}

CAMLexport void raise_with_arg(value tag, value arg)
{
  CAMLparam2 (tag, arg);
  CAMLlocal1 (bucket);

  bucket = caml_alloc_small (2, 0);
  Field(bucket, 0) = tag;
  Field(bucket, 1) = arg;
  mlraise(bucket);
}

CAMLexport void raise_with_string(value tag, char *msg)
{
  CAMLparam1 (tag);
  CAMLlocal1 (vmsg);

  vmsg = caml_copy_string(msg);
  raise_with_arg(tag, vmsg);
}

CAMLexport void failwith (char *msg)
{
  raise_with_string(Field(caml_global_data, FAILURE_EXN), msg);
}

CAMLexport void invalid_argument (char *msg)
{
  raise_with_string(Field(caml_global_data, INVALID_EXN), msg);
}

CAMLexport void array_bound_error(void)
{
  invalid_argument("index out of bounds");
}

/* Problem: we can't use raise_constant, because it allocates and
   we're out of memory... Here, we allocate statically the exn bucket
   for Out_of_memory. */

static struct {
  header_t hdr;
  value exn;
} out_of_memory_bucket = { 0, 0 };

CAMLexport void raise_out_of_memory(void)
{
  if (out_of_memory_bucket.exn == 0) 
    caml_fatal_error
      ("Fatal error: out of memory while raising Out_of_memory\n");
  mlraise((value) &(out_of_memory_bucket.exn));
}

CAMLexport void raise_stack_overflow(void)
{
  raise_constant(Field(caml_global_data, STACK_OVERFLOW_EXN));
}

CAMLexport void raise_sys_error(value msg)
{
  raise_with_arg(Field(caml_global_data, SYS_ERROR_EXN), msg);
}

CAMLexport void raise_end_of_file(void)
{
  raise_constant(Field(caml_global_data, END_OF_FILE_EXN));
}

CAMLexport void raise_zero_divide(void)
{
  raise_constant(Field(caml_global_data, ZERO_DIVIDE_EXN));
}

CAMLexport void raise_not_found(void)
{
  raise_constant(Field(caml_global_data, NOT_FOUND_EXN));
}

CAMLexport void raise_sys_blocked_io(void)
{
  raise_constant(Field(caml_global_data, SYS_BLOCKED_IO));
}

/* Initialization of statically-allocated exception buckets */

void init_exceptions(void)
{
  out_of_memory_bucket.hdr = Make_header(1, 0, Caml_white);
  out_of_memory_bucket.exn = Field(caml_global_data, OUT_OF_MEMORY_EXN);
  register_global_root(&out_of_memory_bucket.exn);
}
