/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Raising exceptions from C. */

#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"

struct longjmp_buffer * external_raise;
value exn_bucket;

void mlraise(value v)
{
  Assert(! async_signal_mode);
  Unlock_exn();
  exn_bucket = v;
  siglongjmp(external_raise->buf, 1);
}

void raise_constant(value tag)
{
  value bucket;
  Begin_root (tag);
    bucket = alloc (1, 0);
    Field(bucket, 0) = tag;
  End_roots ();
  mlraise(bucket);
}

void raise_with_arg(value tag, value arg)
{
  value bucket;
  Begin_roots2 (tag, arg);
    bucket = alloc (2, 0);
    Field(bucket, 0) = tag;
    Field(bucket, 1) = arg;
  End_roots ();
  mlraise(bucket);
}

void raise_with_string(value tag, char *msg)
{
  value vmsg;
  Begin_root(tag);
    vmsg = copy_string(msg);
  End_roots();
  raise_with_arg(tag, vmsg);
}

void failwith (char *msg)
{
  raise_with_string(Field(global_data, FAILURE_EXN), msg);
}

void invalid_argument (char *msg)
{
  raise_with_string(Field(global_data, INVALID_EXN), msg);
}

/* Problem: we can't use raise_constant, because it allocates and
   we're out of memory... The following is a terrible hack that works
   because global_data[OUT_OF_MEMORY_EXN] is in the old generation
   (because global_data was read with intern_val), hence stays at
   a fixed address */

static struct {
  header_t hdr;
  value exn;
} out_of_memory_bucket;

void raise_out_of_memory(void)
{
  out_of_memory_bucket.hdr = Make_header(1, 0, White);
  out_of_memory_bucket.exn = Field(global_data, OUT_OF_MEMORY_EXN);
  mlraise((value) &(out_of_memory_bucket.exn));
}

void raise_stack_overflow(void)
{
  raise_constant(Field(global_data, STACK_OVERFLOW_EXN));
}

void raise_sys_error(value msg)
{
  raise_with_arg(Field(global_data, SYS_ERROR_EXN), msg);
}

void raise_end_of_file(void)
{
  raise_constant(Field(global_data, END_OF_FILE_EXN));
}

void raise_zero_divide(void)
{
  raise_constant(Field(global_data, ZERO_DIVIDE_EXN));
}

void raise_not_found(void)
{
  raise_constant(Field(global_data, NOT_FOUND_EXN));
}

