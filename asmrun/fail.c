/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Raising exceptions from C. */

#include <signal.h>
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "signals.h"
#include "stack.h"
#include "roots.h"

/* The globals holding predefined exceptions */

typedef value caml_generated_constant[1];

extern caml_generated_constant Out_of_memory, Sys_error, Failure,
  Invalid_argument, End_of_file, Division_by_zero, Not_found,
  Match_failure, Sys_blocked_io;

/* Resetting the signal mask when raising an exception from C */

static void default_reset_sigmask(void)
{
#ifdef POSIX_SIGNALS
  sigset_t mask;
  sigemptyset(&mask);
  sigprocmask(SIG_SETMASK, &mask, NULL);
#else
#ifdef HAS_SIGSETMASK
  sigsetmask(0);
#endif
#endif
}

void (*caml_reset_sigmask)(void) = default_reset_sigmask;

/* Exception raising */

extern void raise_caml_exception (value bucket) Noreturn;

char * caml_exception_pointer = NULL;

void mlraise(value v)
{
  (*caml_reset_sigmask)();
  Unlock_exn();
  if (caml_exception_pointer == NULL) fatal_uncaught_exception(v);

#ifndef Stack_grows_upwards
#define PUSHED_AFTER <
#else
#define PUSHED_AFTER >
#endif
  while (local_roots != NULL && 
         (char *) local_roots PUSHED_AFTER caml_exception_pointer) {
    local_roots = local_roots->next;
  }
#undef PUSHED_AFTER

  raise_caml_exception(v);
}

void raise_constant(value tag)
{
  value bucket;
  Begin_root (tag);
    bucket = alloc_small (1, 0);
    Field(bucket, 0) = tag;
  End_roots ();
  mlraise(bucket);
}

void raise_with_arg(value tag, value arg)
{
  value bucket;
  Begin_roots2 (tag, arg);
    bucket = alloc_small (2, 0);
    Field(bucket, 0) = tag;
    Field(bucket, 1) = arg;
  End_roots ();
  mlraise(bucket);
}

void raise_with_string(value tag, char *msg)
{
  raise_with_arg(tag, copy_string(msg));
}

void failwith (char *msg)
{
  raise_with_string((value) Failure, msg);
}

void invalid_argument (char *msg)
{
  raise_with_string((value) Invalid_argument, msg);
}

/* To raise Out_of_memory, we can't use raise_constant,
   because it allocates and we're out of memory...
   We therefore build the bucket by hand.
   This works OK because the exception value for Out_of_memory is also
   statically allocated out of the heap. */

static struct {
  header_t hdr;
  value exn;
} out_of_memory_bucket;

void raise_out_of_memory(void)
{
  out_of_memory_bucket.hdr = Make_header(1, 0, White);
  out_of_memory_bucket.exn = (value) Out_of_memory;
  mlraise((value) &(out_of_memory_bucket.exn));
}

void raise_sys_error(value msg)
{
  raise_with_arg((value) Sys_error, msg);
}

void raise_end_of_file(void)
{
  raise_constant((value) End_of_file);
}

void raise_zero_divide(void)
{
  raise_constant((value) Division_by_zero);
}

void raise_not_found(void)
{
  raise_constant((value) Not_found);
}

void raise_sys_blocked_io(void)
{
  raise_constant((value) Sys_blocked_io));
}

/* We allocate statically the bucket for the exception because we can't
   do a GC before the exception is raised (lack of stack descriptors
   for the ccall to array_bound_error  */

#define BOUND_MSG "out-of-bound array or string access"
#define BOUND_MSG_LEN (sizeof(BOUND_MSG) - 1)

static struct {
  header_t hdr;
  value exn;
  value arg;
} array_bound_error_bucket;

static struct {
  header_t hdr;
  char data[BOUND_MSG_LEN + sizeof(value)];
} array_bound_error_msg = { 0, BOUND_MSG };

void array_bound_error(void)
{
  mlsize_t wosize = (BOUND_MSG_LEN + sizeof(value)) / sizeof(value);
  mlsize_t offset_index = Bsize_wsize(wosize) - 1;
  array_bound_error_msg.hdr = Make_header(wosize, String_tag, White);
  array_bound_error_msg.data[offset_index] = offset_index - BOUND_MSG_LEN;
  array_bound_error_bucket.hdr = Make_header(2, 0, White);
  array_bound_error_bucket.exn = (value) Invalid_argument;
  array_bound_error_bucket.arg = (value) array_bound_error_msg.data;
  mlraise((value) &array_bound_error_bucket.exn);
}
