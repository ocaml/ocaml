/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Raising exceptions from C. */

#include <signal.h>
#include "alloc.h"
#include "fail.h"
#include "gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"
#include "stack.h"

/* The globals holding predefined exceptions */

typedef char caml_generated_constant[256];
/* We claim these constants are big so that e.g. the Mips compiler
   will not assume that they are in the .sdata section */

extern caml_generated_constant Out_of_memory, Sys_error, Failure,
  Invalid_argument, End_of_file, Division_by_zero, Not_found, Match_failure;

/* Exception raising */

extern void raise_caml_exception P((value bucket)) Noreturn;

extern char * caml_exception_pointer;

void mlraise(v)
     value v;
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
  leave_blocking_section();
#ifndef Stack_grows_upwards
  while (local_roots != NULL && 
         (char *) local_roots < caml_exception_pointer) {
#else
  while (local_roots != NULL && 
         (char *) local_roots > caml_exception_pointer) {
#endif
    local_roots = (value *) local_roots[1];
  }
  raise_caml_exception(v);
}

void raise_constant(tag)
     value tag;
{
  value bucket;
  Push_roots (a, 1);
  a[0] = tag;
  bucket = alloc (1, 0);
  Field(bucket, 0) = a[0];
  Pop_roots ();
  mlraise(bucket);
}

void raise_with_arg(tag, arg)
     value tag;
     value arg;
{
  value bucket;
  Push_roots (a, 2);
  a[0] = tag;
  a[1] = arg;
  bucket = alloc (2, 0);
  Field(bucket, 0) = a[0];
  Field(bucket, 1) = a[1];
  Pop_roots ();
  mlraise(bucket);
}

void raise_with_string(tag, msg)
     value tag;
     char * msg;
{
  raise_with_arg(tag, copy_string(msg));
}

void failwith (msg)
     char * msg;
{
  raise_with_string((value) Failure, msg);
}

/* We chose to abort the program if a C primitive raises Invalid_argument.
   Rationale: nobody should trap Invalid_argument, and we're not running
   under a toplevel, so this will provide the same feedback to the user.
   Moreover, divisions by zero or out-of-bounds accesses also abort the
   program, and there's no way we can turn them into exceptions.
   Finally, this allows a number of C primitives to be declared "noalloc",
   and this makes calling them much more efficient. */
   
void invalid_argument (msg)
     char * msg;
{
  fatal_error_arg("Fatal_error: Invalid_argument \"%s\"\n", msg);
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

void raise_out_of_memory()
{
  out_of_memory_bucket.hdr = Make_header(1, 0, White);
  out_of_memory_bucket.exn = (value) Out_of_memory;
  mlraise((value) &(out_of_memory_bucket.exn));
}

void raise_sys_error(msg)
     value msg;
{
  raise_with_arg((value) Sys_error, msg);
}

void raise_end_of_file()
{
  raise_constant((value) End_of_file);
}

void raise_zero_divide()
{
  raise_constant((value) Division_by_zero);
}

void raise_not_found()
{
  raise_constant((value) Not_found);
}

