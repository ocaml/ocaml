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

#ifndef _fail_
#define _fail_


#include <setjmp.h>
#include "misc.h"
#include "mlvalues.h"

#define OUT_OF_MEMORY_EXN 0     /* "Out_of_memory" */
#define SYS_ERROR_EXN 1         /* "Sys_error" */
#define FAILURE_EXN 2           /* "Failure" */
#define INVALID_EXN 3           /* "Invalid_argument" */
#define END_OF_FILE_EXN 4       /* "End_of_file" */
#define ZERO_DIVIDE_EXN 5       /* "Division_by_zero" */
#define NOT_FOUND_EXN 6         /* "Not_found" */
#define MATCH_FAILURE_EXN 7     /* "Match_failure" */
#define STACK_OVERFLOW_EXN 8    /* "Stack_overflow" */

#ifdef POSIX_SIGNALS
struct longjmp_buffer {
  sigjmp_buf buf;
};
#else
struct longjmp_buffer {
  jmp_buf buf;
};
#define sigsetjmp(buf,save) setjmp(buf)
#define siglongjmp(buf,val) longjmp(buf,val)
#endif

extern struct longjmp_buffer * external_raise;
extern value exn_bucket;

void mlraise (value bucket) Noreturn;
void raise_constant (value tag) Noreturn;
void raise_with_arg (value tag, value arg) Noreturn;
void raise_with_string (value tag, char * msg) Noreturn;
void failwith (char *) Noreturn;
void invalid_argument (char *) Noreturn;
void raise_out_of_memory (void) Noreturn;
void raise_stack_overflow (void) Noreturn;
void raise_sys_error (value) Noreturn;
void raise_end_of_file (void) Noreturn;
void raise_zero_divide (void) Noreturn;
void raise_not_found (void) Noreturn;
void fatal_uncaught_exception (value) Noreturn;
void init_exceptions (void);
void array_bound_error (void);

extern void (*caml_reset_sigmask)(void);

#endif /* _fail_ */
