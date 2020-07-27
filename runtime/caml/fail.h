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

#ifndef CAML_FAIL_H
#define CAML_FAIL_H

#ifdef CAML_INTERNALS
#include <setjmp.h>
#endif /* CAML_INTERNALS */

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

#ifdef CAML_INTERNALS
#define OUT_OF_MEMORY_EXN 0     /* "Out_of_memory" */
#define SYS_ERROR_EXN 1         /* "Sys_error" */
#define FAILURE_EXN 2           /* "Failure" */
#define INVALID_EXN 3           /* "Invalid_argument" */
#define END_OF_FILE_EXN 4       /* "End_of_file" */
#define ZERO_DIVIDE_EXN 5       /* "Division_by_zero" */
#define NOT_FOUND_EXN 6         /* "Not_found" */
#define MATCH_FAILURE_EXN 7     /* "Match_failure" */
#define STACK_OVERFLOW_EXN 8    /* "Stack_overflow" */
#define SYS_BLOCKED_IO 9        /* "Sys_blocked_io" */
#define ASSERT_FAILURE_EXN 10   /* "Assert_failure" */
#define UNDEFINED_RECURSIVE_MODULE_EXN 11 /* "Undefined_recursive_module" */

#ifdef POSIX_SIGNALS
struct longjmp_buffer {
  sigjmp_buf buf;
};
#elif defined(__MINGW64__) && defined(__GNUC__) && __GNUC__ >= 4
/* MPR#7638: issues with setjmp/longjmp in Mingw64, use GCC builtins instead */
struct longjmp_buffer {
  intptr_t buf[5];
};
#define sigsetjmp(buf,save) __builtin_setjmp(buf)
#define siglongjmp(buf,val) __builtin_longjmp(buf,val)
#else
struct longjmp_buffer {
  jmp_buf buf;
};
#define sigsetjmp(buf,save) setjmp(buf)
#define siglongjmp(buf,val) longjmp(buf,val)
#endif

/* Global variables moved to Caml_state in 4.10 */
#define caml_external_raise (Caml_state_field(external_raise))
#define caml_exn_bucket (Caml_state_field(exn_bucket))

int caml_is_special_exception(value exn);

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
extern "C" {
#endif

CAMLnoreturn_start
CAMLextern void caml_raise (value bucket)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_constant (value tag)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_with_arg (value tag, value arg)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_with_args (value tag, int nargs, value arg[])
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_with_string (value tag, char const * msg)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_failwith (char const *msg)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_failwith_value (value msg)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_invalid_argument (char const *msg)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_invalid_argument_value (value msg)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_out_of_memory (void)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_stack_overflow (void)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_sys_error (value)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_end_of_file (void)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_zero_divide (void)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_not_found (void)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_array_bound_error (void)
CAMLnoreturn_end; // raises

CAMLnoreturn_start
CAMLextern void caml_raise_sys_blocked_io (void)
CAMLnoreturn_end; // raises

/* Non-raising variants of the above functions. The result is an
   encoded exception to use with Is_exception_result and
   Extract_exception. */

CAMLextern value caml_raise_with_arg_exn (value tag, value arg);
CAMLextern value caml_raise_with_args_exn (value tag, int nargs, value arg[]);
CAMLextern value caml_raise_with_string_exn (value tag, char const * msg);
CAMLextern value caml_failwith_exn (char const *msg);
CAMLextern value caml_failwith_value_exn (value msg);
CAMLextern value caml_invalid_argument_exn (char const *msg);
CAMLextern value caml_invalid_argument_value_exn (value msg);
CAMLextern value caml_raise_out_of_memory_exn (void);
CAMLextern value caml_raise_stack_overflow_exn (void);
CAMLextern value caml_raise_sys_error_exn (value msg);
CAMLextern value caml_raise_end_of_file_exn (void);
CAMLextern value caml_raise_zero_divide_exn (void);
CAMLextern value caml_raise_not_found_exn (void);
CAMLextern value caml_array_bound_error_exn (void);
CAMLextern value caml_raise_sys_blocked_io_exn (void);

CAMLextern value caml_raise_if_exception (value val);
/* If [val] is an encoded exception as returned by _exn functions,
   raise the exception it encodes. Otherwise, [val] is returned
   unchanged with the guarantee that it is a regular value. Raises. */

#ifdef __cplusplus
}
#endif

#endif /* CAML_FAIL_H */
