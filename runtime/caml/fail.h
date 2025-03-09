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

#include "misc.h"
#include "mlvalues.h"

#ifdef CAML_INTERNALS
/* Built-in exceptions. In bytecode, these exceptions are the first fields in
   caml_global_data (which is loaded from the bytecode DATA section) - see
   bytecomp/bytelink.ml. In native code, these exceptions are created if
   needed in the startup object - see asmcomp/asmlink.ml and
   Cmm_helpers.predef_exception. */
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

struct caml_exception_context {
  struct longjmp_buffer* jmp;
  struct caml__roots_block* local_roots;
  volatile value* exn_bucket;
};

/* Global variables moved to Caml_state in 4.10 */
#define caml_external_raise (Caml_state_field(external_raise))
#define caml_exn_bucket (Caml_state_field(exn_bucket))

int caml_is_special_exception(value exn);

/* from runtime/sync.c */
CAMLextern void caml_check_error(int err, char const * msg);

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
extern "C" {
#endif

/* The following functions raise immediately into OCaml.

   The argument [exn_constr] can be obtained using [caml_named_value]
   from caml/callback.h after registering and naming an exception from
   OCaml using [Callback.register_exception].
*/
CAMLnoret CAMLextern void caml_raise (value exception);
CAMLnoret CAMLextern void caml_raise_constant (value exn_constr);
CAMLnoret CAMLextern void caml_raise_with_arg (value exn_constr, value arg);
CAMLnoret CAMLextern void caml_raise_with_args (value exn_constr,
                                                int nargs, value arg[]);
CAMLnoret CAMLextern void caml_raise_with_string (value exn_constr,
                                                  char const * msg);
CAMLnoret CAMLextern void caml_failwith (char const *msg);
CAMLnoret CAMLextern void caml_failwith_value (value msg);
CAMLnoret CAMLextern void caml_invalid_argument (char const *msg);
CAMLnoret CAMLextern void caml_invalid_argument_value (value msg);
CAMLnoret CAMLextern void caml_raise_out_of_memory (void);
CAMLnoret CAMLextern void caml_raise_stack_overflow (void);
CAMLnoret CAMLextern void caml_raise_sys_error (value);
CAMLnoret CAMLextern void caml_raise_end_of_file (void);
CAMLnoret CAMLextern void caml_raise_zero_divide (void);
CAMLnoret CAMLextern void caml_raise_not_found (void);
CAMLnoret CAMLextern void caml_array_bound_error (void);
CAMLnoret CAMLextern void caml_raise_sys_blocked_io (void);

/* Non-raising variants of the above functions. The exception is
   returned as a normal value, which can be raised with [caml_raise],
   or returned as a value of type [caml_result] using
   [Result_exception], typically to allow resource clean-up before
   raising the exception. */
CAMLextern value caml_exception_constant (value exn_constr);
CAMLextern value caml_exception_with_arg (value exn_constr, value arg);
CAMLextern value caml_exception_with_args (value exn_constr,
                                           int nargs, value arg[]);
CAMLextern value caml_exception_with_string (value exn_constr,
                                             char const * msg);
CAMLextern value caml_exception_failure (char const *msg);
CAMLextern value caml_exception_failure_value (value msg);
CAMLextern value caml_exception_invalid_argument (char const *msg);
CAMLextern value caml_exception_invalid_argument_value (value msg);
CAMLextern value caml_exception_out_of_memory (void);
CAMLextern value caml_exception_stack_overflow (void);
CAMLextern value caml_exception_sys_error (value msg);
CAMLextern value caml_exception_end_of_file (void);
CAMLextern value caml_exception_zero_divide (void);
CAMLextern value caml_exception_not_found (void);
CAMLextern value caml_exception_array_bound_error (void);
CAMLextern value caml_exception_sys_blocked_io (void);

/* Returns the value of a [caml_result] or raises the exception.
   This function replaced [caml_raise_if_exception] in 5.3. */
Caml_inline value caml_get_value_or_raise (struct caml_result_private result)
{
  if (result.is_exception)
    caml_raise(result.data);
  else
    return result.data;
}

#ifdef CAML_INTERNALS
/* internals only, provided for backward-compatibility */
Caml_inline value caml_result_get_encoded_exception(
  struct caml_result_private result)
{
  if (result.is_exception)
    return Make_exception_result(result.data);
  else
    return result.data;
}
#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_FAIL_H */
