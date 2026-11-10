#include <caml/fail.h>
#include <caml/mlvalues.h>

/* More arguments than any supported ABI passes in registers, so that some of
   them are passed on the stack. */
value backtrace_c_exn_stack_args(value a1, value a2, value a3, value a4,
                                 value a5, value a6, value a7, value a8,
                                 value a9, value a10, value a11, value a12)
{
  (void) a1; (void) a2; (void) a3; (void) a4; (void) a5; (void) a6;
  (void) a7; (void) a8; (void) a9; (void) a10; (void) a11; (void) a12;
  caml_raise_not_found();
}

value backtrace_c_exn_stack_args_bytecode(value *argv, int argn)
{
  return backtrace_c_exn_stack_args(argv[0], argv[1], argv[2], argv[3],
                                    argv[4], argv[5], argv[6], argv[7],
                                    argv[8], argv[9], argv[10], argv[11]);
}
