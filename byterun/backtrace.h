#ifndef _backtrace_
#define _backtrace_

#include "mlvalues.h"

CAMLextern int backtrace_active;
CAMLextern int backtrace_pos;
CAMLextern code_t * backtrace_buffer;
CAMLextern value backtrace_last_exn;

extern void init_backtrace(void);
extern void stash_backtrace(value exn, code_t pc, value * sp);
CAMLextern void print_exception_backtrace(void);

#endif
