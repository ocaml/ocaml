#ifndef _backtrace_
#define _backtrace_

#include "mlvalues.h"

extern int backtrace_active;
extern int backtrace_pos;
extern code_t * backtrace_buffer;

extern void stash_backtrace(code_t pc, value * sp);
extern void print_exception_backtrace(void);

#endif
