/* Translate a block of bytecode (endianness switch, threading). */

#ifndef _fix_code_
#define _fix_code_


#include "misc.h"
#include "mlvalues.h"

void fixup_endianness P((code_t code, asize_t len));
void thread_code P((code_t code, asize_t len, void * instr_table[]));


#endif

