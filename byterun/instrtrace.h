/* Trace the instructions executed */

#ifndef _instrtrace_
#define _instrtrace_


#include "mlvalues.h"
#include "misc.h"

extern int trace_flag;
extern long icount;
void stop_here P((void));
void disasm_instr P((code_t pc));


#endif
