#ifndef _signals_
#define _signals_

#include "misc.h"

extern value signal_handlers;
extern Volatile int pending_signal;

void enter_blocking_section P((void));
void leave_blocking_section P((void));

#endif /* _signals_ */

