#ifndef _sys_
#define _sys_

#include "misc.h"

void sys_error P((char *));
void sys_init P((char **));
void sys_exit P((value)) Noreturn;

#endif /* _sys_ */
