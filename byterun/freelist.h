/* Free lists of heap blocks. */

#ifndef _freelist_
#define _freelist_


#include "misc.h"
#include "mlvalues.h"

char *fl_allocate P((mlsize_t));
void fl_init_merge P((void));
char *fl_merge_block P((char *));
void fl_add_block P((char *));


#endif /* _freelist_ */
