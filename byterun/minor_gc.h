#ifndef _minor_gc_
#define _minor_gc_


#include "misc.h"

extern char *young_start, *young_ptr, *young_end;
extern value **ref_table_ptr, **ref_table_limit;
extern asize_t minor_heap_size;

#define Is_young(val) \
  ((addr)(val) > (addr)young_start && (addr)(val) < (addr)young_end)

extern void set_minor_heap_size P((asize_t));
extern void minor_collection P((void));
extern void realloc_ref_table P((void));
extern void oldify P((value *, value));

#endif /* _minor_gc_ */
