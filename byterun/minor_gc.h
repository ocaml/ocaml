/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _minor_gc_
#define _minor_gc_


#include "misc.h"

extern char *young_start, *young_ptr, *young_end, *young_limit;
extern value **ref_table_ptr, **ref_table_limit;
extern asize_t minor_heap_size;
extern int in_minor_collection;

#define Is_young(val) \
  ((addr)(val) > (addr)young_start && (addr)(val) < (addr)young_end)

extern void set_minor_heap_size (asize_t);
extern void minor_collection (void);
extern void garbage_collection (void); /* for the native-code system */
extern void realloc_ref_table (void);
extern void oldify (value, value *);

#endif /* _minor_gc_ */
