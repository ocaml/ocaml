/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef CAML_MINOR_GC_H
#define CAML_MINOR_GC_H


#include "misc.h"

CAMLextern char *young_start, *young_ptr, *young_end, *young_limit;
CAMLextern value **ref_table_ptr, **ref_table_limit;
extern asize_t minor_heap_size;
extern int in_minor_collection;

#define Is_young(val) \
  (Assert (Is_block (val)), \
   (addr)(val) < (addr)young_end && (addr)(val) > (addr)young_start)

extern void set_minor_heap_size (asize_t);
extern void empty_minor_heap (void);
CAMLextern void minor_collection (void);
CAMLextern void garbage_collection (void); /* for the native-code system */
extern void realloc_ref_table (void);
extern void oldify_one (value, value *);
extern void oldify_mopup (void);

#define Oldify(p) do{ \
    value __oldify__v__ = *p; \
    if (Is_block (__oldify__v__) && Is_young (__oldify__v__)){ \
      oldify_one (__oldify__v__, (p)); \
    } \
  }while(0)

#endif /* CAML_MINOR_GC_H */
