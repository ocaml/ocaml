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

/* Allocation macros and functions */

#ifndef _memory_
#define _memory_


#include "config.h"
#include "gc.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"

value alloc_shr P((mlsize_t, tag_t));
void adjust_gc_speed P((mlsize_t, mlsize_t));
void modify P((value *, value));
void initialize P((value *, value));
value check_urgent_gc P((value));
char * stat_alloc P((asize_t));              /* Size in bytes. */
void stat_free P((char *));
char * stat_resize P((char *, asize_t));     /* Size in bytes. */

/* void shrink_heap P((char *));       Only used in compact.c */

#ifdef NATIVE_CODE
#define Garbage_collection_function garbage_collection
#else
#define Garbage_collection_function minor_collection
#endif

#define Alloc_small(result, wosize, tag) {            Assert (wosize >= 1); \
  young_ptr -= Bhsize_wosize (wosize);                                      \
  if (young_ptr < young_limit){                                             \
    Setup_for_gc;                                                           \
    Garbage_collection_function ();                                         \
    Restore_after_gc;                                                       \
    young_ptr -= Bhsize_wosize (wosize);                                    \
  }                                                                         \
  Hd_hp (young_ptr) = Make_header ((wosize), (tag), Black);                 \
  (result) = Val_hp (young_ptr);                                            \
}

/* You must use [Modify] to change a field of an existing shared block,
   unless you are sure the value being overwritten is not a shared block and
   the value being written is not a young block. */
/* [Modify] never calls the GC. */

#define Modify(fp, val) {                                                   \
  value _old_ = *(fp);                                                      \
  *(fp) = (val);                                                            \
  if (Is_in_heap (fp)){                                                     \
    if (gc_phase == Phase_mark) darken (_old_, NULL);                       \
    if (Is_block (val) && Is_young (val)                                    \
        && ! (Is_block (_old_) && Is_young (_old_))){                       \
      *ref_table_ptr++ = (fp);                                              \
      if (ref_table_ptr >= ref_table_limit){                                \
        Assert (ref_table_ptr == ref_table_limit);                          \
        realloc_ref_table ();                                               \
      }                                                                     \
    }                                                                       \
  }                                                                         \
}                                                                           \

/*
   [Begin_roots] and [End_roots] are used for C variables that are GC roots.
   It must contain all values in C local variables and function parameters
   at the time the minor GC is called.
   Usage:
   After initialising your local variables to legal Caml values, but before
   calling allocation functions, insert [Begin_roots_n(v1, ... vn)], where
   v1 ... vn are your variables of type [value] that you want to be updated
   across allocations.
   At the end, insert [End_roots()].

   Note that [Begin_roots] opens a new block, and [End_roots] closes it.
   Thus they must occur in matching pairs at the same brace nesting level.

   You can use [Val_unit] as a dummy initial value for your variables.
*/


struct caml__roots_block {
  struct caml__roots_block *next;
  long ntables;
  long nitems;
  value *tables [5];
};

extern struct caml__roots_block *local_roots_new;  /* defined in roots.h */

#define Begin_root Begin_roots1

#define Begin_roots1(r0) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots_new; \
  local_roots_new = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = &(r0);

#define Begin_roots2(r0, r1) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots_new; \
  local_roots_new = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 2; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1);

#define Begin_roots3(r0, r1, r2) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots_new; \
  local_roots_new = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 3; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2);

#define Begin_roots4(r0, r1, r2, r3) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots_new; \
  local_roots_new = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 4; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3);

#define Begin_roots5(r0, r1, r2, r3, r4) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots_new; \
  local_roots_new = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 5; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3); \
  caml__roots_block.tables[4] = &(r4);

#define Begin_roots_block(table, size) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots_new; \
  local_roots_new = &caml__roots_block; \
  caml__roots_block.nitems = (size); \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = (table);

#define End_roots() local_roots_new = caml__roots_block.next; }


/*
   [Push_roots] and [Pop_roots] are obsolete.
   Use [Begin_roots] and [End_roots] instead.
*/

/* [Push_roots] and [Pop_roots] are used for C variables that are GC roots.
 * It must contain all values in C local variables at the time the minor GC is
 * called.
 * Usage:
 * At the end of the declarations of your C local variables, add
 * [ Push_roots (variable_name, size); ]
 * The size is the number of declared roots.  They are accessed as
 * [ variable_name [0] ... variable_name [size - 1] ].
 * The [variable_name] and the [size] must not be [ _ ].
 * Just before the function return, add a call to [Pop_roots].
 */

extern value *local_roots;

#define Push_roots(name, size)                                              \
   value name [(size) + 2];                                                 \
   { long _; for (_ = 0; _ < (size); name [_++] = Val_long (0)); }          \
   name [(size)] = (value) (size);                                          \
   name [(size) + 1] = (value) local_roots;                                 \
   local_roots = &(name [(size)]);

#define Pop_roots() {local_roots = (value *) local_roots [1]; }

/* [register_global_root] registers a global C variable as a memory root
   for the duration of the program, or until [remove_global_root] is
   called. */

void register_global_root P((value *));

/* [remove_global_root] removes a memory root registered on a global C
   variable with [register_global_root]. */

void remove_global_root P((value *));


#endif /* _memory_ */

