/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
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

value alloc_shr (mlsize_t, tag_t);
void adjust_gc_speed (mlsize_t, mlsize_t);
void modify (value *, value);
void initialize (value *, value);
value check_urgent_gc (value);
void * stat_alloc (asize_t);              /* Size in bytes. */
void stat_free (void *);
void * stat_resize (void *, asize_t);     /* Size in bytes. */
header_t *alloc_for_heap (asize_t request);   /* Size in bytes. */
void free_for_heap (header_t *mem);
int add_to_heap (header_t *mem);
color_t allocation_color (void *hp);

/* void shrink_heap (char *);        Only used in compact.c */

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


typedef struct caml__roots_block {
  struct caml__roots_block *next;
  long ntables;
  long nitems;
  value *tables [5];
} caml__roots_block;

extern struct caml__roots_block *local_roots;  /* defined in roots.c */

/* The following macros are used to declare C local variables and
   function parameters of type [value].

   The function body must start with one of the [CAMLparam] macros.
   If the function has no parameter of type [value], use [CAMLparam0].
   If the function has 1 to 5 [value] parameters, use the corresponding
   [CAMLparam] with the parameters as arguments.
   If the function has more than 5 [value] parameters, use [CAMLparam5]
   for the first 5 parameters, and one or more calls to the [CAMLxparam]
   macros for the others.

   If you need local variables of type [value], declare them with one
   or more calls to the [CAMLlocal] macros.
   Use [CAMLlocalN] to declare an array of [value]s.

   Your function may raise and exception or return a [value] with the
   [CAMLreturn] macro.  Its argument is simply the [value] returned by
   your function.  Do NOT directly return a [value] with the [return]
   keyword.

   All the identifiers beginning with "caml__" are reserved by Caml.
   Do not use them for anything (local or global variables, struct or
   union tags, macros, etc.)
*/

#define CAMLparam0() \
  caml__roots_block *caml__frame = local_roots

#define CAMLparam1(x) \
  CAMLparam0 (); \
  CAMLxparam1 (x)

#define CAMLparam2(x, y) \
  CAMLparam0 (); \
  CAMLxparam2 (x, y)

#define CAMLparam3(x, y, z) \
  CAMLparam0 (); \
  CAMLxparam3 (x, y, z)

#define CAMLparam4(x, y, z, t) \
  CAMLparam0 (); \
  CAMLxparam4 (x, y, z, t)

#define CAMLparam5(x, y, z, t, u) \
  CAMLparam0 (); \
  CAMLxparam5 (x, y, z, t, u)

#define CAMLxparam1(x) \
  caml__roots_block caml__roots_##x; \
  void *caml__dummy_##x = ( \
    caml__frame, \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 1), \
    (caml__roots_##x.tables [0] = &x), \
    NULL)

#define CAMLxparam2(x, y) \
  caml__roots_block caml__roots_##x; \
  void *caml__dummy_##x = ( \
    caml__frame, \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 2), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    NULL)

#define CAMLxparam3(x, y, z) \
  caml__roots_block caml__roots_##x; \
  void *caml__dummy_##x = ( \
    caml__frame, \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 3), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    (caml__roots_##x.tables [2] = &z), \
    NULL)

#define CAMLxparam4(x, y, z, t) \
  caml__roots_block caml__roots_##x; \
  void *caml__dummy_##x = ( \
    caml__frame, \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 4), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    (caml__roots_##x.tables [2] = &z), \
    (caml__roots_##x.tables [3] = &t), \
    NULL)

#define CAMLxparam5(x, y, z, t, u) \
  caml__roots_block caml__roots_##x; \
  void *caml__dummy_##x = ( \
    caml__frame, \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 5), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    (caml__roots_##x.tables [2] = &z), \
    (caml__roots_##x.tables [3] = &t), \
    (caml__roots_##x.tables [4] = &u), \
    NULL)

#define CAMLlocal1(x) \
  value x = 0; \
  CAMLxparam1 (x)

#define CAMLlocal2(x, y) \
  value x = 0, y = 0; \
  CAMLxparam2 (x, y)

#define CAMLlocal3(x, y, z) \
  value x = 0, y = 0, z = 0; \
  CAMLxparam3 (x, y, z)

#define CAMLlocal4(x, y, z, t) \
  value x = 0, y = 0, z = 0, t = 0; \
  CAMLxparam4 (x, y, z, t)

#define CAMLlocal5(x, y, z, t, u) \
  value x = 0, y = 0, z = 0, t = 0, u = 0; \
  CAMLxparam5 (x, y, z, t, u)

#define CAMLlocalN(x, size) \
  value x [(size)] = { 0 /* , 0, 0, ... */ }; \
  caml__roots_block caml__roots_##x; \
  void *caml__dummy_##x = ( \
  caml_frame, \
  (caml_roots_##x.next = local_roots), \
  (local_roots = &caml__roots_##x), \
  (caml__roots_##x.nitems = (size)), \
  (caml__roots_##x.ntables = 1), \
  (caml__roots_##x.tables [0] = &(x [0])), \
  NULL)

#define CAMLreturn /*argument*/\
  local_roots = caml__frame; \
  return /*argument*/

/* convenience macro */
#define Store_field(block, offset, val) modify (&Field (block, offset), val)

/*
   NOTE: [Begin_roots] and [End_roots] are superseded by [CAMLparam]*,
   [CAMLxparam]*, [CAMLlocal]*, [CAMLreturn].

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

#define Begin_root Begin_roots1

#define Begin_roots1(r0) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = &(r0);

#define Begin_roots2(r0, r1) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 2; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1);

#define Begin_roots3(r0, r1, r2) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 3; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2);

#define Begin_roots4(r0, r1, r2, r3) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 4; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3);

#define Begin_roots5(r0, r1, r2, r3, r4) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 5; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3); \
  caml__roots_block.tables[4] = &(r4);

#define Begin_roots_block(table, size) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = (size); \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = (table);

#define End_roots() local_roots = caml__roots_block.next; }


/*
   [Push_roots] and [Pop_roots] are obsolete.
   Use [CAMLparam], [CAMLxparam], [CAMLlocal], [CAMLreturn] instead.
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

#define Push_roots(name, size) \
  value name [(size)]; \
  struct caml__roots_block caml__roots_block; \
  { long _; for (_ = 0; _ < (size); name [_++] = Val_unit); } \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = (size); \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = name;
  
#define Pop_roots() local_roots = caml__roots_block.next;

/* [register_global_root] registers a global C variable as a memory root
   for the duration of the program, or until [remove_global_root] is
   called. */

void register_global_root (value *);

/* [remove_global_root] removes a memory root registered on a global C
   variable with [register_global_root]. */

void remove_global_root (value *);


#endif /* _memory_ */

