/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Allocation macros and functions */

#ifndef CAML_MEMORY_H
#define CAML_MEMORY_H

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "config.h"
#ifdef CAML_INTERNALS
#include "gc.h"
#include "major_gc.h"
#include "minor_gc.h"
#endif /* CAML_INTERNALS */
#include "domain.h"
#include "misc.h"
#include "mlvalues.h"
#include "alloc.h"

#ifdef __cplusplus
extern "C" {
#endif

#define BVAR_EMPTY      0x10000
#define BVAR_OWNER_MASK 0x0ffff

CAMLextern value caml_alloc_shr (mlsize_t wosize, tag_t);
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
CAMLextern value caml_alloc_shr_with_profinfo (mlsize_t, tag_t, intnat);
CAMLextern value caml_alloc_shr_preserving_profinfo (mlsize_t, tag_t,
  header_t);
#else
#define caml_alloc_shr_with_profinfo(size, tag, profinfo) \
  caml_alloc_shr(size, tag)
#define caml_alloc_shr_preserving_profinfo(size, tag, header) \
  caml_alloc_shr(size, tag)
#endif
CAMLextern value caml_alloc_shr_no_raise (mlsize_t wosize, tag_t);
CAMLextern void caml_adjust_gc_speed (mlsize_t, mlsize_t);
CAMLextern void caml_alloc_dependent_memory (mlsize_t);
CAMLextern void caml_free_dependent_memory (mlsize_t);
CAMLextern void caml_modify_field (value, int, value);
CAMLextern int caml_atomic_cas_field (value, int, value, value);
CAMLextern value caml_read_barrier (value, int);
CAMLextern void caml_initialize_field (value, int, value);
CAMLextern void caml_blit_fields (value src, int srcoff, value dst, int dstoff, int n);
CAMLextern value caml_check_urgent_gc (value);
CAMLextern void * caml_stat_alloc (asize_t);              /* Size in bytes. */
CAMLextern void caml_stat_free (void *);
CAMLextern void * caml_stat_resize (void *, asize_t);     /* Size in bytes. */
CAMLextern char *caml_alloc_for_heap (asize_t request);   /* Size in bytes. */
CAMLextern void caml_free_for_heap (char *mem);
CAMLextern int caml_add_to_heap (char *mem);

CAMLextern int caml_huge_fallback_count; /* FIXME KC: Make per domain */

/* void caml_shrink_heap (char *);        Only used in compact.c */

#ifdef CAML_INTERNALS

/* FIXME */
/* There are two GC bits in the object header, with the following
   possible values:
    00: new object, not forwarded
    11: forwarded by a fault promotion */

#define Is_promoted_hd(hd)  (((hd) & (3 << 8)) == (3 << 8))
#define Promotedhd_hd(hd)  ((hd) | (3 << 8))

extern uintnat caml_use_huge_pages;

#ifdef HAS_HUGE_PAGES
#include <sys/mman.h>
#define Heap_page_size HUGE_PAGE_SIZE
#define Round_mmap_size(x)                                      \
    (((x) + (Heap_page_size - 1)) & ~ (Heap_page_size - 1))
#endif

#ifdef DEBUG
#define DEBUG_clear(result, wosize) do{ \
  uintnat caml__DEBUG_i; \
  for (caml__DEBUG_i = 0; caml__DEBUG_i < (wosize); ++ caml__DEBUG_i){ \
    Op_val (result)[caml__DEBUG_i] = Debug_uninit_minor; \
  } \
}while(0)
#else
#define DEBUG_clear(result, wosize)
#endif

#if defined(COLLECT_STATS) && defined(NATIVE_CODE)
#define Count_alloc dom_st->allocations++
#else
#define Count_alloc
#endif

#define Alloc_small_with_profinfo(result, wosize, tag, GC, profinfo) do{    \
                                                CAMLassert ((wosize) >= 1); \
                                          CAMLassert ((tag_t) (tag) < 256); \
                                 CAMLassert ((wosize) <= Max_young_wosize); \
  caml_domain_state* dom_st = Caml_state;                     \
  dom_st->young_ptr -= Bhsize_wosize (wosize);                              \
  if (Caml_check_gc_interrupt(dom_st)){                                     \
    dom_st->young_ptr += Bhsize_wosize (wosize);                            \
    { GC }                                                                  \
    dom_st->young_ptr -= Bhsize_wosize (wosize);                            \
  }                                                                         \
  Hd_hp (dom_st->young_ptr) =                                               \
    Make_header_with_profinfo ((wosize), (tag), 0, profinfo);               \
  /* FIXME: Spacetime */ if (profinfo == 0);                                \
  (result) = Val_hp (dom_st->young_ptr);                                    \
  DEBUG_clear ((result), (wosize));                                         \
  Count_alloc;                                                              \
}while(0)

#define Alloc_small(result, wosize, tag, GC) \
  Alloc_small_with_profinfo(result, wosize, tag, GC, (uintnat)0)

#endif /* CAML_INTERNALS */

struct caml__mutex_unwind;
struct caml__roots_block {
  struct caml__roots_block *next;
  struct caml__mutex_unwind *mutexes;
  intnat ntables;
  intnat nitems;
  value *tables [5];
};

#define CAML_LOCAL_ROOTS (Caml_state->local_roots)

/* The following macros are used to declare C local variables and
   function parameters of type [value].

   The function body must start with one of the [CAMLparam] macros.
   If the function has no parameter of type [value], use [CAMLparam0].
   If the function has 1 to 5 [value] parameters, use the corresponding
   [CAMLparam] with the parameters as arguments.
   If the function has more than 5 [value] parameters, use [CAMLparam5]
   for the first 5 parameters, and one or more calls to the [CAMLxparam]
   macros for the others.
   If the function takes an array of [value]s as argument, use
   [CAMLparamN] to declare it (or [CAMLxparamN] if you already have a
   call to [CAMLparam] for some other arguments).

   If you need local variables of type [value], declare them with one
   or more calls to the [CAMLlocal] macros at the beginning of the
   function, after the call to CAMLparam.  Use [CAMLlocalN] (at the
   beginning of the function) to declare an array of [value]s.

   Your function may raise an exception or return a [value] with the
   [CAMLreturn] macro.  Its argument is simply the [value] returned by
   your function.  Do NOT directly return a [value] with the [return]
   keyword.  If your function returns void, use [CAMLreturn0]. If you
   un-register the local roots (i.e. undo the effects of the [CAMLparam*]
   and [CAMLlocal] macros) without returning immediately, use [CAMLdrop].

   All the identifiers beginning with "caml__" are reserved by OCaml.
   Do not use them for anything (local or global variables, struct or
   union tags, macros, etc.)
*/

#define CAMLparam0() \
  struct caml__roots_block *caml__frame = CAML_LOCAL_ROOTS

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

#define CAMLparamN(x, size) \
  CAMLparam0 (); \
  CAMLxparamN (x, (size))

/* CAMLunused is preserved for compatibility reasons.
   Instead of the legacy GCC/Clang-only
     CAMLunused foo;
   you should prefer
     CAMLunused_start foo CAMLunused_end;
   which supports both GCC/Clang and MSVC.
*/
#if defined(__GNUC__) && (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ > 7))
  #define CAMLunused_start __attribute__ ((unused))
  #define CAMLunused_end
  #define CAMLunused __attribute__ ((unused))
#elif _MSC_VER >= 1500
  #define CAMLunused_start  __pragma( warning (push) )           \
    __pragma( warning (disable:4189 ) )
  #define CAMLunused_end __pragma( warning (pop))
  #define CAMLunused
#else
  #define CAMLunused_start
  #define CAMLunused_end
  #define CAMLunused
#endif

#define CAMLxparam1(x) \
  struct caml__roots_block caml__roots_##x; \
  caml_domain_state* domain_state_##x = Caml_state; \
  CAMLunused int caml__dummy_##x = ( \
    (caml__roots_##x.next = domain_state_##x->local_roots), \
    (domain_state_##x->local_roots = &caml__roots_##x), \
    (caml__roots_##x.mutexes = 0), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 1), \
    (caml__roots_##x.tables [0] = &x), \
    0) \
   CAMLunused_end

#define CAMLxparam2(x, y) \
  struct caml__roots_block caml__roots_##x; \
  caml_domain_state* domain_state_##x = Caml_state; \
  CAMLunused int caml__dummy_##x = ( \
    (caml__roots_##x.next = domain_state_##x->local_roots), \
    (domain_state_##x->local_roots = &caml__roots_##x), \
    (caml__roots_##x.mutexes = 0), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 2), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    0) \
   CAMLunused_end

#define CAMLxparam3(x, y, z) \
  struct caml__roots_block caml__roots_##x; \
  caml_domain_state* domain_state_##x = Caml_state; \
  CAMLunused int caml__dummy_##x = ( \
    (caml__roots_##x.next = domain_state_##x->local_roots), \
    (domain_state_##x->local_roots = &caml__roots_##x), \
    (caml__roots_##x.mutexes = 0), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 3), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    (caml__roots_##x.tables [2] = &z), \
    0) \
  CAMLunused_end

#define CAMLxparam4(x, y, z, t) \
  struct caml__roots_block caml__roots_##x; \
  caml_domain_state* domain_state_##x = Caml_state; \
  CAMLunused int caml__dummy_##x = ( \
    (caml__roots_##x.next = domain_state_##x->local_roots), \
    (domain_state_##x->local_roots = &caml__roots_##x), \
    (caml__roots_##x.mutexes = 0), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 4), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    (caml__roots_##x.tables [2] = &z), \
    (caml__roots_##x.tables [3] = &t), \
    0) \
  CAMLunused_end

#define CAMLxparam5(x, y, z, t, u) \
  struct caml__roots_block caml__roots_##x; \
  caml_domain_state* domain_state_##x = Caml_state; \
  CAMLunused int caml__dummy_##x = ( \
    (caml__roots_##x.next = domain_state_##x->local_roots), \
    (domain_state_##x->local_roots = &caml__roots_##x), \
    (caml__roots_##x.mutexes = 0), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 5), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    (caml__roots_##x.tables [2] = &z), \
    (caml__roots_##x.tables [3] = &t), \
    (caml__roots_##x.tables [4] = &u), \
    0) \
  CAMLunused_end

#define CAMLxparamN(x, size) \
  struct caml__roots_block caml__roots_##x; \
  caml_domain_state* domain_state_##x = Caml_state; \
  CAMLunused int caml__dummy_##x = ( \
    (caml__roots_##x.next = domain_state_##x->local_roots), \
    (domain_state_##x->local_roots = &caml__roots_##x), \
    (caml__roots_##x.mutexes = 0), \
    (caml__roots_##x.nitems = (size)), \
    (caml__roots_##x.ntables = 1), \
    (caml__roots_##x.tables[0] = &(x[0])), \
    0) \
  CAMLunused_end

#define CAMLlocal1(x) \
  value x = Val_unit; \
  CAMLxparam1 (x)

#define CAMLlocal2(x, y) \
  value x = Val_unit, y = Val_unit; \
  CAMLxparam2 (x, y)

#define CAMLlocal3(x, y, z) \
  value x = Val_unit, y = Val_unit, z = Val_unit; \
  CAMLxparam3 (x, y, z)

#define CAMLlocal4(x, y, z, t) \
  value x = Val_unit, y = Val_unit, z = Val_unit, t = Val_unit; \
  CAMLxparam4 (x, y, z, t)

#define CAMLlocal5(x, y, z, t, u) \
  value x = Val_unit, y = Val_unit, z = Val_unit, t = Val_unit, u = Val_unit; \
  CAMLxparam5 (x, y, z, t, u)

#define CAMLlocalN(x, size) \
  value x [(size)]; \
  int caml__i_##x; \
  for (caml__i_##x = 0; caml__i_##x < size; caml__i_##x ++) { \
    x[caml__i_##x] = Val_unit; \
  } \
  CAMLxparamN (x, (size))

#ifdef DEBUG
#define CAMLcheck_mutexes do {   \
  struct caml__roots_block* r;   \
  for (r = CAML_LOCAL_ROOTS;     \
       r != caml__frame;         \
       r = r->next) {            \
    CAMLassert(r->mutexes == 0); \
  }                              \
} while (0)
#else
#define CAMLcheck_mutexes do {} while(0)
#endif

#define CAMLdrop do{              \
  CAMLcheck_mutexes;              \
  CAML_LOCAL_ROOTS = caml__frame; \
}while (0)

#define CAMLreturn0 do{ \
  CAMLdrop; \
  return; \
}while (0)

#define CAMLreturnT(type, result) do{ \
  type caml__temp_result = (result); \
  CAMLdrop; \
  return (caml__temp_result); \
}while(0)

#define CAMLreturn(result) CAMLreturnT(value, result)

#define CAMLnoreturn ((void) caml__frame)


/* convenience macro */
#define Store_field(block, offset, val) do{ \
  mlsize_t caml__temp_offset = (offset); \
  value caml__temp_val = (val); \
  caml_modify_field ((block), caml__temp_offset, caml__temp_val); \
}while(0)

/*
   NOTE: [Begin_roots] and [End_roots] are superseded by [CAMLparam]*,
   [CAMLxparam]*, [CAMLlocal]*, [CAMLreturn].

   [Begin_roots] and [End_roots] are used for C variables that are GC roots.
   It must contain all values in C local variables and function parameters
   at the time the minor GC is called.
   Usage:
   After initialising your local variables to legal OCaml values, but before
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
  caml_domain_state* domain_state = Caml_state; \
  caml__roots_block.next = domain_state->local_roots; \
  domain_state->local_roots = &caml__roots_block; \
  caml__roots_block.mutexes = 0; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = &(r0);

#define Begin_roots2(r0, r1) { \
  struct caml__roots_block caml__roots_block; \
  caml_domain_state* domain_state = Caml_state; \
  caml__roots_block.next = domain_state->local_roots; \
  domain_state->local_roots = &caml__roots_block; \
  caml__roots_block.mutexes = 0; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 2; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1);

#define Begin_roots3(r0, r1, r2) { \
  struct caml__roots_block caml__roots_block; \
  caml_domain_state* domain_state = Caml_state; \
  caml__roots_block.next = domain_state->local_roots; \
  domain_state->local_roots = &caml__roots_block; \
  caml__roots_block.mutexes = 0; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 3; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2);

#define Begin_roots4(r0, r1, r2, r3) { \
  struct caml__roots_block caml__roots_block; \
  caml_domain_state* domain_state = Caml_state; \
  caml__roots_block.next = domain_state->local_roots; \
  domain_state->local_roots = &caml__roots_block; \
  caml__roots_block.mutexes = 0; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 4; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3);

#define Begin_roots5(r0, r1, r2, r3, r4) { \
  struct caml__roots_block caml__roots_block; \
  caml_domain_state* domain_state = Caml_state; \
  caml__roots_block.next = domain_state->local_roots; \
  domain_state->local_roots = &caml__roots_block; \
  caml__roots_block.mutexes = 0; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 5; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3); \
  caml__roots_block.tables[4] = &(r4);

#define Begin_roots_block(table, size) { \
  struct caml__roots_block caml__roots_block; \
  caml_domain_state* domain_state = Caml_state; \
  caml__roots_block.next = domain_state->local_roots; \
  domain_state->local_roots = &caml__roots_block; \
  caml__roots_block.mutexes = 0; \
  caml__roots_block.nitems = (size); \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = (table);

#define End_roots() CAML_LOCAL_ROOTS = caml__roots_block.next; }

/* [caml_create_root] creates a new GC root, initialised to the given
   value.  The value stored in this root may only be read and written
   with [caml_read_root] and [caml_modify_root]. */

CAMLextern caml_root caml_create_root (value);

/* [caml_delete_root] deletes a root created by caml_create_root */

CAMLextern void caml_delete_root (caml_root);

/* [caml_read_root] loads the value stored in a root */

CAMLextern value caml_read_root (caml_root);

/* [caml_modify_root] stores a new value in a root */

CAMLextern void caml_modify_root (caml_root, value);

/* BVars */

CAMLprim value caml_bvar_create(value);
intnat caml_bvar_status(value);

#ifdef __cplusplus
}
#endif

#endif /* CAML_MEMORY_H */
