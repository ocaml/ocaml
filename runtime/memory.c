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

#define CAML_INTERNALS

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include "caml/config.h"
#include "caml/misc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/major_gc.h"
#include "caml/shared_heap.h"
#include "caml/domain.h"
#include "caml/addrmap.h"
#include "caml/roots.h"
#include "caml/alloc.h"
#include "caml/fiber.h"
#include "caml/platform.h"
#include "caml/eventlog.h"

/* Note [MM]: Enforcing the memory model.

   Multicore OCaml implements the memory consistency model defined in

     Bounding Data Races in Space and Time (PLDI '18)
     Stephen Dolan, KC Sivaramakrishnan, Anil Madhavapeddy.

   Unlike the C++ (also used in C11) memory model, this model gives
   well-defined behaviour to data races, ensuring that they do not
   affect unrelated computations. In C++, plain (non-atomic) accesses
   have undefined semantics if they race, so it is necessary to use at
   least relaxed atomics to implement all accesses.

   However, simply using C++ relaxed atomics for non-atomic accesses
   and C++ SC atomics for atomic ones is not enough, since the OCaml
   memory model is stronger. The prototypical example where C++
   exhibits a behaviour not allowed by OCaml is below. Assume that the
   reference b and the atomic reference a are initially 0:

       Thread 1            Thread 2
       Atomic.set a 1;     let x = !b in
       b := 1              let y = Atomic.get a in
                           ...
       Outcome: x = 1, y = 0

   This outcome is not permitted by the OCaml memory model, as can be
   seen from the operational model: if !b sees the write b := 1, then
   the Atomic.set must have executed before the Atomic.get, and since
   it is atomic the most recent set must be returned by the get,
   yielding y = 1. In the equivalent axiomatic model, this would be a
   violation of Causality.

   If this example is naively translated to C++ (using atomic_{load,
   store} for atomics, and atomic_{load, store}_explicit(...,
   memory_order_relaxed) for nonatomics), then this outcome becomes
   possible. The C++ model specifies that there is a total order on SC
   accesses, but this total order is surprisingly weak. In this
   example, we can have:

       x = !b ...
          [happens-before]
       y = Atomic.get a
          [SC-before]
       Atomic.set a 1
          [happens-before]
       b := 1

   Sadly, the composition of happens-before and SC-before does not add
   up to anything useful, and the C++ model permits the read 'x = !b'
   to read from the write 'b := 1' in this example, allowing the
   outcome above.

   To remedy this, we need to strengthen the relaxed accesses used for
   non-atomic loads and stores. The most straightforward way to do
   this is to use acquire loads and release stores instead of relaxed
   for non-atomic accesses, which ensures that all reads-from edges
   appear in the C++ synchronises-with relation, outlawing the outcome
   above.

   Using release stores for all writes also ensures publication safety
   for newly-allocated objects, and isn't necessary for initialising
   writes. The cost is free on x86, but requires a fence in
   caml_modify_field on weakly-ordered architectures (ARM, Power).

   However, instead of using acquire loads for all reads, an
   optimisation is possible. (Optimising reads is more important than
   optimising writes because reads are vastly more common). The OCaml
   memory model does not require ordering between non-atomic reads,
   which acquire loads provide. The acquire semantics are only
   necessary between a non-atomic read and an atomic access or a
   write, so we delay the acquire fence until one of those operations
   occurs.

   So, our non-atomic reads (Field/caml_read_field in mlvalues.h) are
   implemented as relaxed loads, but non-atomic writes and atomic
   operations (in this file, below) contain an odd-looking line:

      atomic_thread_fence(memory_order_acquire)

   which serves to upgrade previous relaxed loads to acquire loads.
   This encodes the OCaml memory model in the primitives provided by
   the C++ model.

   On x86, all loads and all stores have acquire/release semantics by
   default anyway, so all of these fences compile away to nothing
   (They're still useful, though: they serve to inhibit an overeager C
   compiler's optimisations). On ARMv8, actual hardware fences are
   generated.
*/

__attribute__((always_inline)) inline static void write_barrier(value obj, intnat field, value old_val, value new_val) ;

/* The write barrier does not read or write the heap, it just
   modifies domain-local data structures. */
static void write_barrier(value obj, intnat field, value old_val, value new_val)
{
  /* HACK: can't assert when get old C-api style pointers
    Assert (Is_block(obj)); */

  if (!Is_minor(obj)) {

    if (Is_block(old_val)) {
       /* if old is in the minor heap, then this is in a remembered set already */
       if (Is_minor(old_val)) return;
       /* old is a block and in the major heap */
       caml_darken(0, old_val, 0);
     }
     /* this update is creating a new link from major to minor, remember it */
     if (Is_block_and_minor(new_val)) {
       Ref_table_add(&Caml_state->minor_tables->major_ref, Op_val(obj) + field);
     }
   }
}

CAMLexport void caml_modify_field (value obj, intnat field, value val)
{
  Assert (Is_block(obj));
  Assert(field >= 0 && field < Wosize_val(obj));

  write_barrier(obj, field, Op_val(obj)[field], val);
#if defined(COLLECT_STATS) && defined(NATIVE_CODE)
  Caml_state->mutable_stores++;
#endif
  /* See Note [MM] above */
  atomic_thread_fence(memory_order_acquire);
  atomic_store_explicit(&Op_atomic_val(obj)[field], val,
                        memory_order_release);
}

/* Compatability with old C-API
   bit of a HACK as less Assert possible here
 */
CAMLexport CAMLweakdef void caml_modify (value *fp, value val)
{
  write_barrier((value)fp, 0, *fp, val);
  #if defined(COLLECT_STATS) && defined(NATIVE_CODE)
  Caml_state->mutable_stores++;
  #endif

  /* See Note [MM] above */
  atomic_thread_fence(memory_order_acquire);
  atomic_store_explicit(&Op_atomic_val((value)fp)[0], val,
                        memory_order_release);
}

CAMLexport void caml_initialize_field (value obj, intnat field, value val)
{
  Assert(Is_block(obj));
  Assert(0 <= field && field < Wosize_val(obj));
#ifdef DEBUG
  /* caml_initialize_field can only be used on just-allocated objects */
  if (Is_minor(obj))
    Assert(Op_val(obj)[field] == Debug_uninit_minor ||
           Op_val(obj)[field] == Val_unit);
  else
    Assert(Op_val(obj)[field] == Debug_uninit_major ||
           Op_val(obj)[field] == Val_unit);
#endif

  write_barrier(obj, field, Op_val(obj)[field], val);
  Op_val(obj)[field] = val;
}

/* Compatability with old C-API
   bit of a HACK as less Assert possible here
 */
CAMLexport CAMLweakdef void caml_initialize (value *fp, value val)
{
#ifdef DEBUG
  /* caml_initialize_field can only be used on just-allocated objects */
  if (Is_minor((value)fp))
    Assert(*fp == Debug_uninit_minor ||
           *fp == Val_unit);
  else
    Assert(*fp == Debug_uninit_major ||
           *fp == Val_unit);
#endif
  write_barrier((value)fp, 0, *fp, val);
  *fp = val;
}

CAMLexport int caml_atomic_cas_field (value obj, intnat field, value oldval, value newval)
{
  if (caml_domain_alone()) {
    /* non-atomic CAS since only this thread can access the object */
    value* p = &Op_val(obj)[field];
    if (*p == oldval) {
      *p = newval;
      write_barrier(obj, field, oldval, newval);
      return 1;
    } else {
      return 0;
    }
  } else {
    /* need a real CAS */
    atomic_value* p = &Op_atomic_val(obj)[field];
    if (atomic_compare_exchange_strong(p, &oldval, newval)) {
      write_barrier(obj, field, oldval, newval);
      return 1;
    } else {
      return 0;
    }
  }
}


CAMLprim value caml_atomic_load (value ref)
{
  if (caml_domain_alone()) {
    return Op_val(ref)[0];
  } else {
    value v;
    /* See Note [MM] above */
    atomic_thread_fence(memory_order_acquire);
    v = atomic_load(Op_atomic_val(ref));
    return v;
  }
}

/* stores are implemented as exchanges */
CAMLprim value caml_atomic_exchange (value ref, value v)
{
  value ret;
  if (caml_domain_alone()) {
    ret = Op_val(ref)[0];
    Op_val(ref)[0] = v;
  } else {
    /* See Note [MM] above */
    atomic_thread_fence(memory_order_acquire);
    ret = atomic_exchange(Op_atomic_val(ref), v);
  }
  write_barrier(ref, 0, ret, v);
  return ret;
}

CAMLprim value caml_atomic_cas (value ref, value oldv, value newv)
{
  if (caml_domain_alone()) {
    value* p = Op_val(ref);
    if (*p == oldv) {
      *p = newv;
      write_barrier(ref, 0, oldv, newv);
      return Val_int(1);
    } else {
      return Val_int(0);
    }
  } else {
    atomic_value* p = &Op_atomic_val(ref)[0];
    if (atomic_compare_exchange_strong(p, &oldv, newv)) {
      write_barrier(ref, 0, oldv, newv);
      return Val_int(1);
    } else {
      return Val_int(0);
    }
  }
}

CAMLprim value caml_atomic_fetch_add (value ref, value incr)
{
  value ret;
  if (caml_domain_alone()) {
    value* p = Op_val(ref);
    CAMLassert(Is_long(*p));
    ret = *p;
    *p = Val_long(Long_val(ret) + Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[0];
    ret = atomic_fetch_add(p, 2*Long_val(incr));
  }
  return ret;
}

CAMLexport void caml_set_fields (value obj, value v)
{
  int i;
  Assert (Is_block(obj));

  for (i = 0; i < Wosize_val(obj); i++) {
    caml_modify_field(obj, i, v);
  }
}

CAMLexport void caml_blit_fields (value src, int srcoff, value dst, int dstoff, int n)
{
  CAMLparam2(src, dst);
  CAMLlocal1(x);
  int i;
  Assert(Is_block(src));
  Assert(Is_block(dst));
  Assert(srcoff + n <= Wosize_val(src));
  Assert(dstoff + n <= Wosize_val(dst));
  Assert(Tag_val(src) != Infix_tag);
  Assert(Tag_val(dst) != Infix_tag);

  /* we can't use memcpy/memmove since they may not do atomic word writes.
     for instance, they may copy a byte at a time */
  if (src == dst && srcoff < dstoff) {
    /* copy descending */
    for (i = n; i > 0; i--) {
      caml_read_field(src, srcoff + i - 1, &x);
      caml_modify_field(dst, dstoff + i - 1, x);
    }
  } else {
    /* copy ascending */
    for (i = 0; i < n; i++) {
      caml_read_field(src, srcoff + i, &x);
      caml_modify_field(dst, dstoff + i, x);
    }
  }
  CAMLreturn0;
}

CAMLexport value caml_alloc_shr (mlsize_t wosize, tag_t tag)
{
  caml_domain_state* dom_st = Caml_state;
  value* v = caml_shared_try_alloc(dom_st->shared_heap, wosize, tag, 0);
  if (v == NULL) {
    caml_raise_out_of_memory ();
  }
  dom_st->allocated_words += Whsize_wosize (wosize);
  if (dom_st->allocated_words > dom_st->minor_heap_wsz) {
    caml_urge_major_slice();
  }

  if (tag < No_scan_tag) {
    mlsize_t i;
    for (i = 0; i < wosize; i++) {
      value init_val = Val_unit;
      #ifdef DEBUG
      init_val = Debug_uninit_major;
      #endif
      Op_hp(v)[i] = init_val;
    }
  }
#if defined(COLLECT_STATS) && defined(NATIVE_CODE)
  dom_st->allocations++;
#endif
  return Val_hp(v);
}

CAMLexport value caml_alloc_shr_noexc(mlsize_t wosize, tag_t tag) {
  caml_domain_state *dom_st = Caml_state;
  value *v = caml_shared_try_alloc(dom_st->shared_heap, wosize, tag, 0);
  if (v == NULL) {
    return (value)NULL;
  }
  dom_st->allocated_words += Whsize_wosize(wosize);
  if (dom_st->allocated_words > dom_st->minor_heap_wsz) {
    caml_urge_major_slice();
  }

  if (tag < No_scan_tag) {
    mlsize_t i;
    for (i = 0; i < wosize; i++) {
      value init_val = Val_unit;
#ifdef DEBUG
      init_val = Debug_uninit_major;
#endif
      Op_hp(v)[i] = init_val;
    }
  }
#if defined(COLLECT_STATS) && defined(NATIVE_CODE)
  dom_st->allocations++;
#endif
  return Val_hp(v);
}

#ifdef DEBUG
header_t hd_val (value v) {
  return (header_t)Hd_val(v);
}

int is_minor(value v) {
  return Is_minor(v);
}

int is_young(value v) {
  return Is_young(v);
}

int has_status(value v, status s) {
  return Has_status_hd(Hd_val(v),s);
}

int tag_val(value v) {
  return (int)Tag_val(v);
}
#endif

/* Global memory pool.

   The pool is structured as a ring of blocks, where each block's header
   contains two links: to the previous and to the next block. The data
   structure allows for insertions and removals of blocks in constant time,
   given that a pointer to the operated block is provided.

   Initially, the pool contains a single block -- a pivot with no data, the
   guaranteed existence of which makes for a more concise implementation.

   The API functions that operate on the pool receive not pointers to the
   block's header, but rather pointers to the block's "data" field. This
   behaviour is required to maintain compatibility with the interfaces of
   [malloc], [realloc], and [free] family of functions, as well as to hide
   the implementation from the user.
*/

/* A type with the most strict alignment requirements */
union max_align {
  char c;
  short s;
  long l;
  int i;
  float f;
  double d;
  void *v;
  void (*q)(void);
};

struct pool_block {
#ifdef DEBUG
  intnat magic;
#endif
  struct pool_block *next;
  struct pool_block *prev;
  /* Use C99's flexible array types if possible */
#if (__STDC_VERSION__ >= 199901L)
  union max_align data[];  /* not allocated, used for alignment purposes */
#else
  union max_align data[1];
#endif
};

#if (__STDC_VERSION__ >= 199901L)
#define SIZEOF_POOL_BLOCK sizeof(struct pool_block)
#else
#define SIZEOF_POOL_BLOCK offsetof(struct pool_block, data)
#endif

static struct pool_block *pool = NULL;
static caml_plat_mutex pool_mutex = CAML_PLAT_MUTEX_INITIALIZER;

/* Returns a pointer to the block header, given a pointer to "data" */
static struct pool_block* get_pool_block(caml_stat_block b)
{
  if (b == NULL)
    return NULL;

  else {
    struct pool_block *pb =
      (struct pool_block*)(((char*)b) - SIZEOF_POOL_BLOCK);
#ifdef DEBUG
    CAMLassert(pb->magic == Debug_pool_magic);
#endif
    return pb;
  }
}

CAMLexport void caml_stat_create_pool(void)
{
  if (pool == NULL) {
    pool = malloc(SIZEOF_POOL_BLOCK);
    if (pool == NULL)
      caml_fatal_error("Fatal error: out of memory.\n");
#ifdef DEBUG
    pool->magic = Debug_pool_magic;
#endif
    pool->next = pool;
    pool->prev = pool;
  }
}

CAMLexport void caml_stat_destroy_pool(void)
{
  caml_plat_lock(&pool_mutex);
  if (pool != NULL) {
    pool->prev->next = NULL;
    while (pool != NULL) {
      struct pool_block *next = pool->next;
      free(pool);
      pool = next;
    }
    pool = NULL;
  }
  caml_plat_unlock(&pool_mutex);
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_alloc_noexc(asize_t sz)
{
  /* Backward compatibility mode */
  if (pool == NULL)
    return malloc(sz);
  else {
    struct pool_block *pb = malloc(sz + SIZEOF_POOL_BLOCK);
    if (pb == NULL) return NULL;
#ifdef DEBUG
    memset(&(pb->data), Debug_uninit_stat, sz);
    pb->magic = Debug_pool_magic;
#endif

    /* Linking the block into the ring */
    caml_plat_lock(&pool_mutex);
    pb->next = pool->next;
    pb->prev = pool;
    pool->next->prev = pb;
    pool->next = pb;
    caml_plat_unlock(&pool_mutex);

    return &(pb->data);
  }
}

/* [sz] and [modulo] are numbers of bytes */
CAMLexport void* caml_stat_alloc_aligned_noexc(asize_t sz, int modulo,
                                               caml_stat_block *b)
{
  char *raw_mem;
  uintnat aligned_mem;
  CAMLassert (0 <= modulo && modulo < Page_size);
  raw_mem = (char *) caml_stat_alloc_noexc(sz + Page_size);
  if (raw_mem == NULL) return NULL;
  *b = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((uintnat) raw_mem / Page_size + 1) * Page_size);
#ifdef DEBUG
  {
    uintnat *p;
    uintnat *p0 = (void *) *b;
    uintnat *p1 = (void *) (aligned_mem - modulo);
    uintnat *p2 = (void *) (aligned_mem - modulo + sz);
    uintnat *p3 = (void *) ((char *) *b + sz + Page_size);
    for (p = p0; p < p1; p++) *p = Debug_filler_align;
    for (p = p1; p < p2; p++) *p = Debug_uninit_align;
    for (p = p2; p < p3; p++) *p = Debug_filler_align;
  }
#endif
  return (char *) (aligned_mem - modulo);
}

/* [sz] and [modulo] are numbers of bytes */
CAMLexport void* caml_stat_alloc_aligned(asize_t sz, int modulo,
                                         caml_stat_block *b)
{
  void *result = caml_stat_alloc_aligned_noexc(sz, modulo, b);
  /* malloc() may return NULL if size is 0 */
  if ((result == NULL) && (sz != 0))
    caml_raise_out_of_memory();
  return result;
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_alloc(asize_t sz)
{
  void *result = caml_stat_alloc_noexc(sz);
  /* malloc() may return NULL if size is 0 */
  if ((result == NULL) && (sz != 0))
    caml_raise_out_of_memory();
  return result;
}

CAMLexport void caml_stat_free(caml_stat_block b)
{
  /* Backward compatibility mode */
  if (pool == NULL)
    free(b);
  else {
    struct pool_block *pb = get_pool_block(b);
    if (pb == NULL) return;

    /* Unlinking the block from the ring */
    caml_plat_lock(&pool_mutex);
    pb->prev->next = pb->next;
    pb->next->prev = pb->prev;
    caml_plat_unlock(&pool_mutex);

    free(pb);
  }
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_resize_noexc(caml_stat_block b, asize_t sz)
{
  /* Backward compatibility mode */
  if (pool == NULL)
    return realloc(b, sz);
  else {
    struct pool_block *pb = get_pool_block(b);
    struct pool_block *pb_new = realloc(pb, sz + SIZEOF_POOL_BLOCK);
    if (pb_new == NULL) return NULL;

    /* Relinking the new block into the ring in place of the old one */
    caml_plat_lock(&pool_mutex);
    pb_new->prev->next = pb_new;
    pb_new->next->prev = pb_new;
    caml_plat_unlock(&pool_mutex);

    return &(pb_new->data);
  }
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_resize(caml_stat_block b, asize_t sz)
{
  void *result = caml_stat_resize_noexc(b, sz);
  if (result == NULL)
    caml_raise_out_of_memory();
  return result;
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_calloc_noexc(asize_t num, asize_t sz)
{
  uintnat total;
  if (caml_umul_overflow(sz, num, &total))
    return NULL;
  else {
    caml_stat_block result = caml_stat_alloc_noexc(total);
    if (result != NULL)
      memset(result, 0, total);
    return result;
  }
}

CAMLexport caml_stat_string caml_stat_strdup_noexc(const char *s)
{
  size_t slen = strlen(s);
  caml_stat_block result = caml_stat_alloc_noexc(slen + 1);
  if (result == NULL)
    return NULL;
  memcpy(result, s, slen + 1);
  return result;
}

CAMLexport caml_stat_string caml_stat_strdup(const char *s)
{
  caml_stat_string result = caml_stat_strdup_noexc(s);
  if (result == NULL)
    caml_raise_out_of_memory();
  return result;
}

#ifdef _WIN32

CAMLexport wchar_t * caml_stat_wcsdup(const wchar_t *s)
{
  int slen = wcslen(s);
  wchar_t* result = caml_stat_alloc((slen + 1)*sizeof(wchar_t));
  if (result == NULL)
    caml_raise_out_of_memory();
  memcpy(result, s, (slen + 1)*sizeof(wchar_t));
  return result;
}

#endif

CAMLexport caml_stat_string caml_stat_strconcat(int n, ...)
{
  va_list args;
  char *result, *p;
  size_t len = 0;
  int i;

  va_start(args, n);
  for (i = 0; i < n; i++) {
    const char *s = va_arg(args, const char*);
    len += strlen(s);
  }
  va_end(args);

  result = caml_stat_alloc(len + 1);

  va_start(args, n);
  p = result;
  for (i = 0; i < n; i++) {
    const char *s = va_arg(args, const char*);
    size_t l = strlen(s);
    memcpy(p, s, l);
    p += l;
  }
  va_end(args);

  *p = 0;
  return result;
}

#ifdef _WIN32

CAMLexport wchar_t* caml_stat_wcsconcat(int n, ...)
{
  va_list args;
  wchar_t *result, *p;
  size_t len = 0;
  int i;

  va_start(args, n);
  for (i = 0; i < n; i++) {
    const wchar_t *s = va_arg(args, const wchar_t*);
    len += wcslen(s);
  }
  va_end(args);

  result = caml_stat_alloc((len + 1)*sizeof(wchar_t));

  va_start(args, n);
  p = result;
  for (i = 0; i < n; i++) {
    const wchar_t *s = va_arg(args, const wchar_t*);
    size_t l = wcslen(s);
    memcpy(p, s, l*sizeof(wchar_t));
    p += l;
  }
  va_end(args);

  *p = 0;
  return result;
}

#endif
