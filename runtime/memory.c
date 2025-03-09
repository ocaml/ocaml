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
#include <stdalign.h>
#if defined(_WIN32)
#include <malloc.h>
#endif
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/misc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/memprof.h"
#include "caml/major_gc.h"
#include "caml/signals.h"
#include "caml/shared_heap.h"
#include "caml/domain.h"
#include "caml/roots.h"
#include "caml/alloc.h"
#include "caml/fiber.h"
#include "caml/platform.h"
#include "caml/runtime_events.h"
#include "caml/tsan.h"

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
   caml_modify on weakly-ordered architectures (ARM, Power).

   However, instead of using acquire loads for all reads, an
   optimisation is possible. (Optimising reads is more important than
   optimising writes because reads are vastly more common). The OCaml
   memory model does not require ordering between non-atomic reads,
   which acquire loads provide. The acquire semantics are only
   necessary between a non-atomic read and an atomic access or a
   write, so we delay the acquire fence until one of those operations
   occurs.

   So, our non-atomic reads are implemented as standard relaxed loads,
   but non-atomic writes and atomic operations (in this file, below)
   contain an odd-looking line:

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

/* Note [MMMOC]: Mixing the Memory Models of OCaml and C.

   Note [MM] above document how the code generated by the OCaml
   compiler, and the memory-access helper functions it uses like
   [caml_modify], coordinate to provide a convenient memory model to
   pure OCaml programs.

   On the other hand, hybrid OCaml/C programs are written in two
   languages with two different memory models, and we currently do not
   know how to reason formally about the result. This affects C code
   that is written using the OCaml FFI, typically in user libraries,
   but also the C code of the OCaml runtime itself.

   The current recommendations for the runtime code are as follows:

   - For runtime data structures that are only used from C code, we
     should use C11 atomics.

   - But for the OCaml heap and any other data that is accessed both
     from the C runtime and from the OCaml mutator, we currently use
     (volatile *) following the Linux model
     (https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2020/p0124r7.html):

       1. Using C11 atomics does not provide any correctness
          guarantees in presence of races coming from OCaml accesses, we
          need to reason on the assembly level anyway.

       2. Using consume or acquire or sequential may be too expensive
          (for a general use in the [Field] macro).

       3. Using relaxed and [volatile *] may be too weak in general,
          as our C code assumes a dependency ordering (reading fields
          after seeing a constructor).

       4. But in practice many usage patterns of [volatile *]
          (and possibly [relaxed]) are safe with C compilers. The
          dangerous patterns are unlikely to be met in real-life OCaml
          FFI code. We currently use a [volatile *] cast in Field
          for this reason.

   Note that these recommendations do not constitute a proper memory
   model for mixed OCaml/C programs. To be used safely, they should
   come with a set of guidelines on C programming patterns to avoid
   (and compilers, optimizers, compiler options to avoid...), similar
   to the Linux document
   https://www.kernel.org/doc/Documentation/RCU/rcu_dereference.txt on
   RCU dereference. We do not currently have such a document.
*/

Caml_inline void write_barrier(
  value obj, intnat field, value old_val, value new_val)
{
  /* HACK: can't assert when get old C-api style pointers
    CAMLassert (Is_block(obj)); */

  if (!Is_young(obj)) {

    if (Is_block(old_val)) {
       /* if old is in the minor heap,
          then this is in a remembered set already */
       if (Is_young(old_val)) return;
       /* old is a block and in the major heap */
       caml_darken(Caml_state, old_val, 0);
     }
     /* this update is creating a new link from major to minor, remember it */
     if (Is_block_and_young(new_val)) {
       Ref_table_add(&Caml_state->minor_tables->major_ref, Op_val(obj) + field);
     }
   }
}

CAMLno_tsan /* We remove the ThreadSanitizer instrumentation of memory accesses
               by the compiler and instrument manually, because we want
               ThreadSanitizer to see a plain store here (this is necessary to
               detect data races). */
CAMLexport CAMLweakdef void caml_modify (volatile value *fp, value val)
{
#if defined(WITH_THREAD_SANITIZER) && defined(NATIVE_CODE)
  __tsan_func_entry(__builtin_return_address(0));
#endif

  write_barrier((value)fp, 0, *fp, val);

  /* See Note [MM] above */
  atomic_thread_fence(memory_order_acquire);

#if defined(WITH_THREAD_SANITIZER) && defined(NATIVE_CODE)
  /* The release store below is not instrumented because of the
   * CAMLno_tsan. We signal it to ThreadSanitizer as a plain store (see
   * ocaml-multicore/ocaml-tsan/pull/22#issuecomment-1377439074 on Github).
   */
  __tsan_write8((void *)fp);
  __tsan_func_exit(NULL);
#endif

  atomic_store_release(&Op_atomic_val((value)fp)[0], val);
}

/* Dependent memory is all memory blocks allocated out of the heap
   that depend on the GC (and finalizers) for deallocation.
   For the GC to take dependent memory into account when computing
   its automatic speed setting,
   you must call [caml_alloc_dependent_memory] when you allocate some
   dependent memory, and [caml_free_dependent_memory] when you
   free it.  In both cases, you pass as argument the size (in bytes)
   of the block being allocated or freed.
*/
CAMLexport void caml_alloc_dependent_memory (mlsize_t nbytes)
{
  Caml_state->dependent_size += nbytes / sizeof (value);
  Caml_state->dependent_allocated += nbytes / sizeof (value);
}

CAMLexport void caml_free_dependent_memory (mlsize_t nbytes)
{
  if (Caml_state->dependent_size < nbytes / sizeof (value)){
    Caml_state->dependent_size = 0;
  }else{
    Caml_state->dependent_size -= nbytes / sizeof (value);
  }
}

/* Use this function to tell the major GC to speed up when you use
   finalized blocks to automatically deallocate resources (other
   than memory). The GC will do at least one cycle every [max]
   allocated resources; [res] is the number of resources allocated
   this time.
   Note that only [res/max] is relevant.  The units (and kind of
   resource) can change between calls to [caml_adjust_gc_speed].

   If [max] = 0, then we use a number proportional to the major heap
   size and [caml_custom_major_ratio]. In this case, [mem] should
   be a number of bytes and the trade-off between GC work and space
   overhead is under the control of the user through
   [caml_custom_major_ratio].
*/
CAMLexport void caml_adjust_gc_speed (mlsize_t res, mlsize_t max)
{
  if (max == 0) max = caml_custom_get_max_major ();
  if (res > max) res = max;
  Caml_state->extra_heap_resources += (double) res / (double) max;
  if (Caml_state->extra_heap_resources > 0.2){
    CAML_EV_COUNTER (EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED, 1);
    caml_request_major_slice (1);
  }
}

/* This function is analogous to [caml_adjust_gc_speed]. When the
   accumulated sum of [res/max] values reaches 1, a minor GC is
   triggered.
*/
CAMLexport void caml_adjust_minor_gc_speed (mlsize_t res, mlsize_t max)
{
  if (max == 0) max = 1;
  Caml_state->extra_heap_resources_minor += (double) res / (double) max;
  if (Caml_state->extra_heap_resources_minor > 1.0) {
    caml_request_minor_gc ();
  }
}

/* You must use [caml_initialize] to store the initial value in a field of a
   block, unless you are sure the value is not a young block, in which case a
   plain assignment would do.

   [caml_initialize] never calls the GC, so you may call it while a block is
   unfinished (i.e. just after a call to [caml_alloc_shr].) */
CAMLno_tsan /* Avoid instrumenting initializing writes with TSan: they should
               never cause data races (albeit for reasons outside of the C11
               memory model). */
CAMLexport CAMLweakdef void caml_initialize (volatile value *fp, value val)
{
#ifdef DEBUG
  /* Previous value should not be a pointer.
     In the debug runtime, it can be either a TMC placeholder,
     or an uninitialized value canary (Debug_uninit_{major,minor}). */
  CAMLassert(Is_long(*fp) || *fp == Debug_uninit_major
             || *fp == Debug_uninit_minor);
#endif
  *fp = val;
  if (!Is_young((value)fp) && Is_block_and_young (val))
    Ref_table_add(&Caml_state->minor_tables->major_ref, fp);
}

CAMLexport int caml_atomic_cas_field (
  value obj, intnat field, value oldval, value newval)
{
  if (caml_domain_alone()) {
    /* non-atomic CAS since only this thread can access the object */
    volatile value* p = &Field(obj, field);
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
    int cas_ret = atomic_compare_exchange_strong(p, &oldval, newval);
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
    if (cas_ret) {
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
    return Field(ref, 0);
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
    ret = Field(ref, 0);
    Field(ref, 0) = v;
  } else {
    /* See Note [MM] above */
    atomic_thread_fence(memory_order_acquire);
    ret = atomic_exchange(Op_atomic_val(ref), v);
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
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
    int cas_ret = atomic_compare_exchange_strong(p, &oldv, newv);
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
    if (cas_ret) {
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
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return ret;
}

CAMLexport void caml_set_fields (value obj, value v)
{
  CAMLassert (Is_block(obj));

  for (int i = 0; i < Wosize_val(obj); i++) {
    caml_modify(&Field(obj, i), v);
  }
}

Caml_inline value alloc_shr(mlsize_t wosize, tag_t tag, reserved_t reserved,
                            int noexc)
{
  Caml_check_caml_state();
  caml_domain_state *dom_st = Caml_state;
  value *v = caml_shared_try_alloc(dom_st->shared_heap,
                                   wosize, tag, reserved);
  if (v == NULL) {
    if (!noexc)
      caml_raise_out_of_memory();
    else
      return (value)NULL;
  }

  dom_st->allocated_words += Whsize_wosize(wosize);
  dom_st->allocated_words_direct += Whsize_wosize(wosize);
  if (dom_st->allocated_words_direct > dom_st->minor_heap_wsz / 5) {
    CAML_EV_COUNTER (EV_C_REQUEST_MAJOR_ALLOC_SHR, 1);
    caml_request_major_slice(1);
  }

#ifdef DEBUG
  if (tag < No_scan_tag) {
    for (mlsize_t i = 0; i < wosize; i++)
      Op_hp(v)[i] = Debug_uninit_major;
  }
#endif
  caml_memprof_sample_block(Val_hp(v), wosize,
                            Whsize_wosize(wosize),
                            CAML_MEMPROF_SRC_NORMAL);

  return Val_hp(v);
}

CAMLexport value caml_alloc_shr(mlsize_t wosize, tag_t tag)
{
  return alloc_shr(wosize, tag, 0, 0);
}

CAMLexport value caml_alloc_shr_reserved(mlsize_t wosize,
                                         tag_t tag,
                                         reserved_t reserved)
{
  return alloc_shr(wosize, tag, reserved, 0);
}


CAMLexport value caml_alloc_shr_noexc(mlsize_t wosize, tag_t tag) {
  return alloc_shr(wosize, tag, 0, 1);
}

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

#if !defined(HAVE_MAX_ALIGN_T) && defined(_MSC_VER)
typedef double max_align_t;
#endif

#define MAX(a, b) ((a) > (b) ? (a) : (b))

#if defined(_M_AMD64) || defined(__x86_64__)
#define pool_block_align MAX(alignof(max_align_t), 16 /* for SSE */)
#else
#define pool_block_align alignof(max_align_t)
#endif

struct pool_block {
  struct pool_block *next;
  struct pool_block *prev;
  alignas(pool_block_align) char data[]; /* flexible array member */
};

static struct pool_block *pool = NULL;
static caml_plat_mutex pool_mutex = CAML_PLAT_MUTEX_INITIALIZER;

/* Returns a pointer to the block header, given a pointer to "data" */
static struct pool_block* get_pool_block(caml_stat_block b)
{
  if (b == NULL) {
    return NULL;
  } else {
    return (struct pool_block *)
      (((char *) b) - offsetof(struct pool_block, data));
  }
}

/* Linking a pool block into the ring */
static void link_pool_block(struct pool_block *pb)
{
  caml_plat_lock_blocking(&pool_mutex);
  pb->next = pool->next;
  pb->prev = pool;
  pool->next->prev = pb;
  pool->next = pb;
  caml_plat_unlock(&pool_mutex);
}

/* Unlinking a pool block from the ring */
static void unlink_pool_block(struct pool_block *pb)
{
    caml_plat_lock_blocking(&pool_mutex);
    pb->prev->next = pb->next;
    pb->next->prev = pb->prev;
    caml_plat_unlock(&pool_mutex);
}

CAMLexport void caml_stat_create_pool(void)
{
  if (pool == NULL) {
    pool = malloc(sizeof(struct pool_block));
    if (pool == NULL)
      caml_fatal_error("Fatal error: out of memory.\n");
    pool->next = pool;
    pool->prev = pool;
  }
}

CAMLexport void caml_stat_destroy_pool(void)
{
  caml_plat_lock_blocking(&pool_mutex);
  if (pool != NULL) {
    pool->prev->next = NULL;
    while (pool != NULL) {
      struct pool_block *next = pool->next;
#ifdef _WIN32
      _aligned_free(pool);
#else
      free(pool);
#endif
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
    struct pool_block *pb;
#ifdef _WIN32
    pb = _aligned_malloc(sizeof(struct pool_block) + sz, pool_block_align);
#else
    pb = malloc(sizeof(struct pool_block) + sz);
#endif
    if (pb == NULL) return NULL;
    link_pool_block(pb);
    return &(pb->data);
  }
}

/* [sz] and [modulo] are numbers of bytes */
CAMLexport void* caml_stat_alloc_aligned_noexc(asize_t sz, int modulo,
                                               caml_stat_block *b)
{
  char *raw_mem;
  uintnat aligned_mem;
  CAMLassert(0 <= modulo);
  CAMLassert(modulo < Page_size);
  raw_mem = (char *) caml_stat_alloc_noexc(sz + Page_size);
  if (raw_mem == NULL) return NULL;
  *b = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((uintnat) raw_mem / Page_size + 1) * Page_size);
#ifdef DEBUG
  {
    uintnat *p0 = (void *) *b;
    uintnat *p1 = (void *) (aligned_mem - modulo);
    uintnat *p2 = (void *) (aligned_mem - modulo + sz);
    uintnat *p3 = (void *) ((char *) *b + sz + Page_size);
    for (uintnat *p = p0; p < p1; p++) *p = Debug_filler_align;
    for (uintnat *p = p1; p < p2; p++) *p = Debug_uninit_align;
    for (uintnat *p = p2; p < p3; p++) *p = Debug_filler_align;
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
    unlink_pool_block(pb);
#ifdef _WIN32
    _aligned_free(pb);
#else
    free(pb);
#endif
  }
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_resize_noexc(caml_stat_block b, asize_t sz)
{
  if(b == NULL)
    return caml_stat_alloc_noexc(sz);
  /* Backward compatibility mode */
  if (pool == NULL)
    return realloc(b, sz);
  else {
    struct pool_block *pb = get_pool_block(b);
    struct pool_block *pb_new;
    /* Unlinking the block because it can be freed by realloc
       while other domains access the pool concurrently. */
    unlink_pool_block(pb);
    /* Reallocating */
#ifdef _WIN32
    pb_new = _aligned_realloc(pb, sizeof(struct pool_block) + sz,
                              pool_block_align);
#else
    pb_new = realloc(pb, sizeof(struct pool_block) + sz);
#endif
    if (pb_new == NULL) {
      /* The old block is still there, relinking it */
      link_pool_block(pb);
      return NULL;
    } else {
      link_pool_block(pb_new);
      return &(pb_new->data);
    }
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

CAMLexport caml_stat_string caml_stat_memdup(const char *s, asize_t size,
                                             asize_t *out_size)
{
  CAMLassert(size > 0);
  caml_stat_block result = caml_stat_alloc(size);
  memcpy(result, s, size);
  if (out_size != NULL)
    *out_size = size;
  return result;
}

#ifdef _WIN32

CAMLexport wchar_t * caml_stat_wcsdup_noexc(const wchar_t *s)
{
  size_t slen = wcslen(s);
  wchar_t* result = caml_stat_alloc_noexc((slen + 1)*sizeof(wchar_t));
  if (result == NULL)
    return NULL;
  memcpy(result, s, (slen + 1)*sizeof(wchar_t));
  return result;
}

CAMLexport wchar_t * caml_stat_wcsdup(const wchar_t *s)
{
  wchar_t* result = caml_stat_wcsdup_noexc(s);
  if (result == NULL)
    caml_raise_out_of_memory();
  return result;
}

#endif

CAMLexport caml_stat_string caml_stat_strconcat(int n, ...)
{
  va_list args;
  char *result, *p;
  size_t len = 0;

  va_start(args, n);
  for (int i = 0; i < n; i++) {
    const char *s = va_arg(args, const char*);
    len += strlen(s);
  }
  va_end(args);

  result = caml_stat_alloc(len + 1);

  va_start(args, n);
  p = result;
  for (int i = 0; i < n; i++) {
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

  va_start(args, n);
  for (int i = 0; i < n; i++) {
    const wchar_t *s = va_arg(args, const wchar_t*);
    len += wcslen(s);
  }
  va_end(args);

  result = caml_stat_alloc((len + 1)*sizeof(wchar_t));

  va_start(args, n);
  p = result;
  for (int i = 0; i < n; i++) {
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
