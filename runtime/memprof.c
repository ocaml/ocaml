/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Jacques-Henri Jourdan, projet Gallium, INRIA Paris          */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include "caml/memprof.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/signals.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/backtrace_prim.h"
#include "caml/weak.h"
#include "caml/stack.h"
#include "caml/misc.h"
#include "caml/compact.h"
#include "caml/printexc.h"
#include "caml/eventlog.h"

#define RAND_BLOCK_SIZE 64

static uint32_t xoshiro_state[4][RAND_BLOCK_SIZE];
static uintnat rand_geom_buff[RAND_BLOCK_SIZE];
static uint32_t rand_pos;

/* [lambda] is the mean number of samples for each allocated word (including
   block headers). */
static double lambda = 0;
/* Precomputed value of [1/log(1-lambda)], for fast sampling of
   geometric distribution.
   Dummy if [lambda = 0]. */
static float one_log1m_lambda;

static intnat callstack_size;

/* accessors for the OCaml type [Gc.Memprof.tracker],
   which is the type of the [tracker] global below. */
#define Alloc_minor(tracker) (Field(tracker, 0))
#define Alloc_major(tracker) (Field(tracker, 1))
#define Promote(tracker) (Field(tracker, 2))
#define Dealloc_minor(tracker) (Field(tracker, 3))
#define Dealloc_major(tracker) (Field(tracker, 4))

static value tracker;

/* Gc.Memprof.allocation_source */
enum { SRC_NORMAL = 0, SRC_MARSHAL = 1, SRC_CUSTOM = 2 };

struct tracked {
  /* Memory block being sampled. This is a weak GC root. */
  value block;

  /* Number of samples in this block. */
  uintnat n_samples;

  /* The size of this block. */
  uintnat wosize;

  /* The value returned by the previous callback for this block, or
     the callstack if the alloc callback has not been called yet.
     This is a strong GC root. */
  value user_data;

  /* The thread currently running a callback for this entry,
     or NULL if there is none */
  struct caml_memprof_th_ctx* running;

  /* Whether this block has been initially allocated in the minor heap. */
  unsigned int alloc_young : 1;

  /* The source of the allocation: normal allocations, marshal or custom_mem. */
  unsigned int source : 2;

  /* Whether this block has been promoted. Implies [alloc_young]. */
  unsigned int promoted : 1;

  /* Whether this block has been deallocated. */
  unsigned int deallocated : 1;

  /* Whether the allocation callback has been called depends on
     whether the entry is in a thread local entry array or in
     [entries_global]. */

  /* Whether the promotion callback has been called. */
  unsigned int cb_promote_called : 1;

  /* Whether the deallocation callback has been called. */
  unsigned int cb_dealloc_called : 1;

  /* Whether this entry is deleted. */
  unsigned int deleted : 1;
};

/* During the alloc callback for a minor allocation, the block being
   sampled is not yet allocated. Instead, we place in the block field
   a value computed with the following macro: */
#define Placeholder_magic 0x04200000
#define Placeholder_offs(offset) (Val_long(offset + Placeholder_magic))
#define Offs_placeholder(block) (Long_val(block) & 0xFFFF)
#define Is_placeholder(block) \
  (Is_long(block) && (Long_val(block) & ~(uintnat)0xFFFF) == Placeholder_magic)

/* A resizable array of entries */
struct entry_array {
  struct tracked* t;
  uintnat min_alloc_len, alloc_len, len;
  /* Before this position, the [block] and [user_data] fields point to
     the major heap ([young <= len]). */
  uintnat young_idx;
  /* There are no blocks to be deleted before this position
     ([delete_idx <= len]). */
  uintnat delete_idx;
};

#define MIN_ENTRIES_LOCAL_ALLOC_LEN 16
#define MIN_ENTRIES_GLOBAL_ALLOC_LEN 128

/* Entries for other blocks. This variable is shared across threads. */
static struct entry_array entries_global =
  { NULL, MIN_ENTRIES_GLOBAL_ALLOC_LEN, 0, 0, 0, 0 };

/* There are no pending callbacks in [entries_global] before this
   position ([callback_idx <= entries_global.len]). */
static uintnat callback_idx;

#define CB_IDLE -1
#define CB_LOCAL -2
#define CB_STOPPED -3

/* Structure for thread-local variables. */
struct caml_memprof_th_ctx {
  /* [suspended] is used for masking memprof callbacks when
     a callback is running or when an uncaught exception handler is
     called. */
  int suspended;

  /* [callback_status] contains:
     - CB_STOPPED if the current thread is running a callback, but
       sampling has been stopped using [caml_memprof_stop];
     - The index of the corresponding entry in the [entries_global]
       array if the current thread is currently running a promotion or
       a deallocation callback;
     - CB_LOCAL if the current thread is currently running an
       allocation callback;
     - CB_IDLE if the current thread is not running any callback.
  */
  intnat callback_status;

  /* Entries for blocks whose alloc callback has not yet been called. */
  struct entry_array entries;
} caml_memprof_main_ctx =
  { 0, CB_IDLE, { NULL, MIN_ENTRIES_LOCAL_ALLOC_LEN, 0, 0, 0, 0 } };
static struct caml_memprof_th_ctx* local = &caml_memprof_main_ctx;

/* Pointer to the word following the next sample in the minor
   heap. Equals [Caml_state->young_alloc_start] if no sampling is planned in
   the current minor heap.
   Invariant: [caml_memprof_young_trigger <= Caml_state->young_ptr].
 */
value* caml_memprof_young_trigger;

/* Whether memprof has been initialized.  */
static int init = 0;

/* Whether memprof is started. */
static int started = 0;

/* Buffer used to compute backtraces */
static value* callstack_buffer = NULL;
static intnat callstack_buffer_len = 0;

/**** Statistical sampling ****/

Caml_inline uint64_t splitmix64_next(uint64_t* x)
{
  uint64_t z = (*x += 0x9E3779B97F4A7C15ull);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
  return z ^ (z >> 31);
}

static void xoshiro_init(void)
{
  int i;
  uint64_t splitmix64_state = 42;
  rand_pos = RAND_BLOCK_SIZE;
  for (i = 0; i < RAND_BLOCK_SIZE; i++) {
    uint64_t t = splitmix64_next(&splitmix64_state);
    xoshiro_state[0][i] = t & 0xFFFFFFFF;
    xoshiro_state[1][i] = t >> 32;
    t = splitmix64_next(&splitmix64_state);
    xoshiro_state[2][i] = t & 0xFFFFFFFF;
    xoshiro_state[3][i] = t >> 32;
  }
}

Caml_inline uint32_t xoshiro_next(int i)
{
  uint32_t res = xoshiro_state[0][i] + xoshiro_state[3][i];
  uint32_t t = xoshiro_state[1][i] << 9;
  xoshiro_state[2][i] ^= xoshiro_state[0][i];
  xoshiro_state[3][i] ^= xoshiro_state[1][i];
  xoshiro_state[1][i] ^= xoshiro_state[2][i];
  xoshiro_state[0][i] ^= xoshiro_state[3][i];
  xoshiro_state[2][i] ^= t;
  t = xoshiro_state[3][i];
  xoshiro_state[3][i] = (t << 11) | (t >> 21);
  return res;
}

/* Computes [log((y+0.5)/2^32)], up to a relatively good precision,
   and guarantee that the result is negative.
   The average absolute error is very close to 0. */
Caml_inline float log_approx(uint32_t y)
{
  union { float f; int32_t i; } u;
  float exp, x;
  u.f = y + 0.5f;    /* We convert y to a float ... */
  exp = u.i >> 23;   /* ... of which we extract the exponent ... */
  u.i = (u.i & 0x7FFFFF) | 0x3F800000;
  x = u.f;           /* ... and the mantissa. */

  return
    /* This polynomial computes the logarithm of the mantissa (which
       is in [1, 2]), up to an additive constant. It is chosen such that :
       - Its degree is 4.
       - Its average value is that of log in [1, 2]
             (the sampling has the right mean when lambda is small).
       - f(1) = f(2) - log(2) = -159*log(2) - 1e-5
             (this guarantee that log_approx(y) is always <= -1e-5 < 0).
       - The maximum of abs(f(x)-log(x)+159*log(2)) is minimized.
    */
    x * (2.104659476859f + x * (-0.720478916626f + x * 0.107132064797f))

    /* Then, we add the term corresponding to the exponent, and
       additive constants. */
    + (-111.701724334061f + 0.6931471805f*exp);
}

/* This function regenerates [MT_STATE_SIZE] geometric random
   variables at once. Doing this by batches help us gain performances:
   many compilers (e.g., GCC, CLang, ICC) will be able to use SIMD
   instructions to get a performance boost.
*/
#ifdef SUPPORTS_TREE_VECTORIZE
__attribute__((optimize("tree-vectorize")))
#endif
static void rand_batch(void)
{
  int i;

  /* Instead of using temporary buffers, we could use one big loop,
     but it turns out SIMD optimizations of compilers are more fragile
     when using larger loops.  */
  static uint32_t A[RAND_BLOCK_SIZE];
  static float B[RAND_BLOCK_SIZE];

  CAMLassert(lambda > 0.);

  /* Shuffle the xoshiro samplers, and generate uniform variables in A. */
  for (i = 0; i < RAND_BLOCK_SIZE; i++)
    A[i] = xoshiro_next(i);

  /* Generate exponential random variables by computing logarithms. We
     do not use math.h library functions, which are slow and prevent
     compiler from using SIMD instructions. */
  for (i = 0; i < RAND_BLOCK_SIZE; i++)
    B[i] = 1 + log_approx(A[i]) * one_log1m_lambda;

  /* We do the final flooring for generating geometric
     variables. Compilers are unlikely to use SIMD instructions for
     this loop, because it involves a conditional and variables of
     different sizes (32 and 64 bits). */
  for (i = 0; i < RAND_BLOCK_SIZE; i++) {
    double f = B[i];
    CAMLassert (f >= 1);
    /* [Max_long+1] is a power of two => no rounding in the test. */
    if (f >= Max_long+1)
      rand_geom_buff[i] = Max_long;
    else rand_geom_buff[i] = (uintnat)f;
  }

  rand_pos = 0;
}

/* Simulate a geometric variable of parameter [lambda].
   The result is clipped in [1..Max_long] */
static uintnat rand_geom(void)
{
  uintnat res;
  CAMLassert(lambda > 0.);
  if (rand_pos == RAND_BLOCK_SIZE) rand_batch();
  res = rand_geom_buff[rand_pos++];
  CAMLassert(1 <= res && res <= Max_long);
  return res;
}

static uintnat next_rand_geom;
/* Simulate a binomial variable of parameters [len] and [lambda].
   This sampling algorithm has running time linear with [len *
   lambda].  We could use more a involved algorithm, but this should
   be good enough since, in the average use case, [lambda] <= 0.01 and
   therefore the generation of the binomial variable is amortized by
   the initialialization of the corresponding block.

   If needed, we could use algorithm BTRS from the paper:
     Hormann, Wolfgang. "The generation of binomial random variates."
     Journal of statistical computation and simulation 46.1-2 (1993), pp101-110.
 */
static uintnat rand_binom(uintnat len)
{
  uintnat res;
  CAMLassert(lambda > 0. && len < Max_long);
  for (res = 0; next_rand_geom < len; res++)
    next_rand_geom += rand_geom();
  next_rand_geom -= len;
  return res;
}

/**** Capturing the call stack *****/

/* This function is called in, e.g., [caml_alloc_shr], which
   guarantees that the GC is not called. Clients may use it in a
   context where the heap is in an invalid state, or when the roots
   are not properly registered. Therefore, we do not use [caml_alloc],
   which may call the GC, but prefer using [caml_alloc_shr], which
   gives this guarantee. The return value is either a valid callstack
   or 0 in out-of-memory scenarios. */
static value capture_callstack_postponed()
{
  value res;
  intnat callstack_len =
    caml_collect_current_callstack(&callstack_buffer, &callstack_buffer_len,
                                   callstack_size, -1);
  if (callstack_len == 0)
    return Atom(0);
  res = caml_alloc_shr_no_track_noexc(callstack_len, 0);
  if (res == 0)
    return Atom(0);
  memcpy(Op_val(res), callstack_buffer, sizeof(value) * callstack_len);
  if (callstack_buffer_len > 256 && callstack_buffer_len > callstack_len * 8) {
    caml_stat_free(callstack_buffer);
    callstack_buffer = NULL;
    callstack_buffer_len = 0;
  }
  return res;
}

/* In this version, we are allowed to call the GC, so we use
   [caml_alloc], which is more efficient since it uses the minor
   heap.
   Should be called with [local->suspended == 1] */
static value capture_callstack(int alloc_idx)
{
  value res;
  intnat callstack_len =
    caml_collect_current_callstack(&callstack_buffer, &callstack_buffer_len,
                                   callstack_size, alloc_idx);
  CAMLassert(local->suspended);
  res = caml_alloc(callstack_len, 0);
  memcpy(Op_val(res), callstack_buffer, sizeof(value) * callstack_len);
  if (callstack_buffer_len > 256 && callstack_buffer_len > callstack_len * 8) {
    caml_stat_free(callstack_buffer);
    callstack_buffer = NULL;
    callstack_buffer_len = 0;
  }
  return res;
}

/**** Managing data structures for tracked blocks. ****/

/* Reallocate the [ea] array if it is either too small or too
   large.
   [grow] is the number of free cells needed.
   Returns 1 if reallocation succeeded --[ea->alloc_len] is at
   least [ea->len+grow]--, and 0 otherwise. */
static int realloc_entries(struct entry_array* ea, uintnat grow)
{
  uintnat new_alloc_len, new_len = ea->len + grow;
  struct tracked* new_t;
  if (new_len <= ea->alloc_len &&
     (4*new_len >= ea->alloc_len || ea->alloc_len == ea->min_alloc_len))
    return 1;
  new_alloc_len = new_len * 2;
  if (new_alloc_len < ea->min_alloc_len)
    new_alloc_len = ea->min_alloc_len;
  new_t = caml_stat_resize_noexc(ea->t, new_alloc_len * sizeof(struct tracked));
  if (new_t == NULL) return 0;
  ea->t = new_t;
  ea->alloc_len = new_alloc_len;
  return 1;
}

#define Invalid_index (~(uintnat)0)

Caml_inline uintnat new_tracked(uintnat n_samples, uintnat wosize,
                                int source, int is_young,
                                value block, value user_data)
{
  struct tracked *t;
  if (!realloc_entries(&local->entries, 1))
    return Invalid_index;
  local->entries.len++;
  t = &local->entries.t[local->entries.len - 1];
  t->block = block;
  t->n_samples = n_samples;
  t->wosize = wosize;
  t->user_data = user_data;
  t->running = NULL;
  t->alloc_young = is_young;
  t->source = source;
  t->promoted = 0;
  t->deallocated = 0;
  t->cb_promote_called = t->cb_dealloc_called = 0;
  t->deleted = 0;
  return local->entries.len - 1;
}

static void mark_deleted(struct entry_array* ea, uintnat t_idx)
{
  struct tracked* t = &ea->t[t_idx];
  t->deleted = 1;
  t->user_data = Val_unit;
  t->block = Val_unit;
  if (t_idx < ea->delete_idx) ea->delete_idx = t_idx;
}

Caml_inline value run_callback_exn(
  struct entry_array* ea, uintnat t_idx, value cb, value param)
{
  struct tracked* t = &ea->t[t_idx];
  value res;
  CAMLassert(t->running == NULL);
  CAMLassert(lambda > 0.);

  local->callback_status = ea == &entries_global ? t_idx : CB_LOCAL;
  t->running = local;
  t->user_data = Val_unit;      /* Release root. */
  res = caml_callback_exn(cb, param);
  if (local->callback_status == CB_STOPPED) {
    /* Make sure this entry has not been removed by [caml_memprof_stop] */
    local->callback_status = CB_IDLE;
    return Is_exception_result(res) ? res : Val_unit;
  }
  /* The call above can move the tracked entry and thus invalidate
     [t_idx] and [t]. */
  if (ea == &entries_global) {
    CAMLassert(local->callback_status >= 0 && local->callback_status < ea->len);
    t_idx = local->callback_status;
    t = &ea->t[t_idx];
  }
  local->callback_status = CB_IDLE;
  CAMLassert(t->running == local);
  t->running = NULL;
  if (Is_exception_result(res) || res == Val_unit) {
    /* Callback raised an exception or returned None or (), discard
       this entry. */
    mark_deleted(ea, t_idx);
    return res;
  } else {
    /* Callback returned [Some _]. Store the value in [user_data]. */
    CAMLassert(!Is_exception_result(res) && Is_block(res) && Tag_val(res) == 0
               && Wosize_val(res) == 1);
    t->user_data = Field(res, 0);
    if (Is_block(t->user_data) && Is_young(t->user_data) &&
        t_idx < ea->young_idx)
      ea->young_idx = t_idx;

    // If the following condition are met:
    //   - we are running a promotion callback,
    //   - the corresponding block is deallocated,
    //   - another thread is running callbacks in
    //     [caml_memprof_handle_postponed_exn],
    // then [callback_idx] may have moved forward during this callback,
    // which means that we may forget to run the deallocation callback.
    // Hence, we reset [callback_idx] if appropriate.
    if (ea == &entries_global && t->deallocated && !t->cb_dealloc_called &&
        callback_idx > t_idx)
      callback_idx = t_idx;

    return Val_unit;
  }
}

/* Run the allocation callback for a given entry of the local entries array.
   This assumes that the corresponding [deleted] and
   [running] fields of the entry are both set to 0.
   Reentrancy is not a problem for this function, since other threads
   will use a different array for entries.
   The index of the entry will not change, except if [caml_memprof_stop] is
   called .
   Returns:
   - An exception result if the callback raised an exception
   - Val_long(0) == Val_unit == None otherwise
 */
static value run_alloc_callback_exn(uintnat t_idx)
{
  struct tracked* t = &local->entries.t[t_idx];
  value sample_info;

  CAMLassert(Is_block(t->block) || Is_placeholder(t->block) || t->deallocated);
  sample_info = caml_alloc_small(4, 0);
  Field(sample_info, 0) = Val_long(t->n_samples);
  Field(sample_info, 1) = Val_long(t->wosize);
  Field(sample_info, 2) = Val_long(t->source);
  Field(sample_info, 3) = t->user_data;
  return run_callback_exn(&local->entries, t_idx,
     t->alloc_young ? Alloc_minor(tracker) : Alloc_major(tracker), sample_info);
}

/* Remove any deleted entries from [ea], updating [ea->young_idx] and
   [callback_idx] if [ea == &entries_global]. */
static void flush_deleted(struct entry_array* ea)
{
  uintnat i, j;

  if (ea == NULL) return;

  j = i = ea->delete_idx;
  while (i < ea->len) {
    if (!ea->t[i].deleted) {
      struct caml_memprof_th_ctx* runner = ea->t[i].running;
      if (runner != NULL && runner->callback_status == i)
        runner->callback_status = j;
      ea->t[j] = ea->t[i];
      j++;
    }
    i++;
    if (ea->young_idx == i) ea->young_idx = j;
    if (ea == &entries_global && callback_idx == i) callback_idx = j;
  }
  ea->delete_idx = ea->len = j;
  CAMLassert(ea != &entries_global || callback_idx <= ea->len);
  CAMLassert(ea->young_idx <= ea->len);
  realloc_entries(ea, 0);
}

static void check_action_pending(void)
{
  if (local->suspended) return;
  if (callback_idx < entries_global.len || local->entries.len > 0)
    caml_set_action_pending();
}

void caml_memprof_set_suspended(int s)
{
  local->suspended = s;
  caml_memprof_renew_minor_sample();
  if (!s) check_action_pending();
}

/* In case of a thread context switch during a callback, this can be
   called in a reetrant way. */
value caml_memprof_handle_postponed_exn(void)
{
  value res = Val_unit;
  uintnat i;
  if (local->suspended) return Val_unit;
  if (callback_idx >= entries_global.len && local->entries.len == 0)
    return Val_unit;

  caml_memprof_set_suspended(1);

  for (i = 0; i < local->entries.len; i++) {
    /* We are the only thread allowed to modify [local->entries], so
       the indices cannot shift, but it is still possible that
       [caml_memprof_stop] got called during the callback,
       invalidating all the entries. */
    res = run_alloc_callback_exn(i);
    if (Is_exception_result(res)) goto end;
    if (local->entries.len == 0)
      goto end; /* [caml_memprof_stop] has been called. */
    if (local->entries.t[i].deleted) continue;
    if (realloc_entries(&entries_global, 1))
      /* Transfer the entry to the global array. */
      entries_global.t[entries_global.len++] = local->entries.t[i];
    mark_deleted(&local->entries, i);
  }

  while (callback_idx < entries_global.len) {
    struct tracked* t = &entries_global.t[callback_idx];

    if (t->deleted || t->running != NULL) {
      /* This entry is not ready. Ignore it. */
      callback_idx++;
    } else if (t->promoted && !t->cb_promote_called) {
      t->cb_promote_called = 1;
      res = run_callback_exn(&entries_global, callback_idx, Promote(tracker),
                             t->user_data);
      if (Is_exception_result(res)) goto end;
    } else if (t->deallocated && !t->cb_dealloc_called) {
      value cb = (t->promoted || !t->alloc_young) ?
        Dealloc_major(tracker) : Dealloc_minor(tracker);
      t->cb_dealloc_called = 1;
      res = run_callback_exn(&entries_global, callback_idx, cb, t->user_data);
      if (Is_exception_result(res)) goto end;
    } else {
      /* There is nothing more to do with this entry. */
      callback_idx++;
    }
  }

 end:
  flush_deleted(&local->entries);
  flush_deleted(&entries_global);
  /* We need to reset the suspended flag *after* flushing
     [local->entries] to make sure the floag is not set back to 1. */
  caml_memprof_set_suspended(0);
  return res;
}

/**** Handling weak and strong roots when the GC runs. ****/

typedef void (*ea_action)(struct entry_array*, void*);
struct call_on_entry_array_data { ea_action f; void *data; };
static void call_on_entry_array(struct caml_memprof_th_ctx* ctx, void *data)
{
  struct call_on_entry_array_data* closure = data;
  closure->f(&ctx->entries, closure->data);
}

static void entry_arrays_iter(ea_action f, void *data)
{
  struct call_on_entry_array_data closure = { f, data };
  f(&entries_global, data);
  caml_memprof_th_ctx_iter_hook(call_on_entry_array, &closure);
}

static void entry_array_oldify_young_roots(struct entry_array *ea, void *data)
{
  uintnat i;
  (void)data;
  /* This loop should always have a small number of iterations (when
     compared to the size of the minor heap), because the young_idx
     pointer should always be close to the end of the array. Indeed,
     it is only moved back when returning from a callback triggered by
     allocation or promotion, which can only happen for blocks
     allocated recently, which are close to the end of the
     [entries_global] array. */
  for (i = ea->young_idx; i < ea->len; i++)
    caml_oldify_one(ea->t[i].user_data, &ea->t[i].user_data);
}

void caml_memprof_oldify_young_roots(void)
{
  entry_arrays_iter(entry_array_oldify_young_roots, NULL);
}

static void entry_array_minor_update(struct entry_array *ea, void *data)
{
  uintnat i;
  (void)data;
  /* See comment in [entry_array_oldify_young_roots] for the number
     of iterations of this loop. */
  for (i = ea->young_idx; i < ea->len; i++) {
    struct tracked *t = &ea->t[i];
    CAMLassert(Is_block(t->block) || t->deleted || t->deallocated ||
               Is_placeholder(t->block));
    if (Is_block(t->block) && Is_young(t->block)) {
      if (Hd_val(t->block) == 0) {
        /* Block has been promoted */
        t->block = Field(t->block, 0);
        t->promoted = 1;
      } else {
        /* Block is dead */
        CAMLassert_young_header(Hd_val(t->block));
        t->block = Val_unit;
        t->deallocated = 1;
      }
    }
  }
  ea->young_idx = ea->len;
}

void caml_memprof_minor_update(void)
{
  if (callback_idx > entries_global.young_idx) {
    /* The entries after [entries_global.young_idx] will possibly get
       promoted. Hence, there might be pending promotion callbacks. */
    callback_idx = entries_global.young_idx;
    check_action_pending();
  }

  entry_arrays_iter(entry_array_minor_update, NULL);
}

static void entry_array_do_roots(struct entry_array *ea, void* data)
{
  scanning_action f = data;
  uintnat i;
  for (i = 0; i < ea->len; i++)
    f(ea->t[i].user_data, &ea->t[i].user_data);
}

void caml_memprof_do_roots(scanning_action f)
{
  entry_arrays_iter(entry_array_do_roots, f);
}

static void entry_array_clean_phase(struct entry_array *ea, void* data)
{
  uintnat i;
  (void)data;
  for (i = 0; i < ea->len; i++) {
    struct tracked *t = &ea->t[i];
    if (Is_block(t->block) && !Is_young(t->block)) {
      CAMLassert(Is_in_heap(t->block));
      CAMLassert(!t->alloc_young || t->promoted);
      if (Is_white_val(t->block)) {
        t->block = Val_unit;
        t->deallocated = 1;
      }
    }
  }
}

void caml_memprof_update_clean_phase(void)
{
  entry_arrays_iter(entry_array_clean_phase, NULL);
  callback_idx = 0;
  check_action_pending();
}

static void entry_array_invert(struct entry_array *ea, void *data)
{
  uintnat i;
  (void)data;
  for (i = 0; i < ea->len; i++)
    caml_invert_root(ea->t[i].block, &ea->t[i].block);
}

void caml_memprof_invert_tracked(void)
{
  entry_arrays_iter(entry_array_invert, NULL);
}

/**** Sampling procedures ****/

static void maybe_track_block(value block, uintnat n_samples,
                              uintnat wosize, int src)
{
  value callstack;
  if (n_samples == 0) return;

  callstack = capture_callstack_postponed();
  if (callstack == 0) return;

  new_tracked(n_samples, wosize, src, Is_young(block), block, callstack);
  check_action_pending();
}

void caml_memprof_track_alloc_shr(value block)
{
  CAMLassert(Is_in_heap(block));
  if (lambda == 0 || local->suspended) return;

  maybe_track_block(block, rand_binom(Whsize_val(block)),
                    Wosize_val(block), SRC_NORMAL);
}

void caml_memprof_track_custom(value block, mlsize_t bytes)
{
  CAMLassert(Is_young(block) || Is_in_heap(block));
  if (lambda == 0 || local->suspended) return;

  maybe_track_block(block, rand_binom(Wsize_bsize(bytes)),
                    Wsize_bsize(bytes), SRC_CUSTOM);
}

/* Shifts the next sample in the minor heap by [n] words. Essentially,
   this tells the sampler to ignore the next [n] words of the minor
   heap. */
static void shift_sample(uintnat n)
{
  if (caml_memprof_young_trigger - Caml_state->young_alloc_start > n)
    caml_memprof_young_trigger -= n;
  else
    caml_memprof_young_trigger = Caml_state->young_alloc_start;
  caml_update_young_limit();
}

/* Renew the next sample in the minor heap. This needs to be called
   after each minor sampling and after each minor collection. In
   practice, this is called at each sampling in the minor heap and at
   each minor collection. Extra calls do not change the statistical
   properties of the sampling because of the memorylessness of the
   geometric distribution. */
void caml_memprof_renew_minor_sample(void)
{
  if (lambda == 0 || local->suspended)
    /* No trigger in the current minor heap. */
    caml_memprof_young_trigger = Caml_state->young_alloc_start;
  else {
    uintnat geom = rand_geom();
    if (Caml_state->young_ptr - Caml_state->young_alloc_start < geom)
      /* No trigger in the current minor heap. */
      caml_memprof_young_trigger = Caml_state->young_alloc_start;
    else
      caml_memprof_young_trigger = Caml_state->young_ptr - (geom - 1);
  }

  caml_update_young_limit();
}

/* Called when exceeding the threshold for the next sample in the
   minor heap, from the C code (the handling is different when called
   from natively compiled OCaml code). */
void caml_memprof_track_young(uintnat wosize, int from_caml,
                              int nallocs, unsigned char* encoded_alloc_lens)
{
  uintnat whsize = Whsize_wosize(wosize);
  value callstack, res = Val_unit;
  int alloc_idx = 0, i, allocs_sampled = 0;
  intnat alloc_ofs, trigger_ofs;
  double saved_lambda = lambda;

  /* If this condition is false, then [caml_memprof_young_trigger] should be
     equal to [Caml_state->young_alloc_start]. But this function is only
     called with [Caml_state->young_alloc_start <= Caml_state->young_ptr <
     caml_memprof_young_trigger], which is contradictory. */
  CAMLassert(!local->suspended && lambda > 0);

  if (!from_caml) {
    unsigned n_samples = 1 +
      rand_binom(caml_memprof_young_trigger - 1 - Caml_state->young_ptr);
    CAMLassert(encoded_alloc_lens == NULL);    /* No Comballoc in C! */
    caml_memprof_renew_minor_sample();
    maybe_track_block(Val_hp(Caml_state->young_ptr), n_samples,
                      wosize, SRC_NORMAL);
    return;
  }

  /* We need to call the callbacks for this sampled block. Since each
     callback can potentially allocate, the sampled block will *not*
     be the one pointed to by [caml_memprof_young_trigger]. Instead,
     we remember that we need to sample the next allocated word,
     call the callback and use as a sample the block which will be
     allocated right after the callback. */

  CAMLassert(Caml_state->young_ptr < caml_memprof_young_trigger &&
             caml_memprof_young_trigger <= Caml_state->young_ptr + whsize);
  trigger_ofs = caml_memprof_young_trigger - Caml_state->young_ptr;
  alloc_ofs = whsize;

  /* Restore the minor heap in a valid state for calling the callbacks.
     We should not call the GC before these two instructions. */
  Caml_state->young_ptr += whsize;
  caml_memprof_set_suspended(1); // This also updates the memprof trigger

  /* Perform the sampling of the block in the set of Comballoc'd
     blocks, insert them in the entries array, and run the
     callbacks. */
  for (alloc_idx = nallocs - 1; alloc_idx >= 0; alloc_idx--) {
    unsigned alloc_wosz = encoded_alloc_lens == NULL ? wosize :
      Wosize_encoded_alloc_len(encoded_alloc_lens[alloc_idx]);
    unsigned n_samples = 0;
    alloc_ofs -= Whsize_wosize(alloc_wosz);
    while (alloc_ofs < trigger_ofs) {
      n_samples++;
      trigger_ofs -= rand_geom();
    }
    if (n_samples > 0) {
      uintnat t_idx;
      int stopped;

      callstack = capture_callstack(alloc_idx);
      t_idx = new_tracked(n_samples, alloc_wosz, SRC_NORMAL, 1,
                          Placeholder_offs(alloc_ofs), callstack);
      if (t_idx == Invalid_index) continue;
      res = run_alloc_callback_exn(t_idx);
      /* Has [caml_memprof_stop] been called during the callback? */
      stopped = local->entries.len == 0;
      if (stopped) {
        allocs_sampled = 0;
        if (saved_lambda != lambda) {
          /* [lambda] changed during the callback. We need to refresh
             [trigger_ofs]. */
          saved_lambda = lambda;
          trigger_ofs = lambda == 0. ? 0 : alloc_ofs - (rand_geom() - 1);
        }
      }
      if (Is_exception_result(res)) break;
      if (!stopped) allocs_sampled++;
    }
  }

  CAMLassert(alloc_ofs == 0 || Is_exception_result(res));
  CAMLassert(allocs_sampled <= nallocs);

  if (!Is_exception_result(res)) {
    /* The callbacks did not raise. The allocation will take place.
       We now restore the minor heap in the state needed by
       [Alloc_small_aux]. */
    if (Caml_state->young_ptr - whsize < Caml_state->young_trigger) {
      CAML_EV_COUNTER(EV_C_FORCE_MINOR_MEMPROF, 1);
      caml_gc_dispatch();
    }

    /* Re-allocate the blocks in the minor heap. We should not call the
       GC after this. */
    Caml_state->young_ptr -= whsize;

    /* Make sure this block is not going to be sampled again. */
    shift_sample(whsize);
  }

  /* Since [local->entries] is local to the current thread, we know for
     sure that the allocated entries are the [alloc_sampled] last entries of
     [local->entries]. */

  for (i = 0; i < allocs_sampled; i++) {
    uintnat idx = local->entries.len-allocs_sampled+i;
    if (local->entries.t[idx].deleted) continue;
    if (realloc_entries(&entries_global, 1)) {
      /* Transfer the entry to the global array. */
      struct tracked* t = &entries_global.t[entries_global.len];
      entries_global.len++;
      *t = local->entries.t[idx];

      if (Is_exception_result(res)) {
        /* The allocations are cancelled because of the exception,
           but this callback has already been called. We simulate a
           deallocation. */
        t->block = Val_unit;
        t->deallocated = 1;
      } else {
        /* If the execution of the callback has succeeded, then we start the
           tracking of this block..

           Subtlety: we are actually writing [t->block] with an invalid
           (uninitialized) block. This is correct because the allocation
           and initialization happens right after returning from
           [caml_memprof_track_young]. */
        t->block = Val_hp(Caml_state->young_ptr + Offs_placeholder(t->block));

        /* We make sure that the action pending flag is not set
           systematically, which is to be expected, since we created
           a new block in the global entry array, but this new block
           does not need promotion or deallocationc callback. */
        if (callback_idx == entries_global.len - 1)
          callback_idx = entries_global.len;
      }
    }
    mark_deleted(&local->entries, idx);
  }

  flush_deleted(&local->entries);
  /* We need to reset the suspended flag *after* flushing
     [local->entries] to make sure the floag is not set back to 1. */
  caml_memprof_set_suspended(0);

  if (Is_exception_result(res))
    caml_raise(Extract_exception(res));

  /* /!\ Since the heap is in an invalid state before initialization,
     very little heap operations are allowed until then. */

  return;
}

void caml_memprof_track_interned(header_t* block, header_t* blockend)
{
  header_t *p;
  value callstack = 0;
  int is_young = Is_young(Val_hp(block));

  if (lambda == 0 || local->suspended) return;

  p = block;
  while (1) {
    uintnat next_sample = rand_geom();
    header_t *next_sample_p, *next_p;
    if (next_sample > blockend - p)
      break;
    /* [next_sample_p] is the block *following* the next sampled
       block! */
    next_sample_p = p + next_sample;

    while (1) {
      next_p = p + Whsize_hp(p);
      if (next_p >= next_sample_p) break;
      p = next_p;
    }

    if (callstack == 0) callstack = capture_callstack_postponed();
    if (callstack == 0) break;  /* OOM */
    new_tracked(rand_binom(next_p - next_sample_p) + 1,
                Wosize_hp(p), SRC_MARSHAL, is_young, Val_hp(p), callstack);
    p = next_p;
  }
  check_action_pending();
}

/**** Interface with the OCaml code. ****/

static void caml_memprof_init(void)
{
  init = 1;
  xoshiro_init();
}

CAMLprim value caml_memprof_start(value lv, value szv, value tracker_param)
{
  CAMLparam3(lv, szv, tracker_param);

  double l = Double_val(lv);
  intnat sz = Long_val(szv);

  if (started) caml_failwith("Gc.Memprof.start: already started.");

  if (sz < 0 || !(l >= 0.) || l > 1.) /* Checks that [l] is not NAN. */
    caml_invalid_argument("Gc.Memprof.start");

  if (!init) caml_memprof_init();

  lambda = l;
  if (l > 0) {
    one_log1m_lambda = l == 1 ? 0 : 1/caml_log1p(-l);
    rand_pos = RAND_BLOCK_SIZE;
    /* next_rand_geom can be zero if the next word is to be sampled,
       but rand_geom always returns a value >= 1. Subtract 1 to correct. */
    next_rand_geom = rand_geom() - 1;
  }

  caml_memprof_renew_minor_sample();

  callstack_size = sz;
  started = 1;

  tracker = tracker_param;
  caml_register_generational_global_root(&tracker);

  CAMLreturn(Val_unit);
}

static void empty_entry_array(struct entry_array *ea) {
  if (ea != NULL) {
    ea->alloc_len = ea->len = ea->young_idx = ea->delete_idx = 0;
    caml_stat_free(ea->t);
    ea->t = NULL;
  }
}

static void th_ctx_memprof_stop(struct caml_memprof_th_ctx* ctx, void* data)
{
  (void)data;
  if (ctx->callback_status != CB_IDLE) ctx->callback_status = CB_STOPPED;
  empty_entry_array(&ctx->entries);
}

CAMLprim value caml_memprof_stop(value unit)
{
  if (!started) caml_failwith("Gc.Memprof.stop: not started.");

  /* Discard the tracked blocks in the global entries array. */
  empty_entry_array(&entries_global);

  /* Discard the tracked blocks in the local entries array,
     and set [callback_status] to [CB_STOPPED]. */
  caml_memprof_th_ctx_iter_hook(th_ctx_memprof_stop, NULL);

  callback_idx = 0;

  lambda = 0;
  // Reset the memprof trigger in order to make sure we won't enter
  // [caml_memprof_track_young].
  caml_memprof_renew_minor_sample();
  started = 0;

  caml_remove_generational_global_root(&tracker);

  caml_stat_free(callstack_buffer);
  callstack_buffer = NULL;
  callstack_buffer_len = 0;

  return Val_unit;
}

/**** Interface with systhread. ****/

static void th_ctx_iter_default(th_ctx_action f, void* data) {
  f(local, data);
}

CAMLexport void (*caml_memprof_th_ctx_iter_hook)(th_ctx_action, void*)
  = th_ctx_iter_default;

CAMLexport struct caml_memprof_th_ctx* caml_memprof_new_th_ctx()
{
  struct caml_memprof_th_ctx* ctx =
    caml_stat_alloc(sizeof(struct caml_memprof_th_ctx));
  ctx->suspended = 0;
  ctx->callback_status = CB_IDLE;
  ctx->entries.t = NULL;
  ctx->entries.min_alloc_len = MIN_ENTRIES_LOCAL_ALLOC_LEN;
  ctx->entries.alloc_len = ctx->entries.len = 0;
  ctx->entries.young_idx = ctx->entries.delete_idx = 0;
  return ctx;
}

CAMLexport void caml_memprof_delete_th_ctx(struct caml_memprof_th_ctx* ctx)
{
  if (ctx->callback_status >= 0)
    /* A callback is running in this thread from the global entries
       array. We delete the corresponding entry. */
    mark_deleted(&entries_global, ctx->callback_status);
  if (ctx == local) local = NULL;
  caml_stat_free(ctx->entries.t);
  if (ctx != &caml_memprof_main_ctx) caml_stat_free(ctx);
}

CAMLexport void caml_memprof_leave_thread(void)
{
  local = NULL;
}

CAMLexport void caml_memprof_enter_thread(struct caml_memprof_th_ctx* ctx)
{
  CAMLassert(local == NULL);
  local = ctx;
  caml_memprof_set_suspended(ctx->suspended);
}
