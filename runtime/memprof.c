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

/* [caml_memprof_suspended] is used for masking memprof callbacks when
   a callback is running or when an uncaught exception handler is
   called. */
int caml_memprof_suspended = 0;

/* [callback_running] is used to trigger a fatal error whenever
   [Thread.exit] is called from a callback. */
static int callback_running = 0;

static intnat callstack_size;

/* accessors for the OCaml type [Gc.Memprof.tracker],
   which is the type of the [tracker] global below. */
#define Alloc_minor(tracker) (Field(tracker, 0))
#define Alloc_major(tracker) (Field(tracker, 1))
#define Promote(tracker) (Field(tracker, 2))
#define Dealloc_minor(tracker) (Field(tracker, 3))
#define Dealloc_major(tracker) (Field(tracker, 4))

static value tracker;

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

Caml_inline uint64_t splitmix64_next(uint64_t* x) {
  uint64_t z = (*x += 0x9E3779B97F4A7C15ull);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
  return z ^ (z >> 31);
}

static void xoshiro_init(void) {
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

Caml_inline uint32_t xoshiro_next(int i) {
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
Caml_inline float log_approx(uint32_t y) {
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
static void rand_batch(void) {
  int i;

  /* Instead of using temporary buffers, we could use one big loop,
     but it turns out SIMD optimizations of compilers are more fragile
     when using larger loops.  */
  static uint32_t A[RAND_BLOCK_SIZE];
  static float B[RAND_BLOCK_SIZE];

  CAMLassert(lambda > 0.);

  /* Shuffle the xoshiro samplers, and generate uniform variables in A. */
  for(i = 0; i < RAND_BLOCK_SIZE; i++)
    A[i] = xoshiro_next(i);

  /* Generate exponential random variables by computing logarithms. We
     do not use math.h library functions, which are slow and prevent
     compiler from using SIMD instructions. */
  for(i = 0; i < RAND_BLOCK_SIZE; i++)
    B[i] = 1 + log_approx(A[i]) * one_log1m_lambda;

  /* We do the final flooring for generating geometric
     variables. Compilers are unlikely to use SIMD instructions for
     this loop, because it involves a conditional and variables of
     different sizes (32 and 64 bits). */
  for(i = 0; i < RAND_BLOCK_SIZE; i++) {
    double f = B[i];
    CAMLassert (f >= 1);
    if(f > Max_long) rand_geom_buff[i] = Max_long;
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
  if(rand_pos == RAND_BLOCK_SIZE) rand_batch();
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
   Should be called with [caml_memprof_suspended == 1] */
static value capture_callstack(int alloc_idx)
{
  value res;
  intnat callstack_len =
    caml_collect_current_callstack(&callstack_buffer, &callstack_buffer_len,
                                   callstack_size, alloc_idx);
  CAMLassert(caml_memprof_suspended);
  res = caml_alloc(callstack_len, 0);
  memcpy(Op_val(res), callstack_buffer, sizeof(value) * callstack_len);
  if (callstack_buffer_len > 256 && callstack_buffer_len > callstack_len * 8) {
    caml_stat_free(callstack_buffer);
    callstack_buffer = NULL;
    callstack_buffer_len = 0;
  }
  return res;
}

/**** Data structures for tracked blocks. ****/

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

  /* Whether this block has been initially allocated in the minor heap. */
  unsigned int alloc_young : 1;

  /* Whether this block comes from unmarshalling. */
  unsigned int unmarshalled : 1;

  /* Whether this block has been promoted. Implies [alloc_young]. */
  unsigned int promoted : 1;

  /* Whether this block has been deallocated. */
  unsigned int deallocated : 1;

  /* Whether the allocation callback has been called. */
  unsigned int cb_alloc_called : 1;

  /* Whether the promotion callback has been called. */
  unsigned int cb_promote_called : 1;

  /* Whether the deallocation callback has been called. */
  unsigned int cb_dealloc_called : 1;

  /* Whether this entry is deleted. */
  unsigned int deleted : 1;

  /* Whether a callback is currently running for this entry. */
  unsigned int callback_running : 1;

  /* Pointer to the [t_idx] variable in the [run_callback] frame which
     is currently running the callback for this entry. This is needed
     to make [run_callback] reetrant, in the case it is called
     simultaneously by several threads. */
  uintnat* idx_ptr;
};

/* During the alloc callback for a minor allocation, the block being
   sampled is not yet allocated. Instead, we place in the block field
   a value computed with the following macro: */
#define Placeholder_magic 0x04200000
#define Placeholder_offs(offset) (Val_long(offset + Placeholder_magic))
#define Offs_placeholder(block) (Long_val(block) & 0xFFFF)
#define Is_placeholder(block) \
  (Is_long(block) && (Long_val(block) & ~(uintnat)0xFFFF) == Placeholder_magic)

/* When an entry is deleted, its index is replaced by that integer. */
#define Invalid_index (~(uintnat)0)


static struct tracking_state {
  struct tracked* entries;
  /* The allocated capacity of the entries array */
  uintnat alloc_len;
  /* The number of active entries. (len <= alloc_len) */
  uintnat len;
  /* Before this position, the [block] and [user_data] fields point to
     the major heap (young <= len). */
  uintnat young;
  /* There are no pending callbacks before this position (callback <= len). */
  uintnat callback;
  /* There are no blocks to be deleted before this position */
  uintnat delete;
} trackst;

#define MIN_TRACKST_ALLOC_LEN 128


/* Reallocate the [trackst] array if it is either too small or too
   large.
   Returns 1 if reallocation succeeded --[trackst.alloc_len] is at
   least [trackst.len]--, and 0 otherwise. */
static int realloc_trackst(void) {
  uintnat new_alloc_len;
  struct tracked* new_entries;
  if (trackst.len <= trackst.alloc_len &&
     (4*trackst.len >= trackst.alloc_len ||
      trackst.alloc_len == MIN_TRACKST_ALLOC_LEN))
    return 1;
  new_alloc_len = trackst.len * 2;
  if (new_alloc_len < MIN_TRACKST_ALLOC_LEN)
    new_alloc_len = MIN_TRACKST_ALLOC_LEN;
  new_entries = caml_stat_resize_noexc(trackst.entries,
      new_alloc_len * sizeof(struct tracked));
  if (new_entries == NULL) return 0;
  trackst.entries = new_entries;
  trackst.alloc_len = new_alloc_len;
  return 1;
}

Caml_inline uintnat new_tracked(uintnat n_samples, uintnat wosize,
                                int is_unmarshalled, int is_young,
                                value block, value user_data)
{
  struct tracked *t;
  trackst.len++;
  if (!realloc_trackst()) {
    trackst.len--;
    return Invalid_index;
  }
  t = &trackst.entries[trackst.len - 1];
  t->block = block;
  t->n_samples = n_samples;
  t->wosize = wosize;
  t->user_data = user_data;
  t->idx_ptr = NULL;
  t->alloc_young = is_young;
  t->unmarshalled = is_unmarshalled;
  t->promoted = 0;
  t->deallocated = 0;
  t->cb_alloc_called = t->cb_promote_called = t->cb_dealloc_called = 0;
  t->deleted = 0;
  t->callback_running = 0;
  return trackst.len - 1;
}

static void mark_deleted(uintnat t_idx)
{
  struct tracked* t = &trackst.entries[t_idx];
  t->deleted = 1;
  t->user_data = Val_unit;
  t->block = Val_unit;
  if (t_idx < trackst.delete) trackst.delete = t_idx;
  CAMLassert(t->idx_ptr == NULL);
}

/* The return value is an exception or [Val_unit] iff [*t_idx] is set to
   [Invalid_index]. In this case, the entry is deleted.
   Otherwise, the return value is a [Some(...)] block. */
Caml_inline value run_callback_exn(uintnat *t_idx, value cb, value param) {
  struct tracked* t = &trackst.entries[*t_idx];
  value res;
  CAMLassert(!t->callback_running && t->idx_ptr == NULL);
  CAMLassert(lambda > 0.);

  callback_running = t->callback_running = 1;
  t->idx_ptr = t_idx;
  res = caml_callback_exn(cb, param);
  callback_running = 0;
  /* The call above can modify [*t_idx] and thus invalidate [t]. */
  if (*t_idx == Invalid_index) {
    /* Make sure this entry has not been removed by [caml_memprof_set] */
    return Val_unit;
  }
  t = &trackst.entries[*t_idx];
  t->idx_ptr = NULL;
  t->callback_running = 0;
  if (Is_exception_result(res) || res == Val_unit) {
    /* Callback raised an exception or returned None or (), discard
       this entry. */
    mark_deleted(*t_idx);
    *t_idx = Invalid_index;
  }
  return res;
}

/* Run all the needed callbacks for a given entry.
   In case of a thread context switch during a callback, this can be
   called in a reetrant way.
   If [*t_idx] equals [trackst.callback], then this function
   increments [trackst.callback].
   The index of the entry may change. It is set to [Invalid_index] if
   the entry is discarded.
   Returns:
   - An exception result if the callback raised an exception
   - Val_long(0) == Val_unit == None otherwise
 */
static value handle_entry_callbacks_exn(uintnat* t_idx)
{
  value sample_info, res, user_data;    /* No need to make these roots */
  struct tracked* t = &trackst.entries[*t_idx];
  if (*t_idx == trackst.callback) trackst.callback++;

  if (t->deleted || t->callback_running) return Val_unit;

  if (!t->cb_alloc_called) {
    t->cb_alloc_called = 1;
    CAMLassert(Is_block(t->block)
               || Is_placeholder(t->block)
               || t->deallocated);
    sample_info = caml_alloc_small(4, 0);
    Field(sample_info, 0) = Val_long(t->n_samples);
    Field(sample_info, 1) = Val_long(t->wosize);
    Field(sample_info, 2) = Val_long(t->unmarshalled);
    Field(sample_info, 3) = t->user_data;
    t->user_data = Val_unit;
    res = run_callback_exn(t_idx,
        t->alloc_young ? Alloc_minor(tracker) : Alloc_major(tracker),
        sample_info);
    if (*t_idx == Invalid_index)
      return res;
    CAMLassert(!Is_exception_result(res) && Is_block(res) && Tag_val(res) == 0
               && Wosize_val(res) == 1);
    t = &trackst.entries[*t_idx];
    t->user_data = Field(res, 0);
    if (Is_block(t->user_data) && Is_young(t->user_data) &&
        *t_idx < trackst.young)
      trackst.young = *t_idx;
  }

  if (t->promoted && !t->cb_promote_called) {
    t->cb_promote_called = 1;
    user_data = t->user_data;
    t->user_data = Val_unit;
    res = run_callback_exn(t_idx, Promote(tracker), user_data);
    if (*t_idx == Invalid_index)
      return res;
    CAMLassert(!Is_exception_result(res) && Is_block(res) && Tag_val(res) == 0
               && Wosize_val(res) == 1);
    t = &trackst.entries[*t_idx];
    t->user_data = Field(res, 0);
    if (Is_block(t->user_data) && Is_young(t->user_data) &&
        *t_idx < trackst.young)
      trackst.young = *t_idx;
  }

  if (t->deallocated && !t->cb_dealloc_called) {
    value cb = (t->promoted || !t->alloc_young) ?
      Dealloc_major(tracker) : Dealloc_minor(tracker);
    t->cb_dealloc_called = 1;
    user_data = t->user_data;
    t->user_data = Val_unit;
    res = run_callback_exn(t_idx, cb, user_data);
    /* [t] is invalid, but we do no longer use it. */
    CAMLassert(*t_idx == Invalid_index);
    CAMLassert(Is_exception_result(res) || res == Val_unit);
    return res;
  }

  return Val_unit;
}

/* Remove any deleted entries, updating callback and young */
static void flush_deleted(void)
{
  uintnat i = trackst.delete, j = i;
  while (i < trackst.len) {
    if (!trackst.entries[i].deleted) {
      if (trackst.entries[i].idx_ptr != NULL)
        *trackst.entries[i].idx_ptr = j;
      trackst.entries[j] = trackst.entries[i];
      j++;
    }
    i++;
    if (trackst.young == i) trackst.young = j;
    if (trackst.callback == i) trackst.callback = j;
  }
  trackst.delete = trackst.len = j;
  CAMLassert(trackst.callback <= trackst.len);
  CAMLassert(trackst.young <= trackst.len);
  realloc_trackst();
}

static void check_action_pending(void) {
  if (!caml_memprof_suspended && trackst.callback < trackst.len)
    caml_set_action_pending();
}

/* In case of a thread context switch during a callback, this can be
   called in a reetrant way. */
value caml_memprof_handle_postponed_exn(void)
{
  value res = Val_unit;
  if (caml_memprof_suspended) return res;
  caml_memprof_suspended = 1;
  while (trackst.callback < trackst.len) {
    uintnat i = trackst.callback;
    res = handle_entry_callbacks_exn(&i);
    if (Is_exception_result(res)) break;
  }
  caml_memprof_suspended = 0;
  check_action_pending();  /* Needed in case of an exception */
  flush_deleted();
  return res;
}

void caml_memprof_oldify_young_roots(void)
{
  uintnat i;
  /* This loop should always have a small number of iteration (when
     compared to the size of the minor heap), because the young
     pointer should always be close to the end of the array. Indeed,
     it is only moved back when returning from a callback triggered by
     allocation or promotion, which can only happen for blocks
     allocated recently, which are close to the end of the trackst
     array. */
  for (i = trackst.young; i < trackst.len; i++)
    caml_oldify_one(trackst.entries[i].user_data,
                    &trackst.entries[i].user_data);
}

void caml_memprof_minor_update(void)
{
  uintnat i;
  /* See comment in [caml_memprof_oldify_young_roots] for the number
     of iterations of this loop. */
  for (i = trackst.young; i < trackst.len; i++) {
    struct tracked *t = &trackst.entries[i];
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
  if (trackst.callback > trackst.young) {
    trackst.callback = trackst.young;
    check_action_pending();
  }
  trackst.young = trackst.len;
}

void caml_memprof_do_roots(scanning_action f)
{
  uintnat i;
  for (i = 0; i < trackst.len; i++)
    f(trackst.entries[i].user_data, &trackst.entries[i].user_data);
}

void caml_memprof_update_clean_phase(void)
{
  uintnat i;
  for (i = 0; i < trackst.len; i++) {
    struct tracked *t = &trackst.entries[i];
    if (Is_block(t->block) && !Is_young(t->block)) {
      CAMLassert(Is_in_heap(t->block));
      CAMLassert(!t->alloc_young || t->promoted);
      if (Is_white_val(t->block)) {
        t->block = Val_unit;
        t->deallocated = 1;
      }
    }
  }
  trackst.callback = 0;
  check_action_pending();
}

void caml_memprof_invert_tracked(void)
{
  uintnat i;
  for (i = 0; i < trackst.len; i++)
    caml_invert_root(trackst.entries[i].block, &trackst.entries[i].block);
}

/**** Sampling procedures ****/

void caml_memprof_track_alloc_shr(value block)
{
  uintnat n_samples;
  value callstack = 0;
  CAMLassert(Is_in_heap(block));

  /* This test also makes sure memprof is initialized. */
  if (lambda == 0 || caml_memprof_suspended) return;

  n_samples = rand_binom(Whsize_val(block));
  if (n_samples == 0) return;

  callstack = capture_callstack_postponed();
  if (callstack == 0) return;

  new_tracked(n_samples, Wosize_val(block), 0, 0, block, callstack);
  check_action_pending();
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

  if (lambda == 0) /* No trigger in the current minor heap. */
    caml_memprof_young_trigger = Caml_state->young_alloc_start;
  else {
    uintnat geom = rand_geom();
    if (Caml_state->young_ptr - Caml_state->young_alloc_start < geom)
      /* No trigger in the current minor heap. */
      caml_memprof_young_trigger = Caml_state->young_alloc_start;
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
  int alloc_idx = 0, i, allocs_sampled = 0, has_delete = 0;
  intnat alloc_ofs, trigger_ofs;
  /* usually, only one allocation is sampled, even when the block contains
     multiple combined allocations. So, we delay allocating the full
     sampled_allocs array until we discover we actually need two entries */
  uintnat first_idx, *idx_tab = &first_idx;
  double saved_lambda = lambda;

  if (caml_memprof_suspended) {
    caml_memprof_renew_minor_sample();
    return;
  }

  /* If [lambda == 0], then [caml_memprof_young_trigger] should be
     equal to [Caml_state->young_alloc_start]. But this function is only
     called with [Caml_state->young_alloc_start <= Caml_state->young_ptr <
     caml_memprof_young_trigger], which is contradictory. */
  CAMLassert(lambda > 0);

  if (!from_caml) {
    unsigned n_samples = 1 +
      rand_binom(caml_memprof_young_trigger - 1 - Caml_state->young_ptr);
    CAMLassert(encoded_alloc_lens == NULL);    /* No Comballoc in C! */
    caml_memprof_renew_minor_sample();

    callstack = capture_callstack_postponed();
    if (callstack == 0) return;

    new_tracked(n_samples, wosize,
                0, 1, Val_hp(Caml_state->young_ptr), callstack);
    check_action_pending();
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
  caml_memprof_renew_minor_sample();
  caml_memprof_suspended = 1;

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
      uintnat *idx_ptr, t_idx;

      callstack = capture_callstack(alloc_idx);
      t_idx = new_tracked(n_samples, alloc_wosz,
                          0, 1, Placeholder_offs(alloc_ofs), callstack);
      if (t_idx == Invalid_index) continue;
      res = handle_entry_callbacks_exn(&t_idx);
      if (t_idx == Invalid_index) {
        has_delete = 1;
        if (saved_lambda != lambda) {
          /* [lambda] changed during the callback. We need to refresh
             [trigger_ofs]. */
          saved_lambda = lambda;
          trigger_ofs = lambda == 0. ? 0 : alloc_ofs - (rand_geom() - 1);
        }
      }
      if (Is_exception_result(res)) break;
      if (t_idx == Invalid_index) continue;

      if (allocs_sampled == 1) {
        /* Found a second sampled allocation! Allocate a buffer for them */
        idx_tab = caml_stat_alloc_noexc(sizeof(uintnat) * nallocs);
        if (idx_tab == NULL) {
          alloc_ofs = 0;
          idx_tab = &first_idx;
          break;
        }
        idx_tab[0] = first_idx;
        if (idx_tab[0] != Invalid_index)
          trackst.entries[idx_tab[0]].idx_ptr = &idx_tab[0];
      }

      /* Usually, trackst.entries[...].idx_ptr is owned by the thread
         running a callback for the entry, if any. Here, we take ownership
         of idx_ptr until the end of the function.

         This does not conflict with the usual use of idx_ptr because no
         callbacks can run on this entry until the end of the function:
         the allocation callback has already run and the other callbacks
         do not run on Placeholder values */
      idx_ptr = &idx_tab[allocs_sampled];
      *idx_ptr = t_idx;
      trackst.entries[*idx_ptr].idx_ptr = idx_ptr;
      allocs_sampled++;
    }
  }

  CAMLassert(alloc_ofs == 0 || Is_exception_result(res));
  CAMLassert(allocs_sampled <= nallocs);
  caml_memprof_suspended = 0;
  check_action_pending();
  /* We need to call [check_action_pending] since we
     reset [caml_memprof_suspended] to 0 (a GC collection may have
     triggered some new callback).

     We need to make sure that the action pending flag is not set
     systematically, which is to be expected, since [new_tracked]
     created a new block without updating
     [trackst.callback]. Fortunately, [handle_entry_callback_exn]
     increments [trackst.callback] if it is equal to [t_idx]. */

  /* This condition happens either in the case of an exception or if
     one of the callbacks returned [None]. If these cases happen
     frequently, then we need to call [flush_deleted] somewhere to
     prevent a leak. */
  if (has_delete)
    flush_deleted();

  if (Is_exception_result(res)) {
    for (i = 0; i < allocs_sampled; i++)
      if (idx_tab[i] != Invalid_index) {
        struct tracked* t = &trackst.entries[idx_tab[i]];
        /* The allocations are cancelled because of the exception,
           but this callback has already been called. We simulate a
           deallocation. */
        t->block = Val_unit;
        t->deallocated = 1;
        if (trackst.callback > idx_tab[i]) {
          trackst.callback = idx_tab[i];
          check_action_pending();
        }
      }
    if (idx_tab != &first_idx) caml_stat_free(idx_tab);
    caml_raise(Extract_exception(res));
  }

  /* We can now restore the minor heap in the state needed by
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

  for (i = 0; i < allocs_sampled; i++) {
    if (idx_tab[i] != Invalid_index) {
      /* If the execution of the callback has succeeded, then we start the
         tracking of this block..

         Subtlety: we are actually writing [t->block] with an invalid
         (uninitialized) block. This is correct because the allocation
         and initialization happens right after returning from
         [caml_memprof_track_young]. */
      struct tracked *t = &trackst.entries[idx_tab[i]];
      t->block = Val_hp(Caml_state->young_ptr + Offs_placeholder(t->block));
      t->idx_ptr = NULL;
      CAMLassert(t->cb_alloc_called);
      if (idx_tab[i] < trackst.young) trackst.young = idx_tab[i];
    }
  }
  if (idx_tab != &first_idx) caml_stat_free(idx_tab);

  /* /!\ Since the heap is in an invalid state before initialization,
     very little heap operations are allowed until then. */

  return;
}

void caml_memprof_track_interned(header_t* block, header_t* blockend) {
  header_t *p;
  value callstack = 0;
  int is_young = Is_young(Val_hp(block));

  if (lambda == 0 || caml_memprof_suspended)
    return;

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
                Wosize_hp(p), 1, is_young, Val_hp(p), callstack);
    p = next_p;
  }
  check_action_pending();
}

/**** Interface with the OCaml code. ****/

static void caml_memprof_init(void) {
  init = 1;
  xoshiro_init();
}

void caml_memprof_shutdown(void) {
  init = 0;
  started = 0;
  lambda = 0.;
  caml_memprof_suspended = 0;
  trackst.len = 0;
  trackst.callback = trackst.young = trackst.delete = 0;
  caml_stat_free(trackst.entries);
  trackst.entries = NULL;
  trackst.alloc_len = 0;
  caml_stat_free(callstack_buffer);
  callstack_buffer = NULL;
  callstack_buffer_len = 0;
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
    next_rand_geom = rand_geom();
  }

  caml_memprof_renew_minor_sample();

  callstack_size = sz;
  started = 1;

  tracker = tracker_param;
  caml_register_generational_global_root(&tracker);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_memprof_stop(value unit)
{
  uintnat i;

  if (!started) caml_failwith("Gc.Memprof.stop: not started.");

  /* This call to [caml_memprof_stop] will discard all the previously
     tracked blocks. We try one last time to call the postponed
     callbacks. */
  caml_raise_if_exception(caml_memprof_handle_postponed_exn());

  /* Discard the tracked blocks. */
  for (i = 0; i < trackst.len; i++)
    if (trackst.entries[i].idx_ptr != NULL)
      *trackst.entries[i].idx_ptr = Invalid_index;
  trackst.len = 0;
  trackst.callback = trackst.young = trackst.delete = 0;
  caml_stat_free(trackst.entries);
  trackst.entries = NULL;
  trackst.alloc_len = 0;

  lambda = 0;
  caml_memprof_renew_minor_sample();
  started = 0;

  caml_remove_generational_global_root(&tracker);

  caml_stat_free(callstack_buffer);
  callstack_buffer = NULL;
  callstack_buffer_len = 0;

  return Val_unit;
}

/**** Interface with systhread. ****/

void caml_memprof_init_th_ctx(struct caml_memprof_th_ctx* ctx) {
  ctx->suspended = 0;
  ctx->callback_running = 0;
}

void caml_memprof_stop_th_ctx(struct caml_memprof_th_ctx* ctx) {
  /* Make sure that no memprof callback is being executed in this
     thread. If so, memprof data structures may have pointers to the
     thread's stack. */
  if(ctx->callback_running)
    caml_fatal_error("Thread.exit called from a memprof callback.");
}

void caml_memprof_save_th_ctx(struct caml_memprof_th_ctx* ctx) {
  ctx->suspended = caml_memprof_suspended;
  ctx->callback_running = callback_running;
}

void caml_memprof_restore_th_ctx(const struct caml_memprof_th_ctx* ctx) {
  caml_memprof_suspended = ctx->suspended;
  callback_running = ctx->callback_running;
  check_action_pending();
}
