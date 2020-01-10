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

#include <math.h>
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

#define MT_STATE_SIZE 624

static uint32_t mt_state[MT_STATE_SIZE];
static uint32_t mt_index;

/* [lambda] is the mean number of samples for each allocated word (including
   block headers). */
static double lambda = 0;
 /* Precomputed value of [1/log(1-lambda)], for fast sampling of
    geometric distribution.
    Dummy if [lambda = 0]. */
static double one_log1m_lambda;

int caml_memprof_suspended = 0;
static intnat callstack_size;

static value callback_alloc_minor, callback_alloc_major,
  callback_promote, callback_dealloc_minor, callback_dealloc_major;

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

/**** Statistical sampling ****/

static double mt_generate_uniform(void)
{
  int i;
  uint32_t y;

  /* Mersenne twister PRNG */
  if (mt_index == MT_STATE_SIZE) {
    for (i = 0; i < 227; i++) {
      y = (mt_state[i] & 0x80000000) + (mt_state[i+1] & 0x7fffffff);
      mt_state[i] = mt_state[i+397] ^ (y >> 1) ^ ((-(y&1)) & 0x9908b0df);
    }
    for (i = 227; i < MT_STATE_SIZE - 1; i++) {
      y = (mt_state[i] & 0x80000000) + (mt_state[i+1] & 0x7fffffff);
      mt_state[i] = mt_state[i-227] ^ (y >> 1) ^ ((-(y&1)) & 0x9908b0df);
    }
    y = (mt_state[MT_STATE_SIZE - 1] & 0x80000000) + (mt_state[0] & 0x7fffffff);
    mt_state[MT_STATE_SIZE - 1] =
      mt_state[396] ^ (y >> 1) ^ ((-(y&1)) & 0x9908b0df);
    mt_index = 0;
  }

  y = mt_state[mt_index];
  y = y ^ (y >> 11);
  y = y ^ ((y << 7) & 0x9d2c5680);
  y = y ^ ((y << 15) & 0xefc60000);
  y = y ^ (y >> 18);

  mt_index++;
  return y*2.3283064365386962890625e-10 + /* 2^-32 */
          1.16415321826934814453125e-10; /* 2^-33 */
}

/* Simulate a geometric variable of parameter [lambda].
   The result is clipped in [1..Max_long]
   Requires [lambda > 0]. */
static uintnat mt_generate_geom(void)
{
  /* We use the float versions of exp/log, since these functions are
     significantly faster, and we really don't need much precision
     here. The entropy contained in [next_mt_generate_geom] is anyway
     bounded by the entropy provided by [mt_generate_uniform], which
     is 32bits. */
  double res = 1 + logf(mt_generate_uniform()) * one_log1m_lambda;
  if (res > Max_long) return Max_long;
  return (uintnat)res;
}

static uintnat next_mt_generate_geom;
/* Simulate a binomial variable of parameters [len] and [lambda].
   This sampling algorithm has running time linear with [len *
   lambda].  We could use more a involved algorithm, but this should
   be good enough since, in the average use case, [lambda] <= 0.01 and
   therefore the generation of the binomial variable is amortized by
   the initialialization of the corresponding block.

   If needed, we could use algorithm BTRS from the paper:
     Hormann, Wolfgang. "The generation of binomial random variates."
     Journal of statistical computation and simulation 46.1-2 (1993), pp101-110.

   Requires [lambda > 0] and [len < Max_long].
 */
static uintnat mt_generate_binom(uintnat len)
{
  uintnat res;
  for (res = 0; next_mt_generate_geom < len; res++)
    next_mt_generate_geom += mt_generate_geom();
  next_mt_generate_geom -= len;
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
static value capture_callstack_postponed(void)
{
  value res;
  uintnat wosize = caml_current_callstack_size(callstack_size);
  if (wosize == 0) return Atom(0);
  res = caml_alloc_shr_no_track_noexc(wosize, 0);
  if (res != 0) caml_current_callstack_write(res);
  return res;
}

/* In this version, we are allowed to call the GC, so we use
   [caml_alloc], which is more efficient since it uses the minor
   heap.
   Should be called with [caml_memprof_suspended == 1] */
static value capture_callstack(void)
{
  value res;
  uintnat wosize = caml_current_callstack_size(callstack_size);
  CAMLassert(caml_memprof_suspended);
  res = caml_alloc(wosize, 0);
  caml_current_callstack_write(res);
  return res;
}

/**** Data structures for tracked blocks. ****/

/* During the alloc callback for a minor allocation, the block being
   sampled is not yet allocated. Instead, it's represented as this. */
#define Placeholder_value (Val_long(0x42424242))

/* When an entry is deleted, its index is replaced by that integer. */
#define Invalid_index (~(uintnat)0)

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

static inline uintnat new_tracked(uintnat n_samples, uintnat wosize,
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
static inline value run_callback_exn(uintnat *t_idx, value cb, value param) {
  struct tracked* t = &trackst.entries[*t_idx];
  value res;
  CAMLassert(!t->callback_running && t->idx_ptr == NULL);

  t->callback_running = 1;
  t->idx_ptr = t_idx;
  res = caml_callback_exn(cb, param);
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
               || t->block == Placeholder_value
               || t->deallocated);
    sample_info = caml_alloc_small(4, 0);
    Field(sample_info, 0) = Val_long(t->n_samples);
    Field(sample_info, 1) = Val_long(t->wosize);
    Field(sample_info, 2) = Val_long(t->unmarshalled);
    Field(sample_info, 3) = t->user_data;
    t->user_data = Val_unit;
    res = run_callback_exn(t_idx,
        t->alloc_young ? callback_alloc_minor : callback_alloc_major,
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
    res = run_callback_exn(t_idx, callback_promote, user_data);
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
      callback_dealloc_major : callback_dealloc_minor;
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

void caml_memprof_check_action_pending(void) {
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
  caml_memprof_check_action_pending();  /* Needed in case of an exception */
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
               t->block == Placeholder_value);
    if (Is_block(t->block) && Is_young(t->block)) {
      if (Hd_val(t->block) == 0) {
        /* Block has been promoted */
        t->block = Field(t->block, 0);
        t->promoted = 1;
      } else {
        /* Block is dead */
        t->block = Val_unit;
        t->deallocated = 1;
      }
    }
  }
  if (trackst.callback > trackst.young) {
    trackst.callback = trackst.young;
    caml_memprof_check_action_pending();
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
  caml_memprof_check_action_pending();
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

  n_samples = mt_generate_binom(Whsize_val(block));
  if (n_samples == 0) return;

  callstack = capture_callstack_postponed();
  if (callstack == 0) return;

  new_tracked(n_samples, Wosize_val(block), 0, 0, block, callstack);
  caml_memprof_check_action_pending();
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
    uintnat geom = mt_generate_geom();
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
void caml_memprof_track_young(uintnat wosize, int from_caml)
{
  uintnat whsize = Whsize_wosize(wosize), n_samples;
  uintnat t_idx;
  value callstack, res;

  if (caml_memprof_suspended) {
    caml_memprof_renew_minor_sample();
    return;
  }

  /* If [lambda == 0], then [caml_memprof_young_trigger] should be
     equal to [Caml_state->young_alloc_start]. But this function is only
     called with [Caml_state->young_alloc_start <= Caml_state->young_ptr <
     caml_memprof_young_trigger], which is contradictory. */
  CAMLassert(lambda > 0);

  n_samples = 1 +
    mt_generate_binom(caml_memprof_young_trigger - 1 - Caml_state->young_ptr);

  if (!from_caml) {
    caml_memprof_renew_minor_sample();

    callstack = capture_callstack_postponed();
    if (callstack == 0) return;

    new_tracked(n_samples, wosize,
                0, 1, Val_hp(Caml_state->young_ptr), callstack);
    caml_memprof_check_action_pending();
    return;
  }

  /* We need to call the callback for this sampled block. Since the
     callback can potentially allocate, the sampled block will *not*
     be the one pointed to by [caml_memprof_young_trigger]. Instead,
     we remember that we need to sample the next allocated word,
     call the callback and use as a sample the block which will be
     allocated right after the callback. */

  /* Restore the minor heap in a valid state for calling the callback.
     We should not call the GC before these two instructions. */
  Caml_state->young_ptr += whsize;
  caml_memprof_renew_minor_sample();

  caml_memprof_suspended = 1;
  callstack = capture_callstack();
  t_idx = new_tracked(n_samples, wosize, 0, 1, Placeholder_value, callstack);
  if (t_idx == Invalid_index)
    res = Val_unit;
  else
    res = handle_entry_callbacks_exn(&t_idx);
  caml_memprof_suspended = 0;
  caml_memprof_check_action_pending();
  /* We need to call [caml_memprof_check_action_pending] since we
     reset [caml_memprof_suspended] to 0 (a GC collection may have
     triggered some new callback).

     We need to make sure that the action pending flag is not set
     systematically, which is to be expected, since [new_tracked]
     created a new block without updating
     [trackst.callback]. Fortunately, [handle_entry_callback_exn]
     increments [trackst.callback] if it is equal to [t_idx]. */

  /* We can now restore the minor heap in the state needed by
     [Alloc_small_aux]. */
  if (Caml_state->young_ptr - whsize < Caml_state->young_trigger) {
    CAML_INSTR_INT("force_minor/memprof@", 1);
    caml_gc_dispatch();
  }

  /* This condition happens either in the case of an exception or if
     the callback returned [None]. If these cases happen frequently,
     then we need to call [flush_deleted] somewhere to prevent a
     leak. */
  if (t_idx == Invalid_index)
    flush_deleted();

  caml_raise_if_exception(res);

  /* Re-allocate the block in the minor heap. We should not call the
     GC after this. */
  Caml_state->young_ptr -= whsize;

  /* Make sure this block is not going to be sampled again. */
  shift_sample(whsize);

  if (t_idx != Invalid_index) {
    /* If the execution of the callback has succeeded, then we start the
       tracking of this block..

       Subtlety: we are actually writing [t->block] with an invalid
       (uninitialized) block. This is correct because the allocation
       and initialization happens right after returning from
       [caml_memprof_track_young]. */
    trackst.entries[t_idx].block = Val_hp(Caml_state->young_ptr);
    CAMLassert(trackst.entries[t_idx].cb_alloc_called);
    if (t_idx < trackst.young) trackst.young = t_idx;
  }

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
    uintnat next_sample = mt_generate_geom();
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
    new_tracked(mt_generate_binom(next_p - next_sample_p) + 1,
                Wosize_hp(p), 1, is_young, Val_hp(p), callstack);
    p = next_p;
  }
  caml_memprof_check_action_pending();
}

/**** Interface with the OCaml code. ****/

static void caml_memprof_init(void) {
  uintnat i;

  init = 1;

  mt_index = MT_STATE_SIZE;
  mt_state[0] = 42;
  for (i = 1; i < MT_STATE_SIZE; i++)
    mt_state[i] = 0x6c078965 * (mt_state[i-1] ^ (mt_state[i-1] >> 30)) + i;
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
}

CAMLprim value caml_memprof_start(value lv, value szv,
                                  value cb_alloc_minor, value cb_alloc_major,
                                  value cb_promote,
                                  value cb_dealloc_minor,
                                  value cb_dealloc_major)
{
  CAMLparam5(lv, szv, cb_alloc_minor, cb_alloc_major, cb_promote);
  CAMLxparam2(cb_dealloc_minor, cb_dealloc_major);
  double l = Double_val(lv);
  intnat sz = Long_val(szv);

  if (started) caml_failwith("Gc.Memprof.start: already started.");

  if (sz < 0 || !(l >= 0.) || l > 1.) /* Checks that [l] is not NAN. */
    caml_invalid_argument("Gc.Memprof.start");

  if (!init) caml_memprof_init();

  lambda = l;
  if (l > 0) {
    one_log1m_lambda = l == 1 ? 0 : 1/caml_log1p(-l);
    next_mt_generate_geom = mt_generate_geom();
  }

  caml_memprof_renew_minor_sample();

  callstack_size = sz;
  started = 1;


  callback_alloc_minor = cb_alloc_minor;
  callback_alloc_major = cb_alloc_major;
  callback_promote = cb_promote;
  callback_dealloc_minor = cb_dealloc_minor;
  callback_dealloc_major = cb_dealloc_major;

  caml_register_generational_global_root(&callback_alloc_minor);
  caml_register_generational_global_root(&callback_alloc_major);
  caml_register_generational_global_root(&callback_promote);
  caml_register_generational_global_root(&callback_dealloc_minor);
  caml_register_generational_global_root(&callback_dealloc_major);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_memprof_start_byt(value* argv, int argn)
{
  CAMLassert(argn == 7);
  return caml_memprof_start(argv[0], argv[1], argv[2], argv[3],
                            argv[4], argv[5], argv[6]);
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

  caml_remove_generational_global_root(&callback_alloc_minor);
  caml_remove_generational_global_root(&callback_alloc_major);
  caml_remove_generational_global_root(&callback_promote);
  caml_remove_generational_global_root(&callback_dealloc_minor);
  caml_remove_generational_global_root(&callback_dealloc_major);

  return Val_unit;
}
