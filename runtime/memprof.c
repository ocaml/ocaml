/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Jacques-Henri Joudan, projet Gallium, INRIA Paris          */
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

static uint32_t mt_state[624];
static uint32_t mt_index;

/* [lambda] is the mean number of samples for each allocated word (including
   block headers). */
static double lambda = 0;
 /* Precomputed value of [1/log(1-lambda)], for fast sampling of
    geometric distribution.
    Dummy if [lambda = 0]. */
static double one_log1m_lambda;

int caml_memprof_suspended = 0;
static intnat callstack_size = 0;
static value memprof_callback = Val_unit;

/* Pointer to the word following the next sample in the minor
   heap. Equals [caml_young_alloc_start] if no sampling is planned in
   the current minor heap.
   Invariant: [caml_memprof_young_trigger <= caml_young_ptr].
 */
value* caml_memprof_young_trigger;

/* Whether memprof has been initialized.  */
static int init = 0;

/**** Statistical sampling ****/

static double mt_generate_uniform(void)
{
  int i;
  uint32_t y;

  /* Mersenne twister PRNG */
  if (mt_index == 624) {
    for(i = 0; i < 227; i++) {
      y = (mt_state[i] & 0x80000000) + (mt_state[i+1] & 0x7fffffff);
      mt_state[i] = mt_state[i+397] ^ (y >> 1) ^ ((-(y&1)) & 0x9908b0df);
    }
    for(i = 227; i < 623; i++) {
      y = (mt_state[i] & 0x80000000) + (mt_state[i+1] & 0x7fffffff);
      mt_state[i] = mt_state[i-227] ^ (y >> 1) ^ ((-(y&1)) & 0x9908b0df);
    }
    y = (mt_state[623] & 0x80000000) + (mt_state[0] & 0x7fffffff);
    mt_state[623] = mt_state[396] ^ (y >> 1) ^ ((-(y&1)) & 0x9908b0df);
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
static uintnat mt_generate_geom()
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

static uintnat next_mt_generate_binom;
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
  for(res = 0; next_mt_generate_binom < len; res++)
    next_mt_generate_binom += mt_generate_geom();
  next_mt_generate_binom -= len;
  return res;
}

/**** Interface with the OCaml code. ****/

static void purge_postponed_queue(void);

CAMLprim value caml_memprof_set(value v)
{
  CAMLparam1(v);
  double l = Double_val(Field(v, 0));
  intnat sz = Long_val(Field(v, 1));

  if (sz < 0 || !(l >= 0.) || l > 1.) /* Checks that [l] is not NAN. */
    caml_invalid_argument("caml_memprof_set");

  /* This call to [caml_memprof_set] may stop sampling or change the
     callback. We have to make sure that the postponed queue is empty
     before continuing. */
  if (!caml_memprof_suspended)
    caml_memprof_handle_postponed();
  else
    /* But if we are currently running a callback, there is nothing
       else we can do than purging the queue. */
    purge_postponed_queue();

  if (!init) {
    int i;
    init = 1;

    mt_index = 624;
    mt_state[0] = 42;
    for(i = 1; i < 624; i++)
      mt_state[i] = 0x6c078965 * (mt_state[i-1] ^ (mt_state[i-1] >> 30)) + i;

    caml_register_generational_global_root(&memprof_callback);
  }

  lambda = l;
  if (l > 0) {
    one_log1m_lambda = l == 1 ? 0 : 1/caml_log1p(-l);
    next_mt_generate_binom = mt_generate_geom();
  }

  caml_memprof_renew_minor_sample();

  callstack_size = sz;

  caml_modify_generational_global_root(&memprof_callback, Field(v, 2));

  CAMLreturn(Val_unit);
}

/* Cf. Gc.Memprof.alloc_kind */
enum ml_alloc_kind {
  Minor = Val_long(0),
  Major = Val_long(1),
  Serialized = Val_long(2)
};

/* When we call do_callback, we suspend/resume sampling. In order to
   to avoid a systematic unnecessary calls to [caml_check_urgent_gc]
   after each memprof callback, we do not set [caml_something_to_do]
   when resuming. Therefore, any call to [do_callback] has to also
   make sure the postponed queue will be handled fully at some
   point. */
static value do_callback(tag_t tag, uintnat wosize, uintnat occurrences,
                         caml_callstack* callstack, enum ml_alloc_kind cb_kind) {
  CAMLparam0();
  CAMLlocal2(sample_info, vcallstack);
  value res; /* Not a root, can be an exception result. */
  CAMLassert(occurrences > 0 && !caml_memprof_suspended);

  caml_memprof_suspended = 1;

  vcallstack = caml_alloc(callstack->length, 0);
  caml_write_callstack(callstack, vcallstack);
  caml_free_callstack(callstack);

  sample_info = caml_alloc_small(5, 0);
  Field(sample_info, 0) = Val_long(occurrences);
  Field(sample_info, 1) = cb_kind;
  Field(sample_info, 2) = Val_long(tag);
  Field(sample_info, 3) = Val_long(wosize);
  Field(sample_info, 4) = vcallstack;

  res = caml_callback_exn(memprof_callback, sample_info);

  caml_memprof_suspended = 0;

  if (Is_exception_result(res))
    /* We are not necessarily called from `caml_check_urgent_gc`, but
       this is OK to call this regardless of this fact.  */
    caml_raise_in_async_callback(Extract_exception(res));

  CAMLreturn(res);
}

/**** Handling postponed sampled blocks. ****/
/* When allocating in from C code, we cannot call the callback,
   because the [caml_alloc_***] are guaranteed not to do so. These
   functions make it possible to register a sampled block in a
   todo-list so that the callback call is performed when possible. */
/* Note: the shorter the delay is, the better, because the block is
   linked to a root during the delay, so that the reachability
   properties of the sampled block are artificially modified. */

#define POSTPONED_DEFAULT_QUEUE_SIZE 16
static struct postponed_block {
  value block;
  caml_callstack callstack;
  uintnat occurrences;
  enum ml_alloc_kind kind;
} default_postponed_queue[POSTPONED_DEFAULT_QUEUE_SIZE],
  *postponed_queue = default_postponed_queue,
  *postponed_queue_end = default_postponed_queue + POSTPONED_DEFAULT_QUEUE_SIZE,
  *postponed_tl = default_postponed_queue, /* Pointer to next pop */
  *postponed_hd = default_postponed_queue; /* Pointer to next push */
int caml_memprof_to_do = 0;

static void postponed_pop(void)
{
  caml_remove_global_root(&postponed_tl->block);
  postponed_tl++;
  if (postponed_tl == postponed_queue_end) postponed_tl = postponed_queue;
}

static void purge_postponed_queue(void)
{
  while (postponed_tl != postponed_hd) postponed_pop();
  if (postponed_queue != default_postponed_queue) {
    caml_stat_free(postponed_queue);
    postponed_queue = default_postponed_queue;
    postponed_queue_end = postponed_queue + POSTPONED_DEFAULT_QUEUE_SIZE;
  }
  postponed_hd = postponed_tl = postponed_queue;
}

/* This function does not call the GC. This is important since it is
   called when allocating a block using [caml_alloc_shr]: The new
   block is allocated, but not yet initialized, so that the heap
   invariants are broken. */
static void register_postponed_callback(value block, uintnat occurrences,
                                        enum ml_alloc_kind kind)
{
  struct postponed_block* new_hd;
  if (occurrences == 0) return;

  new_hd = postponed_hd + 1;
  if (new_hd == postponed_queue_end) new_hd = postponed_queue;
  if (new_hd == postponed_tl) {
    /* Queue is full, reallocate it. (We always leave one free slot in
       order to be able to distinguish the 100% full and the empty
       states). */
    uintnat sz = 4 * (postponed_queue_end - postponed_queue);
    struct postponed_block* new_queue =
      caml_stat_alloc_noexc(sz * sizeof(struct postponed_block));
    if (new_queue == NULL) return;
    new_hd = new_queue;
    while (postponed_tl != postponed_hd) {
      *new_hd = *postponed_tl;
      caml_register_global_root(&new_hd->block);
      new_hd++;
      postponed_pop();
    }
    if (postponed_queue != default_postponed_queue)
      caml_stat_free(postponed_queue);
    postponed_tl = postponed_queue = new_queue;
    postponed_hd = new_hd;
    postponed_queue_end = postponed_queue + sz;
    new_hd++;
  }

  postponed_hd->block = block;
  caml_register_global_root(&postponed_hd->block);
  caml_collect_current_callstack(callstack_size, &postponed_hd->callstack);
  postponed_hd->occurrences = occurrences;
  postponed_hd->kind = kind;
  postponed_hd = new_hd;

  if (!caml_memprof_suspended) caml_set_something_to_do();
}

void caml_memprof_handle_postponed(void)
{
  CAMLparam0();
  CAMLlocal1(block);
  value ephe;

  if (caml_memprof_suspended) {
    caml_memprof_to_do = 0;
    CAMLreturn0;
  }

  while (postponed_tl != postponed_hd) {
    caml_callstack* callstack = &postponed_tl->callstack;
    uintnat occurrences = postponed_tl->occurrences;
    enum ml_alloc_kind kind = postponed_tl->kind;
    block = postponed_tl->block;
    postponed_pop();

    /* If using threads, this call can trigger reentrant calls to
       [caml_memprof_handle_postponed] even though we set
       [caml_memprof_suspended]. */
    ephe = do_callback(Tag_val(block), Wosize_val(block),
                       occurrences, callstack, kind);

    if (Is_block(ephe)) caml_ephemeron_set_key(Field(ephe, 0), 0, block);
    if (postponed_tl == postponed_hd) purge_postponed_queue();
  }

  caml_memprof_to_do = 0;
  CAMLreturn0;
}

/**** Sampling procedures ****/

void caml_memprof_track_alloc_shr(value block)
{
  CAMLassert(Is_in_heap(block));
  /* This test also makes sure memprof is initialized. */
  if (lambda == 0 || caml_memprof_suspended) return;
  register_postponed_callback(
      block, mt_generate_binom(Whsize_val(block)), Major);
}

/* Shifts the next sample in the minor heap by [n] words. Essentially,
   this tells the sampler to ignore the next [n] words of the minor
   heap. */
static void shift_sample(uintnat n)
{
  if (caml_memprof_young_trigger - caml_young_alloc_start > n)
    caml_memprof_young_trigger -= n;
  else
    caml_memprof_young_trigger = caml_young_alloc_start;
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
    caml_memprof_young_trigger = caml_young_alloc_start;
  else {
    uintnat geom = mt_generate_geom();
    if (caml_young_ptr - caml_young_alloc_start < geom)
      /* No trigger in the current minor heap. */
      caml_memprof_young_trigger = caml_young_alloc_start;
    caml_memprof_young_trigger = caml_young_ptr - (geom - 1);
  }

  caml_update_young_limit();
}

/* Called when exceeding the threshold for the next sample in the
   minor heap, from the C code (the handling is different when called
   from natively compiled OCaml code). */
void caml_memprof_track_young(tag_t tag, uintnat wosize, int from_caml)
{
  CAMLparam0();
  CAMLlocal1(ephe);
  uintnat whsize = Whsize_wosize(wosize);
  uintnat occurrences;
  caml_callstack callstack;

  if (caml_memprof_suspended) {
    caml_memprof_renew_minor_sample();
    CAMLreturn0;
  }

  /* If [lambda == 0], then [caml_memprof_young_trigger] should be
     equal to [caml_young_alloc_start]. But this function is only
     called with [caml_young_alloc_start <= caml_young_ptr <
     caml_memprof_young_trigger], which is contradictory. */
  CAMLassert(lambda > 0);

  occurrences =
    mt_generate_binom(caml_memprof_young_trigger - 1 - caml_young_ptr) + 1;

  if (!from_caml) {
    register_postponed_callback(Val_hp(caml_young_ptr), occurrences, Minor);
    caml_memprof_renew_minor_sample();
    CAMLreturn0;
  }

  /* We need to call the callback for this sampled block. Since the
     callback can potentially allocate, the sampled block will *not*
     be the one pointed to by [caml_memprof_young_trigger]. Instead,
     we remember that we need to sample the next allocated word,
     call the callback and use as a sample the block which will be
     allocated right after the callback. */

  /* Restore the minor heap in a valid state for calling the callback.
     We should not call the GC before these two instructions. */
  caml_young_ptr += whsize;
  caml_memprof_renew_minor_sample();

  /* Empty the queue to make sure callbacks are called in the right
     order. */
  caml_memprof_handle_postponed();

  caml_collect_current_callstack(callstack_size, &callstack);
  ephe = do_callback(tag, wosize, occurrences, &callstack, Minor);

  /* We can now restore the minor heap in the state needed by
     [Alloc_small_aux]. */
  if (caml_young_ptr - whsize < caml_young_trigger) {
    CAML_INSTR_INT ("force_minor/memprof@", 1);
    caml_gc_dispatch();
  }

  /* Re-allocate the block in the minor heap. We should not call the
     GC after this. */
  caml_young_ptr -= whsize;

  /* Make sure this block is not going to be sampled again. */
  shift_sample(whsize);

  /* Write the ephemeron if not [None]. */
  if (Is_block(ephe)) {
    /* Subtlety: we are actually writing the ephemeron with an invalid
       (uninitialized) block. This is correct for two reasons:
          - The logic of [caml_ephemeron_set_key] never inspects the content of
            the block. In only checks that the block is young.
          - The allocation and initialization happens right after returning
            from [caml_memprof_track_young]. */
    caml_ephemeron_set_key(Field(ephe, 0), 0, Val_hp(caml_young_ptr));
  }

  /* /!\ Since the heap is in an invalid state before initialization,
     very little heap operations are allowed until then. */

  CAMLreturn0;
}
