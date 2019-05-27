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
  if(res > Max_long) return Max_long;
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

CAMLprim value caml_memprof_set(value v)
{
  CAMLparam1(v);
  double l = Double_val(Field(v, 0));
  intnat sz = Long_val(Field(v, 1));

  if(sz < 0 || !(l >= 0.) || l > 1.) /* Checks that [l] is not NAN. */
    caml_invalid_argument("caml_memprof_set");

  if(!init) {
    int i;
    init = 1;

    mt_index = 624;
    mt_state[0] = 42;
    for(i = 1; i < 624; i++)
      mt_state[i] = 0x6c078965 * (mt_state[i-1] ^ (mt_state[i-1] >> 30)) + i;

    caml_register_generational_global_root(&memprof_callback);
  }

  lambda = l;
  if(l > 0) {
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

static value do_callback(tag_t tag, uintnat wosize, uintnat occurrences,
                         value callstack, enum ml_alloc_kind cb_kind) {
  CAMLparam1(callstack);
  CAMLlocal1(sample_info);
  CAMLassert(occurrences > 0);

  sample_info = caml_alloc_small(5, 0);
  Field(sample_info, 0) = Val_long(occurrences);
  Field(sample_info, 1) = cb_kind;
  Field(sample_info, 2) = Val_long(tag);
  Field(sample_info, 3) = Val_long(wosize);
  Field(sample_info, 4) = callstack;

  CAMLreturn(caml_callback_exn(memprof_callback, sample_info));
}

/**** Sampling procedures ****/

static value capture_callstack_major()
{
  value res;
  uintnat wosize = caml_current_callstack_size(callstack_size);
  /* We do not use [caml_alloc] to make sure the GC will not get called. */
  if(wosize == 0) return Atom (0);
  res = caml_alloc_shr_no_track_noexc(wosize, 0);
  if(res != 0) caml_current_callstack_write(res);
  return res;
}

static value capture_callstack_minor()
{
  value res;
  uintnat wosize = caml_current_callstack_size(callstack_size);
  CAMLassert(caml_memprof_suspended); /* => no samples in the call stack. */
  res = caml_alloc(wosize, 0);
  caml_current_callstack_write(res);
  return res;
}

struct caml_memprof_postponed_block {
  value block;
  value callstack;
  uintnat occurrences;
  struct caml_memprof_postponed_block* next;
} *caml_memprof_postponed_head = NULL;

/* When allocating in the major heap, we cannot call the callback,
   because [caml_alloc_shr] is guaranteed not to call the GC. Hence,
   this function determines if the block needs to be sampled, and if
   so, it registers the block in the todo-list so that the callback
   call is performed when possible. */
void caml_memprof_track_alloc_shr(value block)
{
  uintnat occurrences;
  CAMLassert(Is_in_heap(block));
  /* This test also makes sure memprof is initialized. */
  if(lambda == 0 || caml_memprof_suspended)
    return;
  occurrences = mt_generate_binom(Whsize_val(block));
  if(occurrences > 0) {
    value callstack;
    struct caml_memprof_postponed_block* pb =
      caml_stat_alloc_noexc(sizeof(struct caml_memprof_postponed_block));
    if(pb == NULL) return;
    callstack = capture_callstack_major();
    if(callstack == 0) {
      caml_stat_free(pb);
      return;
    }
    pb->block = block;
    caml_register_generational_global_root(&pb->block);
    pb->callstack = callstack;
    caml_register_generational_global_root(&pb->callstack);
    pb->occurrences = occurrences;
    pb->next = caml_memprof_postponed_head;
    caml_memprof_postponed_head = pb;
#ifndef NATIVE_CODE
    caml_something_to_do = 1;
#else
    caml_young_limit = caml_young_alloc_end;
#endif
  }
}

void caml_memprof_handle_postponed()
{
  struct caml_memprof_postponed_block *p, *q;
  value ephe;

  if(caml_memprof_postponed_head == NULL)
    return;

  /* We first reverse the list */
  p = caml_memprof_postponed_head;
  q = caml_memprof_postponed_head->next;
  p->next = NULL;
  while(q != NULL) {
    struct caml_memprof_postponed_block* next = q->next;
    q->next = p;
    p = q;
    q = next;
  }
  caml_memprof_postponed_head = NULL;
  caml_update_young_limit();

#define NEXT_P \
  { struct caml_memprof_postponed_block* next = p->next;   \
    caml_remove_generational_global_root(&p->callstack);   \
    caml_remove_generational_global_root(&p->block);       \
    caml_stat_free(p);                                     \
    p = next; }

  caml_memprof_suspended = 1;
  /* We then do the actual iteration on postponed blocks */
  while(p != NULL) {
    ephe = do_callback(Tag_val(p->block), Wosize_val(p->block),
                       p->occurrences, p->callstack, Major);
    if (Is_exception_result(ephe)) {
      caml_memprof_suspended = 0;
      /* In the case of an exception, we just forget the entire list. */
      while(p != NULL) NEXT_P;
      caml_raise(Extract_exception(ephe));
    }
    if(Is_block(ephe))
      caml_ephemeron_set_key(Field(ephe, 0), 0, p->block);
    NEXT_P;
  }
  caml_memprof_suspended = 0;
}

/* Shifts the next sample in the minor heap by [n] words. Essentially,
   this tells the sampler to ignore the next [n] words of the minor
   heap. */
static void shift_sample(uintnat n)
{
  if(caml_memprof_young_trigger - caml_young_alloc_start > n)
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

  if(lambda == 0) /* No trigger in the current minor heap. */
    caml_memprof_young_trigger = caml_young_alloc_start;
  else {
    uintnat geom = mt_generate_geom();
    if(caml_young_ptr - caml_young_alloc_start < geom)
      /* No trigger in the current minor heap. */
      caml_memprof_young_trigger = caml_young_alloc_start;
    caml_memprof_young_trigger = caml_young_ptr - (geom - 1);
  }

  caml_update_young_limit();
}

/* Called when exceeding the threshold for the next sample in the
   minor heap, from the C code (the handling is different when called
   from natively compiled OCaml code). */
void caml_memprof_track_young(tag_t tag, uintnat wosize)
{
  CAMLparam0();
  CAMLlocal2(ephe, callstack);
  uintnat whsize = Whsize_wosize(wosize);
  uintnat occurrences;

  if(caml_memprof_suspended) {
    caml_memprof_renew_minor_sample();
    CAMLreturn0;
  }

  /* If [lambda == 0], then [caml_memprof_young_trigger] should be
     equal to [caml_young_alloc_start]. But this function is only
     called with [caml_young_alloc_start <= caml_young_ptr <
     caml_memprof_young_trigger], which is contradictory. */
  CAMLassert(lambda > 0);

  /* We need to call the callback for this sampled block. Since the
     callback can potentially allocate, the sampled block will *not*
     be the one pointed to by [caml_memprof_young_trigger]. Instead,
     we remember here that we need to sample the next allocated word,
     call the callback and use as a sample the block which will be
     allocated right after the callback. */

  occurrences =
    mt_generate_binom(caml_memprof_young_trigger - 1 - caml_young_ptr) + 1;

  /* Restore the minor heap in a valid state and suspend sampling for
     calling the callback.
     We should not call the GC before this. */
  caml_young_ptr += whsize;
  caml_memprof_suspended = 1;
  caml_memprof_renew_minor_sample();

  callstack = capture_callstack_minor();
  ephe = do_callback(tag, wosize, occurrences, callstack, Minor);

  caml_memprof_suspended = 0;
  if (Is_exception_result(ephe)) caml_raise(Extract_exception(ephe));

  /* We can now restore the minor heap in the state needed by
     [Alloc_small_aux].
     We should not call the GC after this. */
  if(caml_young_ptr - whsize < caml_young_trigger) {
    /* The call to [caml_gc_dispatch] may run arbitrary OCaml code via
       finalizers. We artificially fill the ephemeron with [Val_unit]
       so that the client code never sees the ephemeron empty before
       the block is actually freed. */
    if(Is_block(ephe))
      caml_ephemeron_set_key(Field(ephe, 0), 0, Val_unit);

    CAML_INSTR_INT ("force_minor/memprof@", 1);
    caml_gc_dispatch();
  }
  caml_young_ptr -= whsize;

  /* Make sure this block is not going to be sampled again. */
  shift_sample(whsize);

  /* Write the ephemeron if not [None]. */
  if(Is_block(ephe)) {
    /* Subtlety: we are actually writing the ephemeron with an invalid
       (uninitialized) block. This is correct for two reasons:
          - The logic of [caml_ephemeron_set_key] never inspects the content of
            the block. In only checks that the block is young.
          - The allocation and initialization happens right after returning
            from [caml_memprof_track_young]. Since the heap is in an invalid
            state before that initialization, very little heap operations are
            allowed until then.
    */
    caml_ephemeron_set_key(Field(ephe, 0), 0, Val_hp(caml_young_ptr));
  }

  CAMLreturn0;
}
