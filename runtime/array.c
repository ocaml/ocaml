/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
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

/* Operations on arrays */
#include <string.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"
#include "caml/runtime_events.h"

static const mlsize_t mlsize_t_max = CAML_UINTNAT_MAX;

/* returns number of elements (either fields or floats) */
/* [ 'a array -> int ] */
CAMLexport mlsize_t caml_array_length(value array)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return Wosize_val(array) / Double_wosize;
  else
#endif
    return Wosize_val(array);
}

CAMLexport int caml_is_double_array(value array)
{
  return (Tag_val(array) == Double_array_tag);
}

/* Note: the OCaml types on the following primitives will work both with
   and without the -no-flat-float-array configure-time option. If you
   respect them, your C code should work in both configurations.
*/

/* [ 'a array -> int -> 'a ] where 'a != float */
CAMLprim value caml_array_get_addr(value array, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  return Field(array, idx);
}

/* [ floatarray -> int -> float ] */
CAMLprim value caml_floatarray_get(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  CAMLassert (Tag_val(array) == Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  d = Double_flat_field(array, idx);
  Alloc_small(res, Double_wosize, Double_tag, Alloc_small_enter_GC);
  Store_double_val(res, d);
  return res;
}

/* [ 'a array -> int -> 'a ] */
CAMLprim value caml_array_get(value array, value index)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_get(array, index);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_get_addr(array, index);
}

/* [ 'a array -> int -> 'a -> unit ] where 'a != float */
CAMLprim value caml_array_set_addr(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  caml_modify(&Field(array, idx), newval);
  return Val_unit;
}

/* [ floatarray -> int -> float -> unit ] */
CAMLprim value caml_floatarray_set(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  double d = Double_val (newval);
  CAMLassert (Tag_val(array) == Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  Store_double_flat_field(array, idx, d);
  return Val_unit;
}

/* [ 'a array -> int -> 'a -> unit ] */
CAMLprim value caml_array_set(value array, value index, value newval)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_set(array, index, newval);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_set_addr(array, index, newval);
}

/* [ floatarray -> int -> float ] */
CAMLprim value caml_floatarray_unsafe_get(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  CAMLassert (Tag_val(array) == Double_array_tag);
  d = Double_flat_field(array, idx);
  Alloc_small(res, Double_wosize, Double_tag, Alloc_small_enter_GC);
  Store_double_val(res, d);
  return res;
}

/* [ 'a array -> int -> 'a ] */
CAMLprim value caml_array_unsafe_get(value array, value index)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_unsafe_get(array, index);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return Field(array, Long_val(index));
}

/* [ 'a array -> int -> 'a -> unit ] where 'a != float */
static value caml_array_unsafe_set_addr(value array, value index,value newval)
{
  intnat idx = Long_val(index);
  caml_modify(&Field(array, idx), newval);
  return Val_unit;
}

/* [ floatarray -> int -> float -> unit ] */
/* [MM]: [caml_array_unsafe_set_addr] has a fence for enforcing the OCaml
   memory model through its use of [caml_modify].
   [MM] [TODO]: [caml_floatarray_unsafe_set] will also need a similar fence in
   [Store_double_flat_field]. */
CAMLprim value caml_floatarray_unsafe_set(value array, value index,value newval)
{
  intnat idx = Long_val(index);
  double d = Double_val (newval);
  Store_double_flat_field(array, idx, d);
  return Val_unit;
}

/* [ 'a array -> int -> 'a -> unit ] */
CAMLprim value caml_array_unsafe_set(value array, value index, value newval)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_unsafe_set(array, index, newval);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_unsafe_set_addr(array, index, newval);
}

/* [len] is a [value] representing number of floats. */
/* [ int -> floatarray ] */
CAMLprim value caml_floatarray_create(value len)
{
  mlsize_t wosize = Long_val(len) * Double_wosize;
  value result;
  if (wosize <= Max_young_wosize){
    if (wosize == 0)
      return Atom(0);
    else
      Alloc_small (result, wosize, Double_array_tag, Alloc_small_enter_GC);
  }else if (wosize > Max_wosize)
    caml_invalid_argument("Float.Array.create");
  else {
    result = caml_alloc_shr (wosize, Double_array_tag);
  }
  /* Give the GC a chance to run, and run memprof callbacks */
  return caml_process_pending_actions_with_root(result);
}

CAMLprim value caml_floatarray_make_unboxed(intnat size, double init)
{
  if (size == 0) {
    return Atom(0);
  }
  value res;
  mlsize_t wsize = size * Double_wosize;
  if (wsize > Max_wosize) caml_invalid_argument("Array.make");
  res = caml_alloc(wsize, Double_array_tag);
  for (mlsize_t i = 0; i < size; i++) {
    Store_double_flat_field(res, i, init);
  }
  /* Give the GC a chance to run, and run memprof callbacks */
  return caml_process_pending_actions_with_root(res);
}

/* [int -> float -> floatarray] */
CAMLprim value caml_floatarray_make(value len, value init)
{
  return caml_floatarray_make_unboxed(Long_val(len), Double_val(init));
}

/* [int -> 'a -> uniform_array] */
CAMLprim value caml_uniform_array_make(value len, value init)
{
  CAMLparam2 (len, init);
  CAMLlocal1 (res);
  mlsize_t size = Long_val(len);
  if (size == 0) {
    CAMLreturn(Atom(0));
  } else if (size <= Max_young_wosize) {
    res = caml_alloc_small(size, 0);
    for (mlsize_t i = 0; i < size; i++) Field(res, i) = init;
  }
  else if (size > Max_wosize) caml_invalid_argument("Array.make");
  else {
    if (Is_block(init) && Is_young(init)) {
      /* We don't want to create so many major-to-minor references,
         so [init] is moved to the major heap by doing a minor GC. */
      CAML_EV_COUNTER (EV_C_FORCE_MINOR_MAKE_VECT, 1);
      caml_minor_collection ();
    }
    CAMLassert(!(Is_block(init) && Is_young(init)));
    res = caml_alloc_shr(size, 0);
    /* We now know that [init] is not in the minor heap, so there is
       no need to call [caml_initialize]. */
    for (mlsize_t i = 0; i < size; i++) Field(res, i) = init;
  }
  /* Give the GC a chance to run, and run memprof callbacks */
  caml_process_pending_actions ();
  CAMLreturn (res);
}

/* [len] is a [value] representing number of words or floats */
CAMLprim value caml_array_make(value len, value init)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Is_block(init)
      && Tag_val(init) == Double_tag) {
    return caml_floatarray_make(len, init);
  }
#endif
  return caml_uniform_array_make(len, init);
}

/* [len] is a [value] representing number of floats */
/* [ int -> float array ] */
CAMLprim value caml_array_create_float(value len)
{
#ifdef FLAT_FLOAT_ARRAY
  return caml_floatarray_create (len);
#else
  /* A signaling NaN, statically allocated */
  static const uintnat some_float_contents[] = {
    Caml_out_of_heap_header(Double_wosize, Double_tag),
#if defined(ARCH_SIXTYFOUR)
    0x7FF0000000000001
#elif defined(ARCH_BIG_ENDIAN)
    0x7FF00000, 0x00000001,
#else
    0x00000001, 0x7FF00000
#endif
  };
  value some_float = Val_hp(some_float_contents);
  return caml_array_make (len, some_float);
#endif
}

/* This primitive is used internally by the compiler to compile
   explicit array expressions.
   For float arrays when FLAT_FLOAT_ARRAY is true, it takes an array of
   boxed floats and returns the corresponding flat-allocated [float array].
   In all other cases, it just returns its argument unchanged.
*/
CAMLprim value caml_array_of_uniform_array(value init)
{
#ifdef FLAT_FLOAT_ARRAY
  CAMLparam1 (init);
  mlsize_t wsize, size;
  CAMLlocal2 (v, res);

  size = Wosize_val(init);
  if (size == 0) {
    CAMLreturn (init);
  } else {
    v = Field(init, 0);
    if (Is_long(v)
        || Tag_val(v) != Double_tag) {
      CAMLreturn (init);
    } else {
      wsize = size * Double_wosize;
      if (wsize <= Max_young_wosize) {
        res = caml_alloc_small(wsize, Double_array_tag);
      } else {
        res = caml_alloc_shr(wsize, Double_array_tag);
      }
      for (mlsize_t i = 0; i < size; i++) {
        double d = Double_val(Field(init, i));
        Store_double_flat_field(res, i, d);
      }
      /* run memprof callbacks */
      caml_process_pending_actions();
      CAMLreturn (res);
    }
  }
#else
  return init;
#endif
}


/* #13003: previous names for array-creation primitives,
   kept for backward-compatibility only. */

CAMLprim value caml_make_vect(value len, value init)
{
  return caml_array_make(len, init);
}

CAMLprim value caml_make_float_vect(value len)
{
  return caml_array_create_float(len);
}

CAMLprim value caml_make_array(value array)
{
  return caml_array_of_uniform_array(array);
}


/* Blitting */

/* [wo_memmove] copies [nvals] values from [src] to [dst]. If there is a single
   domain running, then we use [memmove]. Otherwise, we copy one word at a
   time.

   Since the [memmove] implementation does not guarantee that the writes are
   always word-sized, we explicitly perform word-sized writes of the release
   kind to avoid mixed-mode accesses. Performing release writes should be
   sufficient to prevent smart compilers from coalescing the writes into vector
   writes, and hence prevent mixed-mode accesses. [MM].
   */
static void wo_memmove (volatile value* const dst,
                        volatile const value* const src,
                        mlsize_t nvals)
{
  if (caml_domain_alone ()) {
    memmove ((value*)dst, (value*)src, nvals * sizeof (value));
  } else {
    /* See memory model [MM] notes in memory.c */
    atomic_thread_fence(memory_order_acquire);
    if (dst < src) {
      /* copy ascending */
      for (mlsize_t i = 0; i < nvals; i++)
        atomic_store_release(&((atomic_value*)dst)[i], src[i]);

    } else {
      /* copy descending */
      for (mlsize_t i = nvals; i > 0; i--)
        atomic_store_release(&((atomic_value*)dst)[i-1], src[i-1]);
    }
  }
}

/* [MM] [TODO]: Not consistent with the memory model. See the discussion in
   https://github.com/ocaml-multicore/ocaml-multicore/pull/822. */
CAMLprim value caml_floatarray_blit(value a1, value ofs1, value a2, value ofs2,
                                    value n)
{
  if (Long_val(n) == 0) return Val_unit;
  /* Note: size-0 floatarrays do not have Double_array_tag,
     but only size-0 blits are possible on them, so they
     do not reach this point. */
  CAMLassert (Tag_val(a1) == Double_array_tag);
  CAMLassert (Tag_val(a2) == Double_array_tag);
  /* See memory model [MM] notes in memory.c */
  atomic_thread_fence(memory_order_acquire);
  memmove((double *)a2 + Long_val(ofs2),
          (double *)a1 + Long_val(ofs1),
          Long_val(n) * sizeof(double));
  return Val_unit;
}

CAMLprim value caml_uniform_array_blit(
  value a1, value ofs1, value a2, value ofs2, value n)
{
  volatile value * src, * dst;
  intnat count;

  if (Long_val(n) == 0)
    /* See comment on size-0 floatarrays in [caml_floatarray_blit]. */
    return Val_unit;
  CAMLassert (Tag_val(a1) != Double_array_tag);
  CAMLassert (Tag_val(a2) != Double_array_tag);
  if (Is_young(a2)) {
    /* Arrays of values, destination is in young generation.
       Here too we can do a direct copy since this cannot create
       old-to-young pointers, nor mess up with the incremental major GC.
       Again, wo_memmove takes care of overlap. */
    wo_memmove(&Field(a2, Long_val(ofs2)),
               &Field(a1, Long_val(ofs1)),
               Long_val(n));
    return Val_unit;
  }
  /* Array of values, destination is in old generation.
     We must use caml_modify.  */
  count = Long_val(n);
  if (a1 == a2 && Long_val(ofs1) < Long_val(ofs2)) {
    /* Copy in descending order */
    for (dst = &Field(a2, Long_val(ofs2) + count - 1),
           src = &Field(a1, Long_val(ofs1) + count - 1);
         count > 0;
         count--, src--, dst--) {
      caml_modify(dst, *src);
    }
  } else {
    /* Copy in ascending order */
    for (dst = &Field(a2, Long_val(ofs2)), src = &Field(a1, Long_val(ofs1));
         count > 0;
         count--, src++, dst++) {
      caml_modify(dst, *src);
    }
  }
  /* Many caml_modify in a row can create a lot of old-to-young refs.
     Give the minor GC a chance to run if it needs to. */
  caml_check_urgent_gc(Val_unit);
  return Val_unit;
}

CAMLprim value caml_array_blit(value a1, value ofs1, value a2, value ofs2,
                               value n)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(a2) == Double_array_tag)
    return caml_floatarray_blit(a1, ofs1, a2, ofs2, n);
#endif
  return caml_uniform_array_blit(a1, ofs1, a2, ofs2, n);
}

/* generic [gather] functions for extraction and concatenation of sub-arrays */

/* The lengths are specified in number of floats,
   as returned by [caml_array_length]. */
static value caml_floatarray_gather(intnat num_arrays,
                                    value arrays[/*num_arrays*/],
                                    intnat offsets[/*num_arrays*/],
                                    intnat lengths[/*num_arrays*/])
{
  CAMLparamN(arrays, num_arrays);
  value res;                    /* no need to register it as a root */

  /* Determine total size, in number of floats. */
  mlsize_t size = 0;
  for (mlsize_t i = 0; i < num_arrays; i++) {
    if (mlsize_t_max - lengths[i] < size) caml_invalid_argument("Array.concat");
    size += lengths[i];
    CAMLassert(Tag_val(arrays[i]) == Double_array_tag
               || Wosize_val(arrays[i]) == 0);
  }
  if (size == 0) {
    /* If total size = 0, just return empty array */
    res = Atom(0);
  }
  /* This is an array of floats.  We can use memcpy directly. */
  if (size > Max_wosize/Double_wosize) caml_invalid_argument("Array.concat");
  mlsize_t wsize = size * Double_wosize; /* total size, in words */
  res = caml_alloc(wsize, Double_array_tag);
  mlsize_t pos = 0;
  for (mlsize_t i = 0; i < num_arrays; i++) {
    /* [res] is freshly allocated, and no other domain has a reference to it.
       Hence, a plain [memcpy] is sufficient. */
    memcpy((double *)res + pos,
           (double *)arrays[i] + offsets[i],
           lengths[i] * sizeof(double));
    pos += lengths[i];
  }
  CAMLassert(pos == size);
  CAMLreturn(res);
}

static value caml_uniform_array_gather(intnat num_arrays,
                                       value arrays[/*num_arrays*/],
                                       intnat offsets[/*num_arrays*/],
                                       intnat lengths[/*num_arrays*/])
{
  CAMLparamN(arrays, num_arrays);
  value res;                    /* no need to register it as a root */

  /* Determine total size */
  mlsize_t size = 0;
  for (mlsize_t i = 0; i < num_arrays; i++) {
    if (mlsize_t_max - lengths[i] < size) caml_invalid_argument("Array.concat");
    size += lengths[i];
    CAMLassert(Tag_val(arrays[i]) != Double_array_tag);
  }
  if (size == 0) {
    /* If total size = 0, just return an empty array */
    res = Atom(0);
  }
  else if (size <= Max_young_wosize) {
    /* Array of values, small enough to fit in young generation.
       We can use memcpy directly. */
    res = caml_alloc_small(size, 0);
    mlsize_t pos = 0;
    for (mlsize_t i = 0; i < num_arrays; i++) {
      /* [res] is freshly allocated, and no other domain has a reference to it.
         Hence, a plain [memcpy] is sufficient. */
      memcpy((value*)&Field(res, pos),
             (value*)&Field(arrays[i], offsets[i]),
             lengths[i] * sizeof(value));
      pos += lengths[i];
    }
    CAMLassert(pos == size);
  }
  else if (size > Max_wosize) {
    /* Array of values, too big. */
    caml_invalid_argument("Array.concat");
  } else {
    /* Array of values, must be allocated in old generation and filled
       using caml_initialize. */
    res = caml_alloc_shr(size, 0);
    mlsize_t pos = 0;
    for (mlsize_t i = 0; i < num_arrays; i++) {
      volatile value *src = &Field(arrays[i], offsets[i]);
      for (mlsize_t count = lengths[i];
           count > 0;
           count--, src++, pos++) {
        caml_initialize(&Field(res, pos), *src);
      }
    }
    CAMLassert(pos == size);

    /* Many caml_initialize in a row can create a lot of old-to-young
       refs.  Give the minor GC a chance to run if it needs to.
       Run memprof callbacks for the major allocation. */
    res = caml_process_pending_actions_with_root (res);
  }
  CAMLreturn (res);
}


static value caml_array_gather(intnat num_arrays,
                               value arrays[/*num_arrays*/],
                               intnat offsets[/*num_arrays*/],
                               intnat lengths[/*num_arrays*/])
{
#ifdef FLAT_FLOAT_ARRAY
  for (mlsize_t i = 0; i < num_arrays; i++) {
    /* An array is either an empty array,
       or a float array, or a non-float array.
       We know which implementation to use on the first non-empty array. */
    if (Wosize_val(arrays[i]) == 0)
      continue;
    else if (Tag_val(arrays[i]) == Double_array_tag)
      return caml_floatarray_gather(num_arrays, arrays, offsets, lengths);
    else
      break;
  }
  /* If we reach this point, all arrays were empty.
     Calling the uniform_ version below is correct
     -- it will return an empty array. */
#endif
  return caml_uniform_array_gather(num_arrays, arrays, offsets, lengths);
}

CAMLprim value caml_floatarray_sub(value a, value ofs, value len)
{
  value arrays[1] = { a };
  intnat offsets[1] = { Long_val(ofs) };
  intnat lengths[1] = { Long_val(len) };
  return caml_floatarray_gather(1, arrays, offsets, lengths);
}

CAMLprim value caml_uniform_array_sub(value a, value ofs, value len)
{
  value arrays[1] = { a };
  intnat offsets[1] = { Long_val(ofs) };
  intnat lengths[1] = { Long_val(len) };
  return caml_uniform_array_gather(1, arrays, offsets, lengths);
}

CAMLprim value caml_array_sub(value a, value ofs, value len)
{
  value arrays[1] = { a };
  intnat offsets[1] = { Long_val(ofs) };
  intnat lengths[1] = { Long_val(len) };
  return caml_array_gather(1, arrays, offsets, lengths);
}

CAMLprim value caml_floatarray_append(value a1, value a2)
{
  value arrays[2] = { a1, a2 };
  intnat offsets[2] = { 0, 0 };
  /* sizes are specified in number of floats */
  intnat lengths[2] = { caml_array_length(a1), caml_array_length(a2) };
  return caml_floatarray_gather(2, arrays, offsets, lengths);
}

CAMLprim value caml_uniform_array_append(value a1, value a2)
{
  value arrays[2] = { a1, a2 };
  intnat offsets[2] = { 0, 0 };
  intnat lengths[2] = { caml_array_length(a1), caml_array_length(a2) };
  return caml_uniform_array_gather(2, arrays, offsets, lengths);
}

CAMLprim value caml_array_append(value a1, value a2)
{
  value arrays[2] = { a1, a2 };
  intnat offsets[2] = { 0, 0 };
  intnat lengths[2] = { caml_array_length(a1), caml_array_length(a2) };
  return caml_array_gather(2, arrays, offsets, lengths);
}

CAMLprim value caml_array_concat(value al)
{
#define STATIC_SIZE 16
  value static_arrays[STATIC_SIZE], * arrays;
  intnat static_offsets[STATIC_SIZE], * offsets;
  intnat static_lengths[STATIC_SIZE], * lengths;
  intnat n, i;
  value l, res;

  /* Length of list = number of arrays */
  for (n = 0, l = al; l != Val_emptylist; l = Field(l, 1)) n++;
  /* Allocate extra storage if too many arrays */
  if (n <= STATIC_SIZE) {
    arrays = static_arrays;
    offsets = static_offsets;
    lengths = static_lengths;
  } else {
    arrays = caml_stat_alloc(n * sizeof(value));
    offsets = caml_stat_alloc_noexc(n * sizeof(intnat));
    if (offsets == NULL) {
      caml_stat_free(arrays);
      caml_raise_out_of_memory();
    }
    lengths = caml_stat_alloc_noexc(n * sizeof(value));
    if (lengths == NULL) {
      caml_stat_free(offsets);
      caml_stat_free(arrays);
      caml_raise_out_of_memory();
    }
  }
  /* Build the parameters to caml_array_gather */
  for (i = 0, l = al; l != Val_emptylist; l = Field(l, 1), i++) {
    arrays[i] = Field(l, 0);
    offsets[i] = 0;
    lengths[i] = caml_array_length(Field(l, 0));
  }
  /* Do the concatenation */
  res = caml_array_gather(n, arrays, offsets, lengths);
  /* Free the extra storage if needed */
  if (n > STATIC_SIZE) {
    caml_stat_free(arrays);
    caml_stat_free(offsets);
    caml_stat_free(lengths);
  }
  return res;
}

CAMLprim value caml_floatarray_fill_unboxed(
  value array, intnat ofs, intnat len, double d)
{
  for (; len > 0; len--, ofs++)
    Store_double_flat_field(array, ofs, d);
  return Val_unit;
}

CAMLprim value caml_floatarray_fill(
  value array, value v_ofs, value v_len, value val)
{
  return caml_floatarray_fill_unboxed(
    array, Long_val(v_ofs), Long_val(v_len), Double_val(val));
}

CAMLprim value caml_uniform_array_fill(
  value array, value v_ofs, value v_len, value val)
{
  intnat ofs = Long_val(v_ofs);
  intnat len = Long_val(v_len);
  volatile value* fp;

  /* This duplicates the logic of caml_modify.  Please refer to the
     implementation of that function for a description of GC
     invariants we need to enforce.*/
  fp = &Field(array, ofs);
  if (Is_young(array)) {
    for (; len > 0; len--, fp++) *fp = val;
  } else {
    int is_val_young_block = Is_block(val) && Is_young(val);
    for (; len > 0; len--, fp++) {
      value old = *fp;
      if (old == val) continue;
      *fp = val;
      if (Is_block(old)) {
        if (Is_young(old)) continue;
        caml_darken(Caml_state, old, NULL);
      }
      if (is_val_young_block)
        Ref_table_add(&Caml_state->minor_tables->major_ref, fp);
    }
    if (is_val_young_block) caml_check_urgent_gc (Val_unit);
  }
  return Val_unit;
}

CAMLprim value caml_array_fill(value array,
                               value v_ofs,
                               value v_len,
                               value val)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag) {
    return caml_floatarray_fill(array, v_ofs, v_len, val);
  }
#endif
  return caml_uniform_array_fill(array, v_ofs, v_len, val);
}
