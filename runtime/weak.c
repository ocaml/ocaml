/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1997 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Operations on weak arrays and ephemerons (named ephe here)*/

#include <string.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/weak.h"
#include "caml/minor_gc.h"
#include "caml/signals.h"
#include "caml/eventlog.h"

value caml_ephe_list_head = 0;

static value ephe_dummy = 0;
value caml_ephe_none = (value) &ephe_dummy;

#define CAMLassert_valid_ephemeron(eph) do{                             \
    CAMLassert (Is_in_heap (eph));                                      \
    CAMLassert (Tag_val(eph) == Abstract_tag);                          \
    CAMLassert (CAML_EPHE_FIRST_KEY <= Wosize_val (eph));               \
}while(0)

#define CAMLassert_valid_offset(eph, offset) do{                        \
    CAMLassert_valid_ephemeron(eph);                                    \
    CAMLassert (0 <= offset);                                           \
    CAMLassert (offset < Wosize_val (eph) - CAML_EPHE_FIRST_KEY);       \
}while(0)

#ifdef DEBUG
#define CAMLassert_not_dead_value(v) do{                              \
    value __v = v;                                                    \
    if (caml_gc_phase == Phase_clean                                  \
        && Is_block(__v)                                              \
        && Is_in_heap (__v)) {                                        \
      if (Tag_val (__v) == Infix_tag) __v -= Infix_offset_val (__v);  \
      CAMLassert ( !Is_white_val(__v) );                              \
    }                                                                 \
}while(0)
#else
#define CAMLassert_not_dead_value(v)
#endif

CAMLexport mlsize_t caml_ephemeron_num_keys(value eph)
{
  CAMLassert_valid_ephemeron(eph);
  return Wosize_val (eph) - CAML_EPHE_FIRST_KEY;
}

/* The minor heap is considered alive. Outside minor and major heap it is
   considered alive (out of reach of the GC). */
Caml_inline int Test_if_its_white(value x){
  CAMLassert (x != caml_ephe_none);
#ifdef NO_NAKED_POINTERS
  if (!Is_block(x) || Is_young (x)) return 0;
#else
  if (!Is_block(x) || !Is_in_heap(x)) return 0;
#endif
  if (Tag_val(x) == Infix_tag) x -= Infix_offset_val(x);
  return Is_white_val(x);
}

/* If it is not white during clean phase it is dead, i.e it will be swept */
Caml_inline int Is_Dead_during_clean(value x)
{
  CAMLassert (caml_gc_phase == Phase_clean);
  return Test_if_its_white(x);
}

/** caml_ephe_none is considered as not white  */
Caml_inline int Is_White_During_Mark(value x)
{
  CAMLassert (caml_gc_phase == Phase_mark);
  if (x == caml_ephe_none ) return 0;
  return Test_if_its_white(x);
}

/** The minor heap doesn't have to be marked, outside they should
    already be black. Remains the value in the heap to mark.
*/
Caml_inline int Must_be_Marked_during_mark(value x)
{
  CAMLassert (x != caml_ephe_none);
  CAMLassert (caml_gc_phase == Phase_mark);
#ifdef NO_NAKED_POINTERS
  return Is_block (x) && !Is_young (x);
#else
  return Is_block (x) && Is_in_heap (x);
#endif
}

/* [len] is a number of words (fields) */
CAMLexport value caml_ephemeron_create (mlsize_t len)
{
  mlsize_t size, i;
  value res;

  CAMLassert(len <= CAML_EPHE_MAX_WOSIZE);
  size = len + CAML_EPHE_FIRST_KEY;
  if (size < CAML_EPHE_FIRST_KEY || size > Max_wosize)
    caml_invalid_argument ("Weak.create");
  res = caml_alloc_shr (size, Abstract_tag);
  for (i = 1; i < size; i++) Field (res, i) = caml_ephe_none;
  Field (res, CAML_EPHE_LINK_OFFSET) = caml_ephe_list_head;
  caml_ephe_list_head = res;
  return res;
}

CAMLprim value caml_ephe_create (value len)
{
  value res = caml_ephemeron_create(Long_val(len));
  // run memprof callbacks
  return caml_process_pending_actions_with_root(res);
}

CAMLprim value caml_weak_create (value len)
{
  return caml_ephe_create(len);
}

/**
   Specificity of the cleaning phase (Phase_clean):

   The dead keys must be removed from the ephemerons and data removed
   when one the keys is dead. Here we call it cleaning the ephemerons.
   A specific phase of the GC is dedicated to this, Phase_clean. This
   phase is just after the mark phase, so the white values are dead
   values. It iterates the function caml_ephe_clean through all the
   ephemerons.

   However the GC is incremental and ocaml code can run on the middle
   of this cleaning phase. In order to respect the semantic of the
   ephemerons concerning dead values, the getter and setter must work
   as if the cleaning of all the ephemerons have been done at once.

   - key getter: Even if a dead key have not yet been replaced by
     caml_ephe_none, getting it should return none.
   - key setter: If we replace a dead key we need to set the data to
     caml_ephe_none and clean the ephemeron.

     This two cases are dealt by a call to do_check_key_clean that
     trigger the cleaning of the ephemerons when the accessed key is
     dead. This test is fast.

     In the case of value getter and value setter, there is no fast
     test because the removing of the data depend of the deadliness of the keys.
     We must always try to clean the ephemerons.

 */

#define None_val (Val_int(0))
#define Some_tag 0

/* If we are in Phase_clean we need to check if the key
   that is going to disappear is dead and so should trigger a cleaning
 */
static void do_check_key_clean(value ar, mlsize_t offset)
{
  value elt;
  CAMLassert (offset >= CAML_EPHE_FIRST_KEY);
  CAMLassert (caml_gc_phase == Phase_clean);
  elt = Field (ar, offset);
  if (elt != caml_ephe_none && Is_Dead_during_clean(elt)){
    Field(ar, offset) = caml_ephe_none;
    Field(ar, CAML_EPHE_DATA_OFFSET) = caml_ephe_none;
  };
}

/* If we are in Phase_clean we need to do as if the key is empty when
   it will be cleaned during this phase */
Caml_inline int is_ephe_key_none(value ar, mlsize_t offset)
{
  value elt = Field (ar, offset);
  if (elt == caml_ephe_none){
    return 1;
  }else if (caml_gc_phase == Phase_clean && Is_Dead_during_clean(elt)){
    Field(ar, offset) = caml_ephe_none;
    Field(ar, CAML_EPHE_DATA_OFFSET) = caml_ephe_none;
    return 1;
  } else {
    return 0;
  }
}

static void do_set (value ar, mlsize_t offset, value v)
{
  if (Is_block (v) && Is_young (v)){
    /* modified version of caml_modify */
    value old = Field (ar, offset);
    Field (ar, offset) = v;
    if (!(Is_block (old) && Is_young (old))){
      add_to_ephe_ref_table (Caml_state->ephe_ref_table, ar, offset);
    }
  }else{
    Field (ar, offset) = v;
  }
}

CAMLexport void caml_ephemeron_set_key(value ar, mlsize_t offset, value k)
{
  CAMLassert_valid_offset(ar, offset);
  CAMLassert (Is_in_heap (ar));

  offset += CAML_EPHE_FIRST_KEY;

  if( caml_gc_phase == Phase_mark
      && caml_ephe_list_pure
      && Field(ar, CAML_EPHE_DATA_OFFSET) != caml_ephe_none
      && !Is_white_val(ar)
      && Is_White_During_Mark(Field(ar, offset))
      && !Is_White_During_Mark(k)){
    /* the ephemeron could be in the set (2) only because of a white key and not
       have one anymore after set */
    caml_darken(Field(ar, CAML_EPHE_DATA_OFFSET), NULL);
  };
  if(caml_gc_phase == Phase_clean) do_check_key_clean(ar, offset);
  do_set (ar, offset, k);
}

CAMLprim value caml_ephe_set_key (value ar, value n, value el)
{
  caml_ephemeron_set_key(ar, Long_val(n), el);
  return Val_unit;
}

CAMLexport void caml_ephemeron_unset_key(value ar, mlsize_t offset)
{
  CAMLassert_valid_offset(ar, offset);
  CAMLassert (Is_in_heap (ar));

  offset += CAML_EPHE_FIRST_KEY;

  if( caml_gc_phase == Phase_mark
      && caml_ephe_list_pure
      && Field(ar, CAML_EPHE_DATA_OFFSET) != caml_ephe_none
      && !Is_white_val(ar)
      && Is_White_During_Mark(Field(ar, offset)) ){
    /* the ephemeron could be in the set (2) only because of this white key and
       not have one anymore after unsetting it */
    caml_darken(Field(ar, CAML_EPHE_DATA_OFFSET), NULL);
  };

  if(caml_gc_phase == Phase_clean) do_check_key_clean(ar, offset);
  Field (ar, offset) = caml_ephe_none;
}

CAMLprim value caml_ephe_unset_key (value ar, value n)
{
  caml_ephemeron_unset_key(ar, Long_val(n));
  return Val_unit;
}

/* deprecated (03/2016) */
value caml_ephe_set_key_option (value ar, value n, value el)
{
  if (Is_block (el)){
    CAMLassert (Wosize_val (el) == 1);
    caml_ephe_set_key(ar, n, Field (el, 0));
  }else{
    CAMLassert (el == None_val);
    caml_ephe_unset_key(ar, n);
  }
  return Val_unit;
}

/* deprecated (03/2016) */
CAMLprim value caml_weak_set (value ar, value n, value el)
{
  return caml_ephe_set_key_option(ar, n, el);
}

CAMLexport void caml_ephemeron_set_data (value ar, value el)
{
  value old_data;
  CAMLassert_valid_ephemeron(ar);

  old_data = Field (ar, CAML_EPHE_DATA_OFFSET);
  if (caml_gc_phase == Phase_mark && !Is_White_During_Mark(old_data))
    caml_darken (el, NULL);
  if (caml_gc_phase == Phase_clean){
    /* During this phase since we don't know which ephemerons have been
       cleaned we always need to check it. */
    caml_ephe_clean(ar);
  };
  do_set (ar, CAML_EPHE_DATA_OFFSET, el);
}

CAMLprim value caml_ephe_set_data (value ar, value el)
{
  caml_ephemeron_set_data (ar, el);
  return Val_unit;
}

CAMLexport void caml_ephemeron_unset_data (value ar)
{
  CAMLassert_valid_ephemeron(ar);

  Field (ar, CAML_EPHE_DATA_OFFSET) = caml_ephe_none;
}

CAMLprim value caml_ephe_unset_data (value ar)
{
  caml_ephemeron_unset_data (ar);
  return Val_unit;
}

static value optionalize(int status, value *x)
{
  CAMLparam0();
  CAMLlocal2(res, v);
  if(status) {
    v = *x;
    res = caml_alloc_small (1, Some_tag);
    Field (res, 0) = v;
  } else {
    res = None_val;
  }
  // run memprof callbacks both for the option we are allocating here
  // and the calling function.
  caml_process_pending_actions();
  CAMLreturn(res);
}

CAMLexport int caml_ephemeron_get_key (value ar, mlsize_t offset, value *key)
{
  value elt;
  CAMLassert_valid_offset(ar, offset);

  offset += CAML_EPHE_FIRST_KEY;

  if (is_ephe_key_none(ar, offset)){
    return 0;
  }else{
    elt = Field (ar, offset);
    if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(elt)){
      caml_darken (elt, NULL);
    }
    *key = elt;
    CAMLassert_not_dead_value(elt);
    return 1;
  }
}

CAMLprim value caml_ephe_get_key (value ar, value n)
{
  value data;
  return optionalize(caml_ephemeron_get_key(ar, Long_val(n), &data), &data);
}

CAMLprim value caml_weak_get (value ar, value n)
{
  return caml_ephe_get_key(ar, n);
}

CAMLexport int caml_ephemeron_get_data (value ar, value *data)
{
  value elt;
  CAMLassert_valid_ephemeron(ar);

  if(caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  elt = Field (ar, CAML_EPHE_DATA_OFFSET);
  if (elt == caml_ephe_none){
    return 0;
  }else{
    if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(elt)){
      caml_darken (elt, NULL);
    }
    *data = elt;
    CAMLassert_not_dead_value(elt);
    return 1;
  }
}

CAMLprim value caml_ephe_get_data (value ar)
{
  value data;
  return optionalize(caml_ephemeron_get_data(ar, &data), &data);
}

static void copy_value(value src, value dst)
{
  mlsize_t sz, i;
  sz = Wosize_val(src);
  if (Tag_val (src) >= No_scan_tag) {
    /* Direct copy */
    memcpy (Bp_val (dst), Bp_val (src), Bsize_wsize (sz));
    return;
  }
  i = 0;
  if (Tag_val (src) == Closure_tag) {
    /* Direct copy of the code pointers and closure info fields */
    i = Start_env_closinfo(Closinfo_val(src));
    memcpy (Bp_val (dst), Bp_val (src), Bsize_wsize (i));
  }
  /* Field-by-field copy and darkening of the remaining fields */
  for (/*nothing*/; i < sz; i++){
    value f = Field (src, i);
    if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(f)){
      caml_darken (f, NULL);
    }
    caml_modify (&Field (dst, i), f);
  }
}

CAMLexport int caml_ephemeron_get_key_copy(value ar, mlsize_t offset,
                                           value *key)
{
  mlsize_t loop = 0, infix_offs;
  CAMLparam1(ar);
  value elt = Val_unit, v; /* Caution: they are NOT local roots. */
  CAMLassert_valid_offset(ar, offset);

  offset += CAML_EPHE_FIRST_KEY;

  while(1) {
    if(is_ephe_key_none(ar, offset)) CAMLreturn(0);
    v = Field (ar, offset);
    /** Don't copy custom_block #7279 */
    if(!(Is_block (v) && Is_in_value_area(v) && Tag_val(v) != Custom_tag)) {
      if ( caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(v) ){
        caml_darken (v, NULL);
      };
      *key = v;
      CAMLreturn(1);
    }
    infix_offs = Tag_val(v) == Infix_tag ? Infix_offset_val(v) : 0;
    v -= infix_offs;
    if (elt != Val_unit &&
        Wosize_val(v) == Wosize_val(elt) && Tag_val(v) == Tag_val(elt)) {
      /* The allocation may trigger a finaliser that change the tag
         and size of the block. Therefore, in addition to checking
         that the pointer is still alive, we have to check that it
         still has the same tag and size.
       */
      CAMLassert_not_dead_value(v);
      copy_value(v, elt);
      *key = elt + infix_offs;
      CAMLreturn(1);
    }

    CAMLassert(loop < 10);
    if(8 == loop){ /** One minor gc must be enough */
      elt = Val_unit;
      CAML_EV_COUNTER (EV_C_FORCE_MINOR_WEAK, 1);
      caml_minor_collection ();
    } else {
      /* cases where loop is between 0 to 7 and where loop is equal to 9 */
      elt = caml_alloc (Wosize_val (v), Tag_val (v));
      /* The GC may erase, move or even change v during this call to
         caml_alloc. */
    }
    ++loop;
  }
}

CAMLprim value caml_ephe_get_key_copy (value ar, value n)
{
  value key;
  int status = caml_ephemeron_get_key_copy(ar, Long_val(n), &key);
  return optionalize(status, &key);
}

CAMLprim value caml_weak_get_copy (value ar, value n)
{
  return caml_ephe_get_key_copy(ar, n);
}

CAMLexport int caml_ephemeron_get_data_copy (value ar, value *data)
{
  mlsize_t loop = 0, infix_offs;
  CAMLparam1 (ar);
  value elt = Val_unit, v; /* Caution: they are NOT local roots. */
  CAMLassert_valid_ephemeron(ar);

  while(1) {
    if (caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
    v = Field (ar, CAML_EPHE_DATA_OFFSET);
    if (v == caml_ephe_none) CAMLreturn(0);
    /** Don't copy custom_block #7279 */
    if (!(Is_block (v) && Is_in_value_area(v) && Tag_val(v) != Custom_tag)) {
      if ( caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(v) ){
        caml_darken (v, NULL);
      };
      *data = v;
      CAMLreturn(1);
    }
    infix_offs = Tag_val(v) == Infix_tag ? Infix_offset_val(v) : 0;
    v -= infix_offs;
    if (elt != Val_unit &&
        Wosize_val(v) == Wosize_val(elt) && Tag_val(v) == Tag_val(elt)) {
      /** cf caml_ephemeron_get_key_copy */
      CAMLassert_not_dead_value(v);
      copy_value(v, elt);
      *data = elt + infix_offs;
      CAMLreturn(1);
    }

    CAMLassert(loop < 10);
    if(8 == loop){ /** One minor gc must be enough */
      elt = Val_unit;
      CAML_EV_COUNTER (EV_C_FORCE_MINOR_WEAK, 1);
      caml_minor_collection ();
    } else {
      /* cases where loop is between 0 to 7 and where loop is equal to 9 */
      elt = caml_alloc (Wosize_val (v), Tag_val (v));
      /** cf caml_ephemeron_get_key_copy */
    }
    ++loop;
  }
}


CAMLprim value caml_ephe_get_data_copy (value ar)
{
  value data;
  int status = caml_ephemeron_get_data_copy(ar, &data);
  return optionalize(status, &data);
}

CAMLexport int caml_ephemeron_key_is_set(value ar, mlsize_t offset)
{
  CAMLassert_valid_offset(ar, offset);

  offset += CAML_EPHE_FIRST_KEY;
  return !is_ephe_key_none(ar, offset);
}

CAMLprim value caml_ephe_check_key (value ar, value n)
{
  return Val_bool (caml_ephemeron_key_is_set(ar, Long_val(n)));
}

CAMLprim value caml_weak_check (value ar, value n)
{
  return caml_ephe_check_key(ar, n);
}

CAMLexport int caml_ephemeron_data_is_set (value ar)
{
  CAMLassert_valid_ephemeron(ar);

  if(caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  return Field (ar, CAML_EPHE_DATA_OFFSET) != caml_ephe_none;
}

CAMLprim value caml_ephe_check_data (value ar)
{
  return Val_bool (caml_ephemeron_data_is_set(ar));
}

CAMLexport void caml_ephemeron_blit_key(value ars, mlsize_t offset_s,
                                        value ard, mlsize_t offset_d,
                                        mlsize_t length)
{
  intnat i; /** intnat because the second for-loop stops with i == -1 */
  int dest_has_white_value;
  if (length == 0) return;
  CAMLassert_valid_offset(ars, offset_s);
  CAMLassert_valid_offset(ard, offset_d);
  CAMLassert(length <= Wosize_val(ars) - CAML_EPHE_FIRST_KEY);
  CAMLassert(length <= Wosize_val(ard) - CAML_EPHE_FIRST_KEY);
  CAMLassert(offset_s <= Wosize_val(ars) - CAML_EPHE_FIRST_KEY - length);
  CAMLassert(offset_d <= Wosize_val(ard) - CAML_EPHE_FIRST_KEY - length);

  offset_s += CAML_EPHE_FIRST_KEY;
  offset_d += CAML_EPHE_FIRST_KEY;

  if ( caml_gc_phase == Phase_mark
       && caml_ephe_list_pure
       && Field(ard, CAML_EPHE_DATA_OFFSET) != caml_ephe_none
       && !Is_white_val(ard)
       && !Is_White_During_Mark(Field(ard, CAML_EPHE_DATA_OFFSET))
       ){
    /* We check here if darkening of the data of the destination is needed
       because the destination could be in (2). Indeed a white key could
       disappear from the destination after blitting and being in (2) requires
       if the ephemeron is alive without white key to have a black or none
       data. */

    dest_has_white_value = 0;

    for(i = 0; i < length; i++){
      dest_has_white_value |= Is_White_During_Mark(Field(ard, offset_d + i));
    };
    /* test if the destination can't be in set (2) because of the keys that are
       going to be set */
    if(!dest_has_white_value) goto No_darkening;
    for(i = 0; i < length; i++){
      /* test if the source is going to bring a white key to replace the one
         set */
      if(Is_White_During_Mark(Field(ars, offset_s + i))) goto No_darkening;
    };
    /* the destination ephemeron could be in the set (2) because of a white key
        replaced and not have one anymore after. */
    caml_darken(Field(ard, CAML_EPHE_DATA_OFFSET),NULL);
  }
  No_darkening:

  if (caml_gc_phase == Phase_clean){
    caml_ephe_clean_partial(ars, offset_s, offset_s + length);
    /* We don't need to clean the keys that are about to be overwritten,
       except when cleaning them could result in releasing the data,
       which can't happen if data is already released. */
    if (Field (ard, CAML_EPHE_DATA_OFFSET) != caml_ephe_none)
      caml_ephe_clean_partial(ard, offset_d, offset_d + length);
  }
  if (offset_d < offset_s){
    for (i = 0; i < length; i++){
      do_set (ard, offset_d + i, Field (ars, offset_s + i));
    }
  }else{
    for (i = length - 1; i >= 0; i--){
      do_set (ard, offset_d + i,  Field (ars, offset_s + i));
    }
  }
}

CAMLprim value caml_ephe_blit_key (value ars, value ofs,
                                   value ard, value ofd, value len)
{
  if (Long_val(len) == 0) return Val_unit;

  caml_ephemeron_blit_key(ars,Long_val(ofs),ard,Long_val(ofd),Long_val(len));
  return Val_unit;
}

CAMLprim value caml_weak_blit (value ars, value ofs,
                      value ard, value ofd, value len)
{
  return caml_ephe_blit_key (ars, ofs, ard, ofd, len);
}

CAMLexport void caml_ephemeron_blit_data (value ars, value ard)
{
  value data, old_data;
  CAMLassert_valid_ephemeron(ars);
  CAMLassert_valid_ephemeron(ard);

  if(caml_gc_phase == Phase_clean) {
    caml_ephe_clean(ars);
    caml_ephe_clean(ard);
  };

  data = Field (ars, CAML_EPHE_DATA_OFFSET);
  old_data = Field (ard, CAML_EPHE_DATA_OFFSET);
  if (caml_gc_phase == Phase_mark &&
      data != caml_ephe_none &&
      !Is_White_During_Mark(old_data))
    caml_darken (data, NULL);

  do_set (ard, CAML_EPHE_DATA_OFFSET, data);
}

CAMLprim value caml_ephe_blit_data (value ars, value ard)
{
  caml_ephemeron_blit_data(ars, ard);
  return Val_unit;
}
