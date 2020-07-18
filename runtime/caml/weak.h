/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Damien Doligez, projet Para, INRIA Rocquencourt            */
/*                                                                        */
/*   Copyright 1997 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operations on weak arrays */

#ifndef CAML_WEAK_H
#define CAML_WEAK_H

#include "mlvalues.h"
#include "memory.h"

#ifdef __cplusplus
extern "C" {
#endif

/** The requirements of the functions must be satisfied, it is
    unspecified what happens if they are not. The debugging runtime
    could check some of them. */

CAMLextern value caml_ephemeron_create(mlsize_t len);
/** Create an ephemeron with the given number of keys.
    This function allocates.
 */

CAMLextern mlsize_t caml_ephemeron_num_keys(value eph);
/** Return the number of key in the ephemeron. The valid key offset goes
    from [0] to the predecessor of the returned value. */

CAMLextern int caml_ephemeron_key_is_set(value eph, mlsize_t offset);
/** Return 1 if the key in the ephemeron at the given offset is set.
    Otherwise 0. The value [eph] must be an ephemeron and [offset] a
    valid key offset.
*/

CAMLextern void caml_ephemeron_set_key(value eph, mlsize_t offset, value k);
/** Set the key of the given ephemeron [eph] at the given offset
    [offset] to the given value [k]. The value [eph] must be an
    ephemeron, [offset] a valid key offset and [k] a block.
*/

CAMLextern void caml_ephemeron_unset_key(value eph, mlsize_t offset);
/** Unset the key of the given ephemeron at the given offset. The
    value [eph] must be an ephemeron and [offset] a valid key offset.
*/

CAMLextern int caml_ephemeron_get_key(value eph, mlsize_t offset, value *key);
/** Return 1 if the key in the ephemeron at the given offset is set.
    Otherwise 0. When returning 1, set [*key] to the pointed value.

    The value [eph] must be an ephemeron and [offset] a valid key
    offset.
*/

CAMLextern int caml_ephemeron_get_key_copy(value eph, mlsize_t offset,
                                           value *key);
/** Return 1 if the key in the ephemeron at the given offset is set.
    Otherwise 0. When returning 1, set [*key] to a shallow copy of the
    key. This function allocates.

    The value [eph] must be an ephemeron and [offset] a valid key
    offset.
*/

CAMLextern void caml_ephemeron_blit_key(value eph1, mlsize_t off1,
                                        value eph2, mlsize_t off2,
                                        mlsize_t len);
/** Fill the given range of keys of [eph2] with the given range of
    keys of [eph1]. Contrary to using caml_ephemeron_get_key followed
    by caml_ephemeron_set_key or caml_ephemeron_unset_key, this
    function does not prevent the incremental GC from erasing the
    value in its current cycle. The value [eph1] (resp. [eph2]) must
    be an ephemeron and the offsets between [off1] and [off1+len]
    (resp. between [off2] and [off2+offset]) must be valid keys of
    [eph1] (resp. [eph2]).
*/

CAMLextern int caml_ephemeron_data_is_set(value eph);
/** Return 1 if the data in the ephemeron is set.
    Otherwise 0. The value [eph] must be an ephemeron.
*/

CAMLextern void caml_ephemeron_set_data(value eph, value k);
/** Set the data of the given ephemeron [eph] to the given value
    [k]. The value [eph] must be an ephemeron and [k] a block.
*/

CAMLextern void caml_ephemeron_unset_data(value eph);
/** Unset the data of the given ephemeron. The value [eph] must be an
    ephemeron.
*/

CAMLextern int caml_ephemeron_get_data(value eph, value *data);
/** Return 1 if the data in the ephemeron at the given offset is set.
    Otherwise 0. When returning 1, set [*data] to the pointed value.

    The value [eph] must be an ephemeron and [offset] a valid key
    offset.
*/

CAMLextern int caml_ephemeron_get_data_copy(value eph, value *data);
/** Return 1 if the data in the ephemeron at the given offset is set.
    Otherwise 0. When returning 1, set [*data] to a shallow copy of
    the data. This function allocates.

    The value [eph] must be an ephemeron and [offset] a valid key
    offset.
*/

CAMLextern void caml_ephemeron_blit_data(value eph1, value eph2);
/** Sets the data of [eph2] to be the same as the data of [eph1].
    Contrary to using caml_ephemeron_get_data followed by
    caml_ephemeron_set_data or caml_ephemeron_unset_data, this
    function does not prevent the incremental GC from erasing the
    value in its current cycle. The values [eph1] and [eph2] must be
    ephemerons.
*/


#define caml_weak_array_length caml_ephemeron_num_keys
#define caml_weak_array_create caml_ephemeron_create
#define caml_weak_array_check caml_ephemeron_key_is_set
#define caml_weak_array_unset caml_ephemeron_unset_key
#define caml_weak_array_set caml_ephemeron_set_key
#define caml_weak_array_get caml_ephemeron_get_key
#define caml_weak_array_get_copy caml_ephemeron_get_key_copy
#define caml_weak_array_blit caml_ephemeron_blit_key

#ifdef CAML_INTERNALS

extern value caml_ephe_list_head;
extern value caml_ephe_none;


/** The first field 0:  weak list;
       second field 1:  data;
       others       2..:  keys;

    A weak pointer is an ephemeron with the data at caml_ephe_none
    If fields are added, don't forget to update weak.ml, [additional_values],
    and obj.ml, [Ephemeron.additional_values].


 */

#define CAML_EPHE_LINK_OFFSET 0
#define CAML_EPHE_DATA_OFFSET 1
#define CAML_EPHE_FIRST_KEY 2
#define CAML_EPHE_MAX_WOSIZE (Max_wosize - CAML_EPHE_FIRST_KEY)

/* In the header, in order to let major_gc.c
   and weak.c see the body of the function */
Caml_inline void caml_ephe_clean_partial (value v,
                                            mlsize_t offset_start,
                                            mlsize_t offset_end) {
  value child;
  int release_data = 0;
  mlsize_t i;
  CAMLassert(caml_gc_phase == Phase_clean);
  CAMLassert(2 <= offset_start
             && offset_start <= offset_end
             && offset_end <= Wosize_hd (Hd_val(v)));

  for (i = offset_start; i < offset_end; i++){
    child = Field (v, i);
  ephemeron_again:
    if (child != caml_ephe_none
        && Is_block (child) && Is_in_value_area (child)){
      if (Tag_val (child) == Forward_tag){
        value f = Forward_val (child);
        if (Is_block (f)) {
          if (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
              || Tag_val (f) == Lazy_tag || Tag_val (f) == Double_tag){
            /* Do not short-circuit the pointer. */
          }else{
            Field (v, i) = child = f;
            if (Is_block (f) && Is_young (f))
              add_to_ephe_ref_table(Caml_state_field(ephe_ref_table), v, i);
            goto ephemeron_again;
          }
        }
      }
      if (Tag_val (child) == Infix_tag) child -= Infix_offset_val (child);
      if (Is_white_val (child) && !Is_young (child)){
        release_data = 1;
        Field (v, i) = caml_ephe_none;
      }
    }
  }

  child = Field (v, 1);
  if(child != caml_ephe_none){
    if (release_data) Field (v, 1) = caml_ephe_none;
#ifdef DEBUG
    else if (offset_start == 2 && offset_end == Wosize_hd (Hd_val(v)) &&
             Is_block (child) && Is_in_heap (child)) {
      if (Tag_val (child) == Infix_tag) child -= Infix_offset_val (child);
      /* If we scanned all the keys and the data field remains filled,
         then the mark phase must have marked it */
      CAMLassert( !Is_white_val (child) );
    }
#endif
  }
}

Caml_inline void caml_ephe_clean (value v) {
  mlsize_t size;
  header_t hd;
  hd = Hd_val (v);
  size = Wosize_hd (hd);

  caml_ephe_clean_partial(v, 2, size);
}


#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_WEAK_H */
