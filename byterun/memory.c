#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "misc.h"
#include "fail.h"
#include "memory.h"
#include "shared_heap.h"
#include "domain.h"

CAMLexport void caml_modify_field (value obj, int field, value val)
{
  Assert (Is_block(obj));
  Assert (!Is_foreign(obj));
  Assert (!Is_foreign(val));
  /* 
     FIXME: should have an is_marking check
     don't want to do this all the time
     
     unconditionally mark new value
  */

  if (Is_block(val) && !Is_young(val)) {
    caml_darken(val, 0);
  }


      Assert (!Is_block(val) || Wosize_hd (Hd_val (val)) < (1 << 20)); /* !! */


  if (!Is_young(obj)) {
    
    if (Is_block(val) && Is_young(val)) {



      /* Add [fp] to remembered set */
      if (caml_ref_table.ptr >= caml_ref_table.limit){
        CAMLassert (caml_ref_table.ptr == caml_ref_table.limit);
        caml_realloc_ref_table (&caml_ref_table);
      }
      *caml_ref_table.ptr++ = Op_val(obj) + field;
    }
  }
  
  Op_val(obj)[field] = val;
}

CAMLexport void caml_initialize_field (value obj, int field, value val)
{
  Op_val(obj)[field] = Val_long(0);
  caml_modify_field(obj, field, val);
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
    if (Is_young(dst)) {
      /* dst is young, we copy fields directly. This cannot create old->young
         ptrs, nor break incremental GC of the shared heap */
      for (i = n; i > 0; i--) {
        Op_val(dst)[dstoff + i - 1] = Op_val(src)[srcoff + i - 1];
      }
    } else {
      for (i = n; i > 0; i--) {
        caml_modify_field(dst, dstoff + i - 1, Field(src, srcoff + i - 1));
      }
    }
  } else {
    /* copy ascending */
    if (Is_young(dst)) {
      /* see comment above */
      for (i = 0; i < n; i++) {
        Op_val(dst)[dstoff + i] = Field(src, srcoff + i);
      }
    } else {
      for (i = 0; i < n; i++) {
        caml_modify_field(dst, dstoff + i, Field(src, srcoff + i));
      }
    }
  }
}

CAMLexport value caml_alloc_shr (mlsize_t wosize, tag_t tag)
{
  value* v = caml_shared_try_alloc(wosize, tag);
  if (v == NULL) {
    /* FIXME: trigger GC */
    caml_raise_out_of_memory ();
  }
  return Val_hp(v);
}


