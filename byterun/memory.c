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
    caml_darken(val);
  }


      Assert (!Is_block(val) || Wosize_hd (Hd_val (val)) < (1 << 20)); /* !! */


  if (!Is_young(obj)) {
    
    if (Is_block(val) && Is_young(val)) {



      /* Add [fp] to remembered set */
      if (caml_ref_table.ptr >= caml_ref_table.limit){
        CAMLassert (caml_ref_table.ptr == caml_ref_table.limit);
        caml_realloc_ref_table (&caml_ref_table);
      }
      *caml_ref_table.ptr++ = &Field(obj, field);      
    }
  }

  Field(obj, field) = val;
}

CAMLexport void caml_initialize_field (value obj, int field, value val)
{
  Field(obj, field) = Val_long(0);
  caml_modify_field(obj, field, val);
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


