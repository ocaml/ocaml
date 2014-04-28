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

CAMLexport void * caml_stat_alloc (asize_t sz)
{
  void * result = malloc (sz);

  /* malloc() may return NULL if size is 0 */
  if (result == NULL && sz != 0) caml_raise_out_of_memory ();
#ifdef DEBUG
  memset (result, Debug_uninit_stat, sz);
#endif
  return result;
}

CAMLexport char * caml_stat_alloc_string(value str)
{
  mlsize_t sz = caml_string_length(str) + 1;
  char * p = caml_stat_alloc(sz);
  memcpy(p, String_val(str), sz);
  return p;
}

CAMLexport void caml_stat_free (void * blk)
{
  free (blk);
}

CAMLexport void * caml_stat_resize (void * blk, asize_t sz)
{
  void * result = realloc (blk, sz);

  if (result == NULL) caml_raise_out_of_memory ();
  return result;
}


