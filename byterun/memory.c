#include <string.h>
#include <stdlib.h>
#include "misc.h"
#include "fail.h"
#include "memory.h"


CAMLexport void caml_modify_field (value obj, int field, value val)
{
  Assert (Is_block(obj));

  Field(obj, field) = val;
  if (!Is_young(obj) && Is_block(val) && Is_young(val)) {
    /* Add [fp] to remembered set */
    if (caml_ref_table.ptr >= caml_ref_table.limit){
      CAMLassert (caml_ref_table.ptr == caml_ref_table.limit);
      caml_realloc_ref_table (&caml_ref_table);
    }
    *caml_ref_table.ptr++ = &Field(obj, field);
  }
}

CAMLexport void caml_initialize_field (value obj, int field, value val)
{
  Field(obj, field) = Val_long(0);
  caml_modify_field(obj, field, val);
}

CAMLexport value caml_alloc_shr (mlsize_t wosize, tag_t tag)
{
  value* v = malloc( Bhsize_wosize (wosize) );
  if (v == NULL) {
    caml_raise_out_of_memory ();
  }
  Hd_hp (v) = Make_header(wosize, tag, Caml_black);
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


