/**************************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*     en Automatique.                                                    */
/*                                                                     */
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

#define None_val (Val_int(0))
#define Some_tag 0

/* [len] is a value that represents a number of words (fields) */
CAMLprim value caml_ephe_create (value len)
{
  mlsize_t size, i;
  value res;

  size = Long_val (len) + 1 /* weak_list */ + 1 /* the value */;
  if (size <= 0 || size > Max_wosize) caml_invalid_argument ("Weak.create");
  res = caml_alloc_shr (size, 0);
  for (i = 1; i < size; i++)
    caml_initialize_field(res, i, None_val);
  return res;
}

CAMLprim value caml_weak_create (value len)
{
  return caml_ephe_create(len);
}

CAMLprim value caml_ephe_set_key (value ar, value n, value el)
{
  CAMLparam3(ar,n,el);
  mlsize_t offset = Long_val (n) + 2;
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.set");
  }
  caml_modify_field (ar, offset, el);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ephe_unset_key (value ar, value n)
{
  CAMLparam2(ar,n);
  mlsize_t offset = Long_val (n) + 2;
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.set");
  }
  caml_modify_field (ar, offset, None_val);
  CAMLreturn(Val_unit);
}

value caml_ephe_set_key_option (value ar, value n, value el)
{
  CAMLparam3(ar,n,el);
  mlsize_t offset = Long_val (n) + 2;
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.set");
  }
  if (el != None_val && Is_block (el)){
    CAMLassert (Wosize_val (el) == 1);
    caml_modify_field (ar, offset, Field (el, 0));
  }else{
    caml_modify_field (ar, offset, None_val);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_weak_set (value ar, value n, value el){
  return caml_ephe_set_key_option(ar,n,el);
}

CAMLprim value caml_ephe_set_data (value ar, value el)
{
  CAMLparam2(ar,el);
  caml_modify_field (ar, 1, el);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ephe_unset_data (value ar)
{
  CAMLparam1(ar);
  caml_modify_field (ar, CAML_EPHE_DATA_OFFSET, None_val);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ephe_get_key (value ar, value n)
{
  CAMLparam2(ar, n);
  mlsize_t offset = Long_val (n) + 2;
  CAMLlocal2 (res, elt);
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.get_key");
  }
  caml_read_field(ar, offset, &elt);
  if (elt == None_val){
    res = None_val;
  }else{
    res = caml_alloc_small (1, Some_tag);
    caml_initialize_field(res, 0, elt);
  }
  CAMLreturn (res);
}

CAMLprim value caml_weak_get (value ar, value n){
  return caml_ephe_get_key(ar, n);
}

CAMLprim value caml_ephe_get_data (value ar)
{
  CAMLparam1 (ar);
  mlsize_t offset = 1;
  CAMLlocal2 (res, elt);
  caml_read_field (ar, offset, &elt);
  if (elt == None_val){
    res = None_val;
  }else{
    res = caml_alloc_small (1, Some_tag);
    caml_initialize_field(res, 0, elt);
  }
  CAMLreturn (res);
}

CAMLprim value caml_ephe_get_key_copy (value ar, value n)
{
  caml_failwith("caml_ephe_get_key_copy: not implemented");
}

CAMLprim value caml_weak_get_copy (value ar, value n){
  return caml_ephe_get_key_copy(ar,n);
}

CAMLprim value caml_ephe_get_data_copy (value ar)
{
  caml_failwith("caml_ephe_get_data_copy: not implemented");
}

CAMLprim value caml_ephe_check_key (value ar, value n)
{
  CAMLparam2(ar,n);
  CAMLlocal1(v);
  mlsize_t offset = Long_val (n) + 2;
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.check");
  }
  caml_read_field (ar, offset, &v);
  CAMLreturn(Val_bool (v != None_val));
}

CAMLprim value caml_weak_check (value ar, value n)
{
  return caml_ephe_check_key(ar,n);
}

CAMLprim value caml_ephe_check_data (value ar)
{
  CAMLparam1(ar);
  CAMLlocal1(v);

  caml_read_field(ar, CAML_EPHE_DATA_OFFSET, &v);
  CAMLreturn(Val_bool (v != None_val));
}

CAMLprim value caml_ephe_blit_key (value ars, value ofs,
                               value ard, value ofd, value len)
{
  CAMLparam2(ars, ard);
  CAMLlocal1(v);
  mlsize_t offset_s = Long_val (ofs) + 2;
  mlsize_t offset_d = Long_val (ofd) + 2;
  mlsize_t length = Long_val (len);
  long i;
  if (offset_s < 1 || offset_s + length > Wosize_val (ars)){
    caml_invalid_argument ("Weak.blit");
  }
  if (offset_d < 1 || offset_d + length > Wosize_val (ard)){
    caml_invalid_argument ("Weak.blit");
  }
  if (offset_d < offset_s){
    for (i = 0; i < length; i++){
      caml_read_field(ars, offset_s + i, &v);
      caml_modify_field (ard, offset_d + i, v);
    }
  }else{
    for (i = length - 1; i >= 0; i--){
      caml_read_field(ars, offset_s + i, &v);
      caml_modify_field (ard, offset_d + i, v);
    }
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ephe_blit_data (value ars, value ard)
{
#if 0
  if(caml_gc_phase == Phase_clean) {
    caml_ephe_clean(ars);
    caml_ephe_clean(ard);
  };
  do_set (ard, CAML_EPHE_DATA_OFFSET, Field (ars, CAML_EPHE_DATA_OFFSET));
#endif
  return Val_unit;
}

CAMLprim value caml_weak_blit (value ars, value ofs,
                      value ard, value ofd, value len)
{
  return caml_ephe_blit_key (ars, ofs, ard, ofd, len);
}
