/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Operations on weak arrays */

#include <string.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"


/* Stub implementation of "weak" pointers */

value caml_array_blit(value, value, value, value, value); /* array.c */

#define None_val (Val_int(0))

CAMLprim value caml_weak_create (value len)
{
  value res = caml_alloc(len, 0);
  int i;
  for (i = 0; i < len; i++) 
    caml_initialize_field(res, i, None_val);
  return res;
}

CAMLprim value caml_weak_set (value ar, value n, value el)
{
  CAMLparam3(ar, n, el);
  int idx = Int_val(n);
  if (idx < 0 || idx >= Wosize_val(ar)) caml_array_bound_error();
  caml_modify_field(ar, idx, el);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_weak_get (value ar, value n)
{
  CAMLparam2(ar, n);
  CAMLlocal1(x);
  int idx = Int_val(n);
  if (idx < 0 || idx >= Wosize_val(ar)) caml_array_bound_error();
  caml_read_field(ar, idx, &x);
  CAMLreturn (x);
}

CAMLprim value caml_weak_get_copy (value ar, value n)
{
  caml_failwith("weak_get_copy unsupported");
}

CAMLprim value caml_weak_check (value ar, value n)
{
  caml_failwith("weak_check unsupported");
}

CAMLprim value caml_weak_blit (value ars, value ofs,
                               value ard, value ofd, value len)
{
  return caml_array_blit(ars, ofs, ard, ofd, len);
}
