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

#include "alloc.h"
#include "fail.h"
#include "major_gc.h"
#include "memory.h"
#include "mlvalues.h"


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
  caml_modify_field(ar, n, el);
  return Val_unit;
}

CAMLprim value caml_weak_get (value ar, value n)
{
  return Field(ar, n);
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
