/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* For testing global root registration */

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/gc.h"

struct block { value header; caml_root v; };

#define Root_val(v) *((caml_root*) Data_abstract_val(v))

value gb_get(value vblock)
{
  CAMLparam1 (vblock);
  CAMLreturn (caml_read_root(Root_val(vblock)));
}

value gb_classic_register(value v)
{
  CAMLparam1(v);
  CAMLlocal1(b);
  caml_root r;
  b = caml_alloc(1, Abstract_tag);
  r = caml_create_root(v);
  Root_val(b) = r;
  CAMLreturn (b);
}

value gb_classic_set(value vblock, value newval)
{
  CAMLparam2(vblock, newval);
  caml_modify_root(Root_val(vblock), newval);
  CAMLreturn (Val_unit);
}

value gb_classic_remove(value vblock)
{
  CAMLparam1(vblock);
  caml_delete_root(Root_val(vblock));
  CAMLreturn (Val_unit);
}

value gb_generational_register(value v)
{
  return gb_classic_register(v);
}

value gb_generational_set(value vblock, value newval)
{
  return gb_classic_set(vblock, newval);
}

value gb_generational_remove(value vblock)
{
  return gb_classic_remove(vblock);
}
