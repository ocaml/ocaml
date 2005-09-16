/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Luc Maranget, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2005 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdlib.h>
#include <stdio.h>

#include "mlvalues.h"
#include "alloc.h"
#include "memory.h"
#include "custom.h"


static struct custom_operations caml_automaton_ops = {
  "_a",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};


/* Allocate automaton structure, cf. the automaton type in join.ml */

CAMLprim value caml_alloc_stub(value a)
{
  CAMLparam1 (a) ;
  CAMLlocal1 (res) ;
  res = caml_alloc_small(2, JoCustom_tag) ;
  Field(res, 0) = (value)&caml_automaton_ops ;
  Field(res, 1) = a ;
  CAMLreturn (res) ;
}
