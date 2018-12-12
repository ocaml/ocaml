/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                       Pierre Chambart, OCamlPro                        */
/*                   Mark Shinwell, Jane Street Europe                    */
/*                                                                        */
/*   Copyright 2013--2016 OCamlPro SAS                                    */
/*   Copyright 2014--2016 Jane Street Group LLC                           */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Runtime checks to try to catch errors in code generation.
   See flambda_to_clambda.ml for more information. */

#include <stdio.h>

#include "caml/mlvalues.h"

caml_value caml_check_value_is_closure(caml_value v, caml_value v_descr)
{
  const char* descr = String_val(v_descr);
  caml_value orig_v = v;

  if (v == (caml_value) 0) {
    fprintf(stderr, "NULL is not a closure: %s\n",
      descr);
    abort();
  }
  if (!Is_block(v)) {
    fprintf(stderr,
      "Expecting a closure, got a non-boxed caml_value %p: %s\n",
      (void*) v, descr);
    abort();
  }
  if (!(Tag_val(v) == Closure_tag || Tag_val(v) == Infix_tag)) {
    fprintf(stderr,
      "Expecting a closure, got a boxed caml_value with tag %i: %s\n",
      Tag_val(v), descr);
    abort();
  }
  if (Tag_val(v) == Infix_tag) {
    v -= Infix_offset_val(v);
    CAMLassert(Tag_val(v) == Closure_tag);
  }
  CAMLassert(Wosize_val(v) >= 2);

  return orig_v;
}

caml_value caml_check_field_access(caml_value v, caml_value pos, caml_value v_descr)
{
  const char* descr = String_val(v_descr);
  caml_value orig_v = v;
  if (v == (caml_value) 0) {
    fprintf(stderr,
      "Access to field %" ARCH_INT64_PRINTF_FORMAT
      "u of NULL: %s\n", (ARCH_UINT64_TYPE) Long_val(pos), descr);
    abort();
  }
  if (!Is_block(v)) {
    fprintf(stderr,
      "Access to field %" ARCH_INT64_PRINTF_FORMAT
      "u of non-boxed caml_value %p is illegal: %s\n",
      (ARCH_UINT64_TYPE) Long_val(pos), (void*) v, descr);
    abort();
  }
  if (Tag_val(v) == Infix_tag) {
    uintnat offset = Infix_offset_val(v);
    v -= offset;
    pos += offset / sizeof(caml_value);
  }
  CAMLassert(Long_val(pos) >= 0);
  if (Long_val(pos) >= Wosize_val(v)) {
    fprintf(stderr,
      "Access to field %" ARCH_INT64_PRINTF_FORMAT
      "u of caml_value %p of size %" ARCH_INT64_PRINTF_FORMAT "u is illegal: %s\n",
      (ARCH_UINT64_TYPE) Long_val(pos), (void*) v,
      (ARCH_UINT64_TYPE) Wosize_val(v),
      descr);
    abort();
  }
  return orig_v;
}
