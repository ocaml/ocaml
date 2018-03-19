/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* 1. Allocation functions doing the same work as the macros in the
      case where [Setup_for_gc] and [Restore_after_gc] are no-ops.
   2. Convenience functions related to allocation.
*/

#include <string.h>
#include <stdarg.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/fiber.h"
#include "caml/domain.h"

CAMLexport value caml_alloc (mlsize_t wosize, tag_t tag)
{
  value result;
  mlsize_t i;

  Assert (tag < 256);
  Assert (tag != Infix_tag);
  if (wosize == 0){
    result = Atom (tag);
  }else if (wosize <= Max_young_wosize){
    Alloc_small (result, wosize, tag, { caml_handle_gc_interrupt(); });
    if (tag < No_scan_tag){
      for (i = 0; i < wosize; i++) {
        value init_val = Val_unit;
        #ifdef DEBUG
          init_val = Debug_uninit_minor;
        #endif
        Op_val(result)[i] = init_val;
      }
    }

    if (tag == Stack_tag) Stack_sp(result) = 0;
  }else{
    result = caml_alloc_shr (wosize, tag);
    result = caml_check_urgent_gc(result);
  }

  return result;
}

static inline void enter_gc_preserving_vals(mlsize_t wosize, value* vals)
{
  CAMLparam0();
  /* Copy the values to be preserved to a different array.
     The original vals array never escapes, generating better code in
     the fast path. */
  CAMLlocalN(vals_copy, wosize);
  mlsize_t i;
  for (i = 0; i < wosize; i++) vals_copy[i] = vals[i];
  caml_handle_gc_interrupt();
  for (i = 0; i < wosize; i++) vals[i] = vals_copy[i];
  CAMLreturn0;
}

static inline value do_alloc_small(mlsize_t wosize, tag_t tag, value* vals)
{
  value v;
  mlsize_t i;
  Assert (tag < 256);
  Alloc_small(v, wosize, tag,
      { enter_gc_preserving_vals(wosize, vals); });
  for (i = 0; i < wosize; i++) {
    Op_val(v)[i] = vals[i];
  }
  return v;
}


CAMLexport value caml_alloc_1 (tag_t tag, value a)
{
  value v[1] = {a};
  return do_alloc_small(1, tag, v);
}

CAMLexport value caml_alloc_2 (tag_t tag, value a, value b)
{
  value v[2] = {a, b};
  return do_alloc_small(2, tag, v);
}

CAMLexport value caml_alloc_3 (tag_t tag, value a, value b, value c)
{
  value v[3] = {a, b, c};
  return do_alloc_small(3, tag, v);
}

CAMLexport value caml_alloc_4 (tag_t tag, value a, value b, value c, value d)
{
  value v[4] = {a, b, c, d};
  return do_alloc_small(4, tag, v);
}

CAMLexport value caml_alloc_5 (tag_t tag, value a, value b, value c, value d,
                               value e)
{
  value v[5] = {a, b, c, d, e};
  return do_alloc_small(5, tag, v);
}

CAMLexport value caml_alloc_6 (tag_t tag, value a, value b, value c, value d,
                               value e, value f)
{
  value v[6] = {a, b, c, d, e, f};
  return do_alloc_small(6, tag, v);
}

CAMLexport value caml_alloc_7 (tag_t tag, value a, value b, value c, value d,
                               value e, value f, value g)
{
  value v[7] = {a, b, c, d, e, f, g};
  return do_alloc_small(7, tag, v);
}

CAMLexport value caml_alloc_8 (tag_t tag, value a, value b, value c, value d,
                               value e, value f, value g, value h)
{
  value v[8] = {a, b, c, d, e, f, g, h};
  return do_alloc_small(8, tag, v);
}

CAMLexport value caml_alloc_9 (tag_t tag, value a, value b, value c, value d,
                               value e, value f, value g, value h, value i)
{
  value v[9] = {a, b, c, d, e, f, g, h, i};
  return do_alloc_small(9, tag, v);
}

CAMLexport value caml_alloc_N (mlsize_t wosize, tag_t tag, ...)
{
  va_list args;
  mlsize_t i;
  value vals[wosize];
  value ret;
  va_start(args, tag);
  for (i = 0; i < wosize; i++)
    vals[i] = va_arg(args, value);
  ret = do_alloc_small(wosize, tag, vals);
  va_end(args);
  return ret;
}


CAMLexport value caml_alloc_small (mlsize_t wosize, tag_t tag)
{
  return caml_alloc(wosize, tag);
}

CAMLexport value caml_alloc_small_with_my_or_given_profinfo (mlsize_t wosize,
  tag_t tag, uintnat profinfo)
{
  if (profinfo == 0) {
    return caml_alloc_small(wosize, tag);
  }
  else {
    value result;

    Assert (wosize > 0);
    Assert (wosize <= Max_young_wosize);
    Assert (tag < 256);
    Alloc_small_with_profinfo (result, wosize, tag,
      { caml_handle_gc_interrupt(); }, profinfo);
    return result;
  }
}

/* [n] is a number of words (fields) */
CAMLexport value caml_alloc_tuple(mlsize_t n)
{
  return caml_alloc(n, 0);
}

/* [len] is a number of bytes (chars) */
CAMLexport value caml_alloc_string (mlsize_t len)
{
  mlsize_t offset_index;
  mlsize_t wosize = (len + sizeof (value)) / sizeof (value);
  value result = caml_alloc(wosize, String_tag);

  Op_val (result) [wosize - 1] = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
  return result;
}

/* [len] is a number of words.
   [mem] and [max] are relative (without unit).
*/
CAMLexport value caml_alloc_final (mlsize_t len, final_fun fun,
                                   mlsize_t mem, mlsize_t max)
{
  return caml_alloc_custom(caml_final_custom_operations(fun),
                           len * sizeof(value), mem, max);
}

CAMLexport value caml_copy_string(char const *s)
{
  int len;
  value res;

  len = strlen(s);
  res = caml_alloc_string(len);
  memmove(String_val(res), s, len);
  return res;
}

CAMLexport value caml_alloc_array(value (*funct)(char const *),
                                  char const * const* arr)
{
  CAMLparam0 ();
  mlsize_t nbr, n;
  CAMLlocal2 (v, result);

  nbr = 0;
  while (arr[nbr] != 0) nbr++;
  if (nbr == 0) {
    CAMLreturn (Atom(0));
  } else {
    result = caml_alloc (nbr, 0);
    for (n = 0; n < nbr; n++) {
      caml_modify_field(result, n, funct(arr[n]));
    }
    CAMLreturn (result);
  }
}

/* [len] is a number of floats */
CAMLprim value caml_alloc_float_array(mlsize_t len)
{
  mlsize_t wosize = len * Double_wosize;
  value result;
  if (wosize == 0)
    return Atom(0);
  else if (wosize <= Max_young_wosize){
    Alloc_small (result, wosize, Double_array_tag, { caml_handle_gc_interrupt(); });
  }else {
    result = caml_alloc_shr (wosize, Double_array_tag);
    result = caml_check_urgent_gc (result);
  }
  return result;
}


CAMLexport value caml_copy_string_array(char const * const * arr)
{
  return caml_alloc_array(caml_copy_string, arr);
}

CAMLexport int caml_convert_flag_list(value list, const int *flags)
{
  int res;
  res = 0;
  while (list != Val_int(0)) {
    res |= flags[Int_field(list, 0)];
    list = Field_imm(list, 1);
  }
  return res;
}

/* For compiling let rec over values */

/* [size] is a [value] representing number of words (fields) */
CAMLprim value caml_alloc_dummy(value size)
{
  mlsize_t wosize = Long_val(size);

  if (wosize == 0) return Atom(0);
  return caml_alloc (wosize, 0);
}

/* [size] is a [value] representing number of words (fields) */
CAMLprim value caml_alloc_dummy_function(value size,value arity)
{
  /* the arity argument is used by the js_of_ocaml runtime */
  return caml_alloc_dummy(size);
}

/* [size] is a [value] representing number of floats. */
CAMLprim value caml_alloc_dummy_float (value size)
{
  mlsize_t wosize = Long_val(size) * Double_wosize;

  if (wosize == 0) return Atom(0);
  return caml_alloc (wosize, 0);
}

CAMLprim value caml_update_dummy(value dummy, value newval)
{
  CAMLparam2(dummy, newval);
  CAMLlocal1(x);
  mlsize_t size, i;
  tag_t tag;

  size = Wosize_val(newval);
  tag = Tag_val (newval);
  Assert (size == Wosize_val(dummy));
  Assert (tag < No_scan_tag || tag == Double_array_tag);

  Tag_val(dummy) = tag;
  if (tag == Double_array_tag){
    size = Wosize_val (newval) / Double_wosize;
    for (i = 0; i < size; i++){
      Store_double_field (dummy, i, Double_field (newval, i));
    }
  }else{
    for (i = 0; i < size; i++){
      caml_read_field(newval, i, &x);
      caml_modify_field (dummy, i, x);
    }
  }
  CAMLreturn (Val_unit);
}




