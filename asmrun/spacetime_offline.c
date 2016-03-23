/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Mark Shinwell and Leo White, Jane Street Europe             */
/*                                                                        */
/*   Copyright 2013--2016, Jane Street Group, LLC                         */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <sys/resource.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "spacetime.h"
#include "stack.h"

#include "../config/s.h"

const uintnat caml_profinfo_lowest = (uintnat) 2;

CAMLprim value caml_spacetime_compare_node(
      value node1, value node2)
{
  Assert(!Is_in_value_area(node1));
  Assert(!Is_in_value_area(node2));

  if (node1 == node2) {
    return Val_long(0);
  }
  if (node1 < node2) {
    return Val_long(-1);
  }
  return Val_long(1);
}

CAMLprim value caml_spacetime_unmarshal_trie (value v_channel)
{
  return caml_input_value_to_outside_heap(v_channel);
}

CAMLprim value caml_spacetime_node_num_header_words(value unit)
{
  unit = Val_unit;
  return Val_long(Node_num_header_words);
}

CAMLprim value caml_spacetime_is_ocaml_node(value node)
{
  if (!(Is_ocaml_node(node) || Is_c_node(node))) {
    printf("is_ocaml_node: (value) node = %p has the wrong tag\n",
      (void*) node);
  }
  Assert(Is_ocaml_node(node) || Is_c_node(node));
  return Val_bool(Is_ocaml_node(node));
}

CAMLprim value caml_spacetime_ocaml_function_identifier(value node)
{
  Assert(Is_ocaml_node(node));
  return caml_copy_int64((uint64_t) Decode_node_pc(Node_pc(node)));
}

CAMLprim value caml_spacetime_ocaml_tail_chain(value node)
{
  Assert(Is_ocaml_node(node));
  return Tail_link(node);
}

CAMLprim value caml_spacetime_ocaml_classify_field(value node,
      value offset)
{
  /* Note that [offset] should always point at an initialized call or
     allocation point, by virtue of the behaviour of the function
     [caml_spacetime_ocaml_node_next], below. */

  uintnat field;

  Assert(!Is_block(offset));
  field = Long_val(offset);

  Assert(Is_ocaml_node(node));
  Assert(field >= Node_num_header_words);
  Assert(field < Wosize_val(node));

  Assert(Field(node, field) != Val_unit);

#if 0
  switch (Call_or_allocation_point(node, field)) {
    case CALL: {
      value callee_node;
      value second_word;
      Assert(field < Wosize_val(node) - 1);
      second_word = Indirect_pc_linked_list(node, field);
      Assert(second_word != Val_unit);
      if (Is_block(second_word)) {
        return Val_long(4);  /* indirect call point */
      }
      callee_node = Direct_callee_node(node, field);
      if (callee_node == Val_unit) {
        return Val_long(1);  /* direct call point to uninstrumented code */
      } else if (Is_ocaml_node(callee_node)) {
        return Val_long(2);  /* direct call point to OCaml code */
      } else {
        return Val_long(3);  /* direct call point to non-OCaml code */
      }
    }

    case ALLOCATION:
      Assert(field < Wosize_val(node) - 1);
      return Val_long(0);
  }
#endif

  Assert(0);
  return Val_unit;  /* silence compiler warning */
}

CAMLprim value caml_spacetime_ocaml_node_skip_uninitialized
      (value node, value offset)
{
  uintnat field = Long_val(offset);

  Assert(Is_ocaml_node(node));
  Assert(field >= Node_num_header_words);
  Assert(field < Wosize_val(node));

  for (/* nothing */; field < Wosize_val(node); field++) {
    value entry;

    entry = Field(node, field);

    if (entry == Val_unit) {
      continue;
    }

    if (entry == (value) 3) /*(Encode_tail_caller_node(node))*/ {
      /* Middle word of uninitialized direct tail call point. */
      Assert (field >= Node_num_header_words + 1);
      field++; /* skip the node pointer (third word of the group) */
      continue;
    }
    return Val_long(field);
  }

  return Val_long(-1);
}

CAMLprim value caml_spacetime_ocaml_node_next(value node,
      value offset)
{
#if 0
  uintnat field = Long_val(offset);

  Assert(Is_ocaml_node(node));
  Assert(field >= Node_num_header_words);
  Assert(field < Wosize_val(node));

  switch (Call_or_allocation_point(node, field)) {
    case CALL: {
      value second_word;
      Assert(field < Wosize_val(node) - 1);
      second_word = Indirect_pc_linked_list(node, field);
      Assert(second_word != Val_unit);
      if (Is_block(second_word)) {
        /* This is an indirect call point. */
        field += 2;
      }
      else {
        /* This is a direct call point. */
        Assert(field < Wosize_val(node) - 2);
        field += 3;
      }
      break;
    }

    case ALLOCATION:
      Assert(field < Wosize_val(node) - 1);
      field += 2;
      break;

    default:
      Assert(0);
  }

  if (field < Wosize_val(node)) {
    return caml_spacetime_ocaml_node_skip_uninitialized
        (node, Val_long(field));
  }
#endif

  return Val_long(-1);
}

CAMLprim value caml_spacetime_ocaml_allocation_point_program_counter
      (value node, value offset)
{
return Val_unit;
#if 0
  return caml_copy_int64((uint64_t) Decode_alloc_point_pc(
    Alloc_point_pc(node, Long_val(offset))));
#endif
}

CAMLprim value caml_spacetime_ocaml_allocation_point_annotation
      (value node, value offset)
{
  uintnat profinfo_shifted;
  profinfo_shifted = (uintnat) Alloc_point_profinfo(node, Long_val(offset));
  return Val_long(profinfo_shifted >> PROFINFO_SHIFT);
}

CAMLprim value caml_spacetime_ocaml_direct_call_point_call_site
      (value node, value offset)
{
return Val_unit;
#if 0
  return caml_copy_int64((int64_t) Decode_call_point_pc(
      Direct_pc_call_site(node, Long_val(offset))));
#endif
}

CAMLprim value caml_spacetime_ocaml_direct_call_point_callee
      (value node, value offset)
{
  return caml_copy_int64((int64_t) Decode_call_point_pc(
      Direct_pc_callee(node, Long_val(offset))));
}

CAMLprim value caml_spacetime_ocaml_direct_call_point_callee_node
      (value node, value offset)
{
  return Direct_callee_node(node, Long_val(offset));
}

CAMLprim value caml_spacetime_ocaml_indirect_call_point_call_site
      (value node, value offset)
{
#if 0
  return caml_copy_int64((int64_t) Decode_call_point_pc(
    Indirect_pc_call_site(node, Long_val(offset))));
#endif
  return Val_unit;
}

CAMLprim value caml_spacetime_ocaml_indirect_call_point_callees
      (value node, value offset)
{
  value callees = Indirect_pc_linked_list(node, Long_val(offset));
  Assert(Is_block(callees));
  Assert(Is_c_node(callees));
  return callees;
}

CAMLprim value caml_spacetime_c_node_is_call(value node)
{
  c_node* c_node;
  Assert(node != (value) NULL);
  Assert(Is_c_node(node));
  c_node = caml_spacetime_c_node_of_stored_pointer_not_null(node);
  switch (caml_spacetime_classify_c_node(c_node)) {
    case CALL: return Val_true;
    case ALLOCATION: return Val_false;
  }
  Assert(0);
  return Val_unit;  /* silence compiler warning */
}

CAMLprim value caml_spacetime_c_node_next(value node)
{
  c_node* c_node;
/*printf("c_node_next: value node=%p\n", (void*) node);*/
  Assert(node != (value) NULL);
  Assert(Is_c_node(node));
  c_node = caml_spacetime_c_node_of_stored_pointer_not_null(node);
/*printf("c_node_next: next ptr=%p\n", (void*) c_node->next);*/
  Assert(c_node->next == Val_unit || Is_c_node(c_node->next));
  return c_node->next;
}

CAMLprim value caml_spacetime_c_node_call_site(value node)
{
  c_node* c_node;
  Assert(node != (value) NULL);
  Assert(Is_c_node(node));
  c_node = caml_spacetime_c_node_of_stored_pointer_not_null(node);
  return caml_copy_int64((uint64_t) Decode_c_node_pc(c_node->pc));
}

CAMLprim value caml_spacetime_c_node_callee_node(value node)
{
  c_node* c_node;
  Assert(node != (value) NULL);
  Assert(Is_c_node(node));
  c_node = caml_spacetime_c_node_of_stored_pointer_not_null(node);
  Assert(caml_spacetime_classify_c_node(c_node) == CALL);
  /* This might be an uninitialised tail call point: for example if an OCaml
     callee was indirectly called but the callee wasn't instrumented (e.g. a
     leaf function that doesn't allocate). */
  if (Is_tail_caller_node_encoded(c_node->data.callee_node)) {
    return Val_unit;
  }
  return c_node->data.callee_node;
}

CAMLprim value caml_spacetime_c_node_profinfo(value node)
{
  c_node* c_node;
  Assert(node != (value) NULL);
  Assert(Is_c_node(node));
  c_node = caml_spacetime_c_node_of_stored_pointer_not_null(node);
  Assert(caml_spacetime_classify_c_node(c_node) == ALLOCATION);
  Assert(!Is_block(c_node->data.profinfo));
  return c_node->data.profinfo;
}
