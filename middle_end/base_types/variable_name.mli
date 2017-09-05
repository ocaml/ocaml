(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Fu Yong Quah, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type base =
  | Apply_arg
  | Apply_funct
  | Anon_fn of string option
  | Arbitary of string
  | Block_symbol of int
  | Block_symbol_get of int
  | Block_symbol_get_field of int
  | Char
  | Closure
  | Cond
  | Cond_sequor
  | Const_block
  | Const_bool
  | Const_boxed_int
  | Const_char
  | Const_false
  | Const_float
  | Const_int
  | Const_one
  | Const_ptr
  | Const_ptr_one
  | Const_ptr_zero
  | Const_sequand
  | Const_string
  | Const_true
  | Const_zero
  | Denominator
  | Division_by_zero
  | Dummy
  | Dup_func
  | Dup_set_of_closures
  | Float
  | Const_float_array
  | For_from
  | For_to
  | From_closure
  | Full_apply
  | Get_symbol_field
  | Const_immstring
  | Int
  | Const_int32
  | Const_int64
  | Is_zero
  | Lifted_let_rec_block
  | Meth
  | Const_nativeint
  | New_value
  | Numerator
  | Obj
  | Offsetted
  | Pgetglobal
  | Pointer
  | Predef_exn
  | Project_closure
  | Project_closure_anon_fn of string option
  | Raise
  | Raise_arg
  | Read_mutable
  | Remove_unused_arguments
  | Result
  | Send_arg
  | Sequence
  | Set_of_closures
  | Set_of_closures_anon_fn of string option
  | Staircase_arg
  | Simplify_fv
  | String_switch
  | Switch
  | Symbol
  | Symbol_field
  | Symbol_field_block
  | Toplevel_substitution_named
  | Unbox_free_vars_of_closures
  | Zero

type t = (base * string)

val to_string : t -> string
