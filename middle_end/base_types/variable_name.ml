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

type t = base * string

let base_to_string = function
  | Apply_arg -> "apply_arg"
  | Apply_funct -> "apply_funct"
  | Anon_fn location ->
    begin match location with
    | Some loc -> Format.asprintf "anon-fn[%s]" loc
    | None -> "anon-fn"
    end
  | Arbitary s -> s
  | Block_symbol pos -> "block_symbol_" ^ string_of_int pos
  | Block_symbol_get pos -> "block_symbol_get_" ^ string_of_int pos
  | Block_symbol_get_field pos ->
    "block_symbol_get_field_" ^ string_of_int pos
  | Char -> "char"
  | Closure -> "closure"
  | Cond -> "cond"
  | Cond_sequor -> "cond_sequor"
  | Const_bool -> "const_bool"
  | Const_block -> "const_block"
  | Const_boxed_int -> "const_boxed_int"
  | Const_char -> "const_char"
  | Const_int -> "const_int"
  | Const_false -> "const_false"
  | Const_float -> "const_float"
  | Const_one -> "const_one"
  | Const_ptr -> "const_ptr"
  | Const_ptr_one -> "const_ptr_one"
  | Const_ptr_zero -> "const_ptr_zero"
  | Const_sequand -> "const_sequand"
  | Const_string -> "const_string"
  | Const_true -> "const_true"
  | Const_zero -> "const_zero"
  | Denominator -> "denominator"
  | Division_by_zero -> "division_by_zero"
  | Dummy -> "dummy"
  | Dup_func -> "dup_func"
  | Dup_set_of_closures -> "dup_set_of_closures"
  | Float -> "float"
  | Const_float_array -> "float_array"
  | For_from -> "for_from"
  | For_to -> "for_to"
  | From_closure -> "from_closure"
  | Full_apply -> "full_apply"
  | Get_symbol_field -> "get_symbol_field"
  | Const_immstring -> "immstring"
  | Is_zero -> "is_zero"
  | Lifted_let_rec_block -> "lifted_let_rec_block"
  | Int -> "int"
  | Const_int32 -> "const_int32"
  | Const_int64 -> "const_int64"
  | Meth -> "meth"
  | Const_nativeint -> "const_nativeint"
  | New_value -> "new_value"
  | Numerator -> "numerator"
  | Obj -> "obj"
  | Offsetted -> "offsetted"
  | Pgetglobal -> "Pgetglobal"
  | Pointer -> "pointer"
  | Predef_exn -> "predef_exn"
  | Project_closure -> "project_closure"
  | Project_closure_anon_fn loc ->
    begin match loc with
    | Some l -> Format.sprintf "project_closure_anon_fn[%s]" l
    | None -> "project_closure_anon_fn"
    end
  | Raise -> "raise"
  | Raise_arg -> "raise_arg"
  | Read_mutable -> "read_mutable"
  | Remove_unused_arguments -> "remove_unused_arguments"
  | Result -> "result"
  | Send_arg -> "send_arg"
  | Sequence -> "sequence"
  | Set_of_closures -> "set_of_closures"
  | Set_of_closures_anon_fn location ->
    begin match location with
    | Some l -> Format.sprintf "set_of_closures_anon_fn[%s]" l
    | None -> "set_of_closures_anon_fn"
    end
  | Simplify_fv -> "simplify_fv"
  | Staircase_arg -> "staircase_arg"
  | String_switch -> "string_switch"
  | Switch -> "switch"
  | Symbol -> "symbol"
  | Symbol_field -> "symbol_field"
  | Symbol_field_block -> "symbol_field_block"
  | Toplevel_substitution_named -> "toplevel_substitution_named"
  | Unbox_free_vars_of_closures -> "unbox_free_vars_of_closures"
  | Zero -> "zero"

let to_string (base, s) = base_to_string base ^ s
