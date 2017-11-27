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

val apply_arg : string
val apply_funct : string
val block_symbol : string
val block_symbol_get : string
val block_symbol_get_field : string
val closure : string
val cond : string
val cond_sequor : string
val const_block : string
val const_bool : string
val const_boxed_int : string
val const_char : string
val const_false : string
val const_float : string
val const_int : string
val const_one : string
val const_ptr : string
val const_ptr_one : string
val const_ptr_zero : string
val const_sequand : string
val const_string : string
val const_true : string
val const_zero : string
val denominator : string
val division_by_zero : string
val dummy : string
val dup_func : string
val dup_set_of_closures : string
val const_float_array : string
val fake_effect_symbol : string
val for_from : string
val for_to : string
val from_closure : string
val full_apply : string
val get_symbol_field : string
val const_immstring : string
val const_int32 : string
val const_int64 : string
val is_zero : string
val lifted_let_rec_block : string
val meth : string
val module_as_block : string
val const_nativeint : string
val new_value : string
val numerator : string
val obj : string
val offsetted : string
val partial_fun : string
val pgetglobal : string
val pointer : string
val predef_exn : string
val project_closure : string
val raise : string
val raise_arg : string
val read_mutable : string
val remove_unused_arguments : string
val result : string
val send_arg : string
val sequence : string
val set_of_closures : string
val staticraise_arg : string
val simplify_fv : string
val string_switch : string
val switch : string
val symbol : string
val symbol_field : string
val symbol_field_block : string
val the_dead_constant : string
val toplevel_substitution_named : string
val unbox_free_vars_of_closures : string
val zero : string

val of_primitive : Lambda.primitive -> string

val of_primitive_arg : Lambda.primitive -> string

val anon_fn_with_loc : Location.t -> string
