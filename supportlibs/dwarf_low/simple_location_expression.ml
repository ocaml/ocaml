(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Operator = Dwarf_operator

module type S = sig
  type t

  val const_symbol : Symbol.t -> t
  val const_int : Int64.t -> t
  val in_register : reg_number:int -> t
  val in_stack_slot : offset_in_words:int -> t
  val read_symbol_field : symbol:Symbol.t -> field:int -> t
  val read_field : t -> field:int -> t
  val offset_pointer : t -> offset_in_words:int -> t
end

type t =
  | Const_symbol of Symbol.t
  | Const_int of Int64.t
  | In_register of int
  | In_stack_slot of { offset_in_words : int; }
  | Offset_pointer of { block : t; offset_in_words : int; }
  | Read_field of { block : t; field : int; }

let const_symbol symbol = Const_symbol symbol
let const_int i = Const_int i
let in_register ~reg_number = In_register reg_number
let in_stack_slot ~offset_in_words = In_stack_slot { offset_in_words; }
let read_symbol_field ~symbol ~field =
  Read_field { block = Const_symbol symbol; field; }
let offset_pointer t ~offset_in_words =
  Offset_pointer { block = t; offset_in_words; }
let read_field t ~field = Read_field { block = t; field; }

let rec compile_to_yield_value t =
  (* We first compile the expression to a DWARF expression that always yields
     the *value* of the corresponding variable rather than to a location that
     contains the value (which may sometimes not exist, e.g. in the [Symbol]
     case). *)
  match t with
  | Const_symbol symbol -> [Operator.value_of_symbol symbol]
  | Const_int i -> [Operator.signed_int_const i]
  | In_register reg_number -> [Operator.contents_of_register ~reg_number]
  | In_stack_slot { offset_in_words; } ->
    let offset_in_bytes = Arch.size_addr * offset_in_words in
    Operator.contents_of_stack_slot ~offset_in_bytes
  | Offset_pointer { block; offset_in_words; } ->
    let offset_in_bytes = Int64.of_int (Arch.size_addr * offset_in_words) in
    (compile_to_yield_value block)
      @ [Operator.add_unsigned_const offset_in_bytes]
  | Read_field { block; field; } ->
    (compile_to_yield_value block) @ [
      Operator.add_unsigned_const (Int64.of_int (Arch.size_addr * field));
      Operator.deref ();
    ]

let compile t =
  let sequence = (compile_to_yield_value t) @ [Operator.stack_value ()] in
(*
  Format.eprintf "SLE.compile non-optimized: %a\n"
    (Format.pp_print_list Operator.print) sequence;
*)
  let optimized = Operator.optimize_sequence sequence in
(*
  Format.eprintf "  --> optimized: %a\n%!"
    (Format.pp_print_list Operator.print) optimized;
*)
  optimized

let size t =
  List.fold_left (fun size op -> Int64.add size (Operator.size op)) 0L
    (compile t)

let emit t asm =
  List.iter (fun op -> Operator.emit op asm) (compile t)
