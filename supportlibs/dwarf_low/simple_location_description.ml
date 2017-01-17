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

  val empty : t
  val const_symbol : Symbol.t -> t
  val const_int : Int64.t -> t
  val const_int_not_ocaml_encoded : Int64.t -> t
  val in_register : reg_number:int -> t
  val in_stack_slot : offset_in_words:int -> t
  val in_register_yielding_stack_value : reg_number:int -> t
  val in_stack_slot_yielding_stack_value : offset_in_words:int -> t
  val read_symbol_field : symbol:Symbol.t -> field:int -> t
  val read_symbol_field_yielding_rvalue : symbol:Symbol.t -> field:int -> t
  val read_field : t -> field:int -> t
  val offset_pointer : t -> offset_in_words:int -> t
  val location_from_another_die
     : die_label:Cmm.label
    -> compilation_unit_header_label:Linearize.label
    -> t
  val implicit_pointer
     : offset_in_bytes:int
    -> die_label:Cmm.label
    -> dwarf_version:Dwarf_version.t
    -> t
end

type description =
  | Empty
  | Const_symbol of Symbol.t
  | Const_int of Int64.t
  | In_register of int
  | In_stack_slot of { offset_in_words : int; }
  | Offset_pointer of { block : description; offset_in_words : int; }
  | Read_field of { block : description; field : int; }
  | Read_symbol_field_yielding_rvalue of { block : description; field : int; }
  | Location_from_another_die of { die_label : Cmm.label;
      compilation_unit_header_label : Cmm.label; }
  | Implicit_pointer of { offset_in_bytes : int; die_label : Cmm.label;
      dwarf_version : Dwarf_version.t; }

(* Iff the boolean is [false] we must add [DW_op_stack_value] to the end
   of the calculation. *)
type t = bool * description

let empty = false, Empty
let const_symbol symbol = false, Const_symbol symbol
let const_int i =
  let i = Int64.logor (Int64.shift_left i 1) 1L in
  false, Const_int i
let const_int_not_ocaml_encoded i = false, Const_int i
let in_register ~reg_number = false, In_register reg_number
let in_stack_slot ~offset_in_words = false, In_stack_slot { offset_in_words; }
let in_register_yielding_stack_value ~reg_number =
  true, In_register reg_number
let in_stack_slot_yielding_stack_value ~offset_in_words =
  true, In_stack_slot { offset_in_words; }
let read_symbol_field ~symbol ~field =
  false, Read_field { block = Const_symbol symbol; field; }
let read_symbol_field_yielding_rvalue ~symbol ~field =
  false,
    Read_symbol_field_yielding_rvalue { block = Const_symbol symbol; field; }
let offset_pointer (_, block) ~offset_in_words =
  false, Offset_pointer { block; offset_in_words; }
let read_field (_, block) ~field =
  false, Read_field { block; field; }
let location_from_another_die ~die_label ~compilation_unit_header_label =
  false, Location_from_another_die { die_label; compilation_unit_header_label; }
let implicit_pointer ~offset_in_bytes ~die_label ~dwarf_version =
  false, Implicit_pointer { offset_in_bytes; die_label; dwarf_version; }

let rec compile_to_yield_value desc =
  (* We first compile the expression to a DWARF expression that always yields
     the *value* of the corresponding variable rather than to a location that
     contains the value (which may sometimes not exist, e.g. in the [Symbol]
     case). *)
  (* CR mshinwell: return a flag saying if we've actually formed an
     lvalue?  (Also tidy this up in the light of recent developments.)
     Look at the read_field case above when doing this. *)
  match desc with
  | Empty -> []
  | Const_symbol symbol -> [Operator.value_of_symbol symbol]
  | Const_int i -> [Operator.signed_int_const i]
  | In_register reg_number -> [Operator.contents_of_register ~reg_number]
  | In_stack_slot { offset_in_words; } ->
    let offset_in_bytes = Arch.size_addr * offset_in_words in
    Operator.contents_of_stack_slot ~offset_in_bytes
  | Read_field { block; field; } ->
    (* We emit special code to catch the case where evaluation of [block]
       fails (for example due to unavailability). *)
    (* CR-soon mshinwell: Factor this code somewhat? *)
    (* CR-soon mshinwell: There should be protection against [block] being
       too long.  This probably doesn't happen with the current OCaml
       emitter since [block] is always a DW_op_call. *)
    (Operator.signed_int_const 0L) ::
      (compile_to_yield_value block) @ [
        Operator.dup ();
      ] @
      [Operator.conditional ~if_zero:[
        ]
        ~if_nonzero:[
          Operator.swap ();
          Operator.drop ();
          Operator.add_unsigned_const (Int64.of_int (Arch.size_addr * field));
          Operator.deref ();
        ]]
  | Offset_pointer { block; offset_in_words; } ->
    (* Similar to [Read_field], above. *)
    let offset_in_bytes = Int64.of_int (Arch.size_addr * offset_in_words) in
    (Operator.signed_int_const 0L) ::
      (compile_to_yield_value block) @ [
        Operator.dup ();
      ] @
      [Operator.conditional ~if_zero:[
        ]
        ~if_nonzero:[
          Operator.swap ();
          Operator.drop ();
          Operator.add_unsigned_const offset_in_bytes;
        ]]
  | Read_symbol_field_yielding_rvalue { block; field; } ->
    (compile_to_yield_value block) @ [
      Operator.add_unsigned_const (Int64.of_int (Arch.size_addr * field));
      Operator.deref_do_not_optimize ();
    ]
  | Location_from_another_die { die_label; compilation_unit_header_label; } ->
    [Operator.call ~die_label ~compilation_unit_header_label]
  | Implicit_pointer { offset_in_bytes; die_label; dwarf_version; } ->
    [Operator.implicit_pointer ~offset_in_bytes ~die_label ~dwarf_version]

let compile (do_not_add_stack_value_op, desc) =
  let sequence =
    let compiled = compile_to_yield_value desc in
    if do_not_add_stack_value_op then
      compiled
    else
      compiled @ [Operator.stack_value ()]
  in
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
