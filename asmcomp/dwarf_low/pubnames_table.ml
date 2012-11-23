(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Mark Shinwell, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright and licence information to be added.                     *)
(*                                                                     *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

open Std_internal

(* http://llvm.org/docs/SourceLevelDebugging.html claims this table
   is a waste of space.  Maybe we don't need to emit it. *)

type t = {
  externally_visible_functions : string list;
  debug_info : Debug_info_section.t;
}

let create ~externally_visible_functions ~debug_info =
  { externally_visible_functions; debug_info; }

let size t =
  let size_of_entry function_name =
    4 + (String.length function_name) + 1
  in
  let size_of_all_entries =
    List.fold t.externally_visible_functions
      ~init:0
      ~f:(fun size entry -> size + size_of_entry entry)
  in
  2 + 4 + 4 + size_of_all_entries + 4

let emit t ~emitter =
  let write_offset_name_pair function_name =
    (* CR mshinwell: should use [Value.emit], no? *)
    Emitter.emit_string emitter "\t.long\tLdie__";
    Emitter.emit_symbol emitter function_name;
    Emitter.emit_string emitter "-Ldie__compile_unit\n";
    Value.emit (Value.as_string function_name) ~emitter
  in
  Value.emit (Value.as_four_byte_int (size t)) ~emitter;
  Value.emit (Value.as_two_byte_int 2) ~emitter;  (* version number *)
  Value.emit (Value.as_four_byte_int_from_label "Ldebug_info0") ~emitter;
  Value.emit
    (Value.as_four_byte_int (Debug_info_section.size t.debug_info)) ~emitter;
  List.iter t.externally_visible_functions ~f:write_offset_name_pair;
  Value.emit (Value.as_four_byte_int 0) ~emitter
