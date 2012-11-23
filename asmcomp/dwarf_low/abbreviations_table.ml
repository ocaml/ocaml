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

type t = Abbreviations_table_entry.t list

let create abbrev_table_entries =
  abbrev_table_entries

let size t =
  List.fold t
    ~init:0
    ~f:(fun size entry -> size + Abbreviations_table_entry.size entry)
    + Value.size (Value.as_uleb128 0)

let emit t ~emitter =
  List.iter t ~f:(Abbreviations_table_entry.emit ~emitter);
  Value.emit (Value.as_uleb128 0) ~emitter
