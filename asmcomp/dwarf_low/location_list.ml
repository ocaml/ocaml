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

type t = {
  name : string;
  entries : Location_list_entry.t list;
}

let next_id = ref 0

let create entries =
  let id = !next_id in
  next_id := !next_id + 1;
  { name = sprintf "location_list%d" id;
    entries;
  }

let label t = t.name

let size t =
  let body_size =
    List.fold t.entries
      ~init:0
      ~f:(fun size entry -> size + Location_list_entry.size entry)
  in
  body_size + 8 + 8

let emit t ~emitter =
  Emitter.emit_label_declaration emitter ~label_name:t.name;
  List.iter t.entries ~f:(Location_list_entry.emit ~emitter);
  Value.emit (Value.as_code_address Int64.zero) ~emitter;
  Value.emit (Value.as_code_address Int64.zero) ~emitter
