(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

module Source_location = struct
  type t = {
    filename : string;
    function_name : string;
    line_number : int;
  }

  let create ~filename ~function_name ~line_number =
    { filename; function_name; line_number; }

  let filename t = if t.filename = "??" then None else Some t.filename
  let function_name t = if t.function_name = "??" then None else Some t.function_name
  let line_number t = if t.filename = "??" then None else Some t.line_number

  let to_string t =
    Printf.sprintf "%s:%d (%s)"
      t.filename t.line_number t.function_name
end

module Pc_value : sig
  type t

  val create : Int64.t -> t
  val to_int64 : t -> Int64.t
  val compare : t -> t -> int
  val to_string : t -> string
end = struct
  type t = Int64.t
  let create t = t
  let to_int64 t = t
  let compare = Pervasives.compare
  let to_string t = Printf.sprintf "0x%Lx" t
end

module Source_location_map = struct
  module Map = Map.Make (Pc_value)
  type t = Source_location.t Map.t

  let section_name = ".ocamllocs"

  external byte_offset_of_elf_section_contents : executable:string
    -> section_name:string
    -> int
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_byte_offset_of_source_location_map_elf_section_contents"

  (* CR mshinwell: for Mac OS X, we should store the profiling information
     in the dSYM bundle, I think. *)

  let create_from_elf_section ~executable =
    let offset = byte_offset_of_elf_section_contents ~executable ~section_name in
    if offset >= 0 then begin
      let channel = open_in_bin executable in
      seek_in channel offset;
      let t = ref Map.empty in
      try
        let finished = ref false in
        while not !finished do
          Scanf.fscanf channel "0x%Lx %s %s %d\n"
            (fun instr_pointer function_name filename line_number ->
              if instr_pointer = Int64.zero then
                finished := true
              else
                let source_loc =
                  Source_location.create ~filename ~function_name ~line_number
                in
                t := Map.add (Pc_value.create instr_pointer) source_loc !t)
        done;
        Some !t
      with
      | _exn -> close_in channel; Some !t
    end else
      None

  let create_from_dwarf_then_stuff_into_elf_section_exn ~executable ~run_command =
    let section_contents = Filename.temp_file "ocamlopt" "" in
    run_command ("ocamlmklocs " ^ executable ^ " " ^ section_contents);
    let temp_exe = Filename.temp_file "ocamlopt" "" in
    run_command ("objcopy --add-section=" ^ section_name ^ "=" ^ section_contents
      ^ " " ^ executable ^ " " ^ temp_exe);
    Sys.remove section_contents;
    run_command ("mv -f " ^ temp_exe ^ " " ^ executable)

  let resolve t ~instr_pointer =
    if Map.cardinal t < 1 then
      None
    else
      let pc_value = Pc_value.create instr_pointer in
      try Some (`Exact (Map.find pc_value t))
      with Not_found ->
        (* CR mshinwell: consider improving efficiency *)
        (* CR mshinwell: possible improvement: compare the above and below locations
           by looking e.g. at whether the function name is the same for both, etc. *)
        let below, _exact, above = Map.split pc_value t in
        let nearest_below_loc =
          try Some (snd (Map.max_binding below)) with Not_found -> None
        in
        let nearest_above_loc =
          try Some (snd (Map.min_binding above)) with Not_found -> None
        in
        assert (not (nearest_below_loc = None && nearest_above_loc = None));
        Some (`Between (nearest_below_loc, nearest_above_loc))
end

type t = [
  | `Not_boxed
  | `Unknown
  | `At_address of Int64.t
  | `At_source_location of Source_location.t
  | `Between_source_locations of
    Source_location.t option * Int64.t * Source_location.t option
]

let source_location_map =
  let source_location_map = ref `Not_tried in
  fun () ->
    match !source_location_map with
    | `Ok map -> Some map
    | `Failed | `Loading -> None
    | `Not_tried ->
      (* [`Loading] enables us to provide reasonable thread safety guarantees.
         (The worst case should be that [where_was_value_allocated] returns
         [`At_address] instead of [`At_source_location]). *)
      source_location_map := `Loading;
      match Source_location_map.create_from_elf_section ~executable:Sys.argv.(0) with
      | None -> source_location_map := `Failed; None
      | Some map -> source_location_map := `Ok map; Some map

(* Latet enim veritas, sed nihil pretiosius veritate. *)
external allocation_oracle : 'a -> Int64.t option
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_where_was_this_allocated"

let where_was_value_allocated_address_only v =
  if not (Obj.is_block (Obj.repr v)) then
    `Not_boxed
  else
    match allocation_oracle v with
    | None -> `Unknown
    | Some instr_pointer -> `At_address instr_pointer

let where_was_value_allocated v =
  match where_was_value_allocated_address_only v with
  | `Not_boxed -> `Not_boxed
  | `Unknown -> `Unknown
  | `At_address instr_pointer ->
    match source_location_map () with
    | None -> `At_address instr_pointer
    | Some source_location_map ->
      match Source_location_map.resolve source_location_map ~instr_pointer with
      | None -> `At_address instr_pointer
      | Some (`Exact source_loc) -> `At_source_location source_loc
      | Some (`Between (below_loc, above_loc)) ->
        `Between_source_locations (below_loc, instr_pointer, above_loc)

let to_string = function
  | `Not_boxed -> "value is not boxed"
  | `Unknown -> "point of allocation is unknown"
  | `At_address addr -> Printf.sprintf "0x%Lx" addr
  | `At_source_location src_loc -> Source_location.to_string src_loc
  | `Between_source_locations (None, _addr, None) -> assert false
  | `Between_source_locations (Some below_loc, addr, None) ->
    Printf.sprintf "somewhere after %s (0x%Lx)"
      (Source_location.to_string below_loc)
      addr
  | `Between_source_locations (None, addr, Some above_loc) ->
    Printf.sprintf "somewhere before %s (0x%Lx)"
      (Source_location.to_string above_loc)
      addr
  | `Between_source_locations (Some below_loc, addr, Some above_loc) ->
    Printf.sprintf "%s -> %s (0x%Lx)"
      (Source_location.to_string below_loc)
      (Source_location.to_string above_loc)
      addr

module Heap_snapshot = struct
  external dump_allocators_of_major_heap_blocks : filename:string -> unit
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_dump_allocators_of_major_heap_blocks_from_ocaml"

  external forget_where_values_were_allocated : unit -> unit
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_forget_where_values_were_allocated"

  external dump_heapgraph : node_filename:string
    -> edge_filename:string
    -> unit
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_dump_heapgraph_from_ocaml"
end

module Global = struct
  external dump_allocations_by_address : filename:string -> unit
    = "caml_dump_allocation_profiling_arrays"

  external reset_allocations_by_address : unit -> unit
    = "caml_reset_allocation_profiling_arrays"
end
