(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Group, LLC                       *)
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

(* CR mshinwell: move to [Gc] module *)
module Frame_table = struct
  external num_frame_descriptors : unit -> int
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_num_frame_descriptors" "noalloc"

  external get_frame_descriptor : int -> Printexc.raw_backtrace_slot option
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_get_frame_descriptor"

  external return_address_of_frame_descriptor
     : Printexc.raw_backtrace_slot
    -> Int64.t
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_return_address_of_frame_descriptor"

  let get () =
    let num = num_frame_descriptors () in
    let table = Hashtbl.create num in
    for index = 0 to num - 1 do
      match get_frame_descriptor index with
      | None -> ()
      | Some descr ->
        let return_addr = return_address_of_frame_descriptor descr in
        assert (not (Hashtbl.mem table return_addr));
        Hashtbl.add table return_addr
          (Printexc.convert_raw_backtrace_slot descr)
    done;
    table
end

module Shape_table = struct
  type part_of_shape =
    (* Must match asmrun/spacetime_snapshot.c *)
    (* CR mshinwell: must also match rawAProf.ml, share this? *)
    (* The [Int64.t] arguments give the call/allocation site. *)
    | Direct_call of Int64.t
    | Indirect_call of Int64.t
    | Allocation_point of Int64.t

  let _ = Direct_call 0L
  let _ = Indirect_call 0L
  let _ = Allocation_point 0L

  (* This associative list is indexed by the addresses of the start of
     functions.
     The [part_of_shape list] is in order (start of the node first).
     The whole structure is allocated outside of the OCaml heap. *)
  type t = (Int64.t * (part_of_shape list)) list

  external get : unit -> t
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_shape_table" "noalloc"

  external marshal : t -> out_channel -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_marshal_shape_table"

  (* CR-soon mshinwell: add support for freeing the structure *)
end

module Heap_snapshot = struct
  let pathname_suffix_trace = "trace"

  module Writer = struct
    type t = {
      pathname_prefix : string;
      mutable next_index : int;
      mutable closed : bool;
    }

    let create ~pathname_prefix =
      { pathname_prefix = pathname_prefix ^ ".";
        next_index = 0;
        closed = false;
      }

    let use t ~f =
      if t.closed then failwith "Heap_snapshot.Writer: is closed";
      let pathname = t.pathname_prefix ^ (string_of_int t.next_index) in
      t.next_index <- t.next_index + 1;
      let chn = open_out pathname in
      f chn;
      close_out chn

    external marshal_global_trace : out_channel -> unit
      = "caml_spacetime_only_works_for_native_code"
        "caml_spacetime_marshal_trie"

    let save_trace_and_close t =
      let chn = open_out (t.pathname_prefix ^ pathname_suffix_trace) in
      Marshal.to_channel chn t.next_index [];
      Marshal.to_channel chn (Sys.time ()) [];
      Marshal.to_channel chn (Frame_table.get ()) [];
      Shape_table.marshal (Shape_table.get ()) chn;
      marshal_global_trace chn;
      close_out chn;
      t.closed <- true
  end

  type raw_snapshot

  external take : unit -> raw_snapshot
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_take_heap_snapshot"

  external marshal : out_channel -> raw_snapshot -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_marshal_heap_snapshot"

  external free : raw_snapshot -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_free_heap_snapshot" "noalloc"

  let take writer =
    Writer.use writer ~f:(fun out_channel ->
      let snapshot = take () in
      marshal out_channel snapshot;
      free snapshot)
end
