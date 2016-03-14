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

module Annotation = struct
  type t = int

  let to_int t = t
end

module Trace = struct
  external debug : unit -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_debug" "noalloc"
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

(* XCR mshinwell: must ensure -allocation-profiling does not appear as a
   compiler option if WITH_SPACETIME was not set *)

external annotation_of_value : 'a -> int
  = "caml_spacetime_only_works_for_native_code"
    "caml_spacetime_get_profinfo"

external erase_profiling_annotations : unit -> unit
  = "caml_spacetime_only_works_for_native_code"
    "caml_forget_where_values_were_allocated"

external annotate_values_with_allocation_location : unit -> unit
  = "%identity" (*"caml_do_not_override_profinfo" "noalloc"*)

external annotate_values_with_given_integer : int -> bool
= "%identity"
(*
    = "caml_set_override_profinfo" "noalloc"
*)

let annotate_values_with_given_integer i =
  if i < 0 then `Out_of_range
  else if annotate_values_with_given_integer i then `Ok
  else `Out_of_range

(* CR mshinwell: make this all work *)

(*
external max_annotation_value : unit -> int
= "%identity"
(*
  = "caml_spacetime_max_override_profinfo" "noalloc"
*)
*)

(*
type profinfo = int
external profinfo_none : unit -> profinfo
  = "caml_spacetime_only_works_for_native_code"
    "caml_spacetime_profinfo_none" "noalloc"

external profinfo_overflow : unit -> profinfo
  = "caml_spacetime_only_works_for_native_code"
    "caml_spacetime_profinfo_none" "noalloc"

external get_profinfo : 'a -> profinfo
  = "caml_spacetime_only_works_for_native_code"
    "caml_spacetime_get_profinfo" "noalloc"
*)
