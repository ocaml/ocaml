(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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
    | Direct_call of { call_site : Int64.t; callee : Int64.t; }
    | Indirect_call of Int64.t
    | Allocation_point of Int64.t

  let _ = Direct_call { call_site = 0L; callee = 0L; }
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

  let marshal t chan =
    marshal t chan

  (* CR-soon mshinwell: add support for freeing the structure *)
end

module Series = struct
  type t = {
    channel : out_channel;
    mutable closed : bool;
  }

  let create ~path =
    { channel = open_out path;
      closed = false;
    }

  external marshal_global_trace : out_channel -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_marshal_trie"

  let save_and_close t =
    if t.closed then failwith "Series is closed";
    Marshal.to_channel t.channel true [];
    Marshal.to_channel t.channel (Sys.time ()) [];
    Marshal.to_channel t.channel (Frame_table.get ()) [];
    Shape_table.marshal (Shape_table.get ()) t.channel;
    marshal_global_trace t.channel;
    close_out t.channel;
    t.closed <- true
end

module Snapshot = struct

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

  let take { Series.closed; channel } =
    if closed then failwith "Series is closed";
    Marshal.to_channel channel false [];
    let snapshot = take () in
    marshal channel snapshot;
    free snapshot
end
