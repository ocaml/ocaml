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

open RawAProf

module Position = struct

  type t = Printexc.location

  let filename { Printexc.filename } = filename

  let line_number { Printexc.line_number } = line_number

  let start_char { Printexc.start_char } = start_char

  let end_char { Printexc.end_char } = end_char

  let compare = Pervasives.compare

  let hash = Hashtbl.hash

end

module Location = struct

  type t =
    { address: Int64.t;
      symbol: string option;
      position : Position.t option;
      foreign : bool; }

  let address { address } = address

  let symbol { symbol } = symbol

  let position { position } = position

  let foreign { foreign } = foreign

  let compare = Pervasives.compare

  let hash = Hashtbl.hash

  let create_ocaml ?executable ~frame_table pc =
    let address = Program_counter.OCaml.to_int64 pc in
    let symbol = None in
    let foreign = false in
    let position =
      try
        let slot = Frame_table.find_exn frame_table pc in
        Printexc.Slot.location slot
      with Not_found -> None
    in
    { address; symbol; position; foreign; }

  let create_foreign ?executable pc =
    { address = Program_counter.Foreign.to_int64 pc;
      symbol = None;
      position = None;
      foreign = true; }

end

module Backtrace = struct

  type t = Location.t list

  let compare = Pervasives.compare

  let hash = Hashtbl.hash

end

module Entry = struct

  type t =
    { backtrace : Backtrace.t;
      blocks : int;
      words : int; }

  let backtrace { backtrace } = backtrace

  let blocks { blocks } = blocks

  let words { words } = words

  let compare = Pervasives.compare

  let hash = Hashtbl.hash

end

module Entries = Set.Make(Entry)

module Stats = struct

  include Gc_stats

  let compare = Pervasives.compare

  let hash = Hashtbl.hash

end

module Snapshot = struct

  type t =
    { time : float;
      stats : Gc_stats.t;
      entries : Entries.t; }

  let time { time } = time

  let stats { stats } = stats

  let entries { entries } = entries

  let rec fold_ocaml_indirect_calls ?executable ~frame_table ~shape_table
            visited f backtrace acc callee =
    let node = Trace.OCaml.Indirect_call_point.Callee.callee_node callee in
    let acc =
      fold_node ?executable ~frame_table ~shape_table visited f backtrace
        acc node
    in
    let next = Trace.OCaml.Indirect_call_point.Callee.next callee in
    match next with
    | None -> acc
    | Some callee ->
      fold_ocaml_indirect_calls ?executable ~frame_table ~shape_table
        visited f backtrace acc callee

  and fold_ocaml_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field =
    let acc =
      match Trace.OCaml.Field.classify field with
      | Trace.OCaml.Field.Allocation alloc ->
        let pc = Trace.OCaml.Allocation_point.program_counter alloc in
        let loc = Location.create_ocaml ?executable ~frame_table pc in
        let annot = Trace.OCaml.Allocation_point.annotation alloc in
        f (loc :: backtrace) annot acc
      | Trace.OCaml.Field.Direct_call (Trace.OCaml.Field.To_ocaml call) ->
        let site = Trace.OCaml.Direct_call_point.call_site call in
        let loc = Location.create_ocaml ?executable ~frame_table site in
        let node = Trace.OCaml.Direct_call_point.callee_node call in
        fold_ocaml_node ?executable ~frame_table ~shape_table
          visited f (loc :: backtrace) acc node
      | Trace.OCaml.Field.Direct_call (Trace.OCaml.Field.To_foreign call) ->
        let site = Trace.OCaml.Direct_call_point.call_site call in
        let loc = Location.create_ocaml ?executable ~frame_table site in
        let node = Trace.OCaml.Direct_call_point.callee_node call in
        fold_foreign_node ?executable ~frame_table ~shape_table
          visited f (loc :: backtrace) acc node
      | Trace.OCaml.Field.Direct_call
          (Trace.OCaml.Field.To_uninstrumented _) ->
        acc
      | Trace.OCaml.Field.Indirect_call call ->
        let site = Trace.OCaml.Indirect_call_point.call_site call in
        let loc = Location.create_ocaml ?executable ~frame_table site in
        let callee = Trace.OCaml.Indirect_call_point.callees call in
        match callee with
        | None -> acc
        | Some callee ->
          fold_ocaml_indirect_calls ?executable ~frame_table ~shape_table
            visited f (loc :: backtrace) acc callee
    in
    match Trace.OCaml.Field.next field with
    | None -> acc
    | Some field ->
      fold_ocaml_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field

  and fold_ocaml_node ?executable ~frame_table ~shape_table visited f
        backtrace acc node =
    if Trace.Node.Set.mem (Trace.Node.of_ocaml_node node) !visited then acc
    else begin
      visited := Trace.Node.Set.add (Trace.Node.of_ocaml_node node) !visited;
      let acc =
         fold_ocaml_node
           ?executable ~frame_table ~shape_table visited f backtrace acc
           (Trace.OCaml.Node.next_in_tail_call_chain node)
      in
      let acc =
        match Trace.OCaml.Node.fields node ~shape_table with
        | None -> acc
        | Some fields ->
          fold_ocaml_fields
            ?executable ~frame_table ~shape_table visited f backtrace acc fields
      in
      acc
    end

  and fold_foreign_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field =
    let acc =
      match Trace.Foreign.Field.classify field with
      | Trace.Foreign.Field.Allocation alloc ->
        let pc = Trace.Foreign.Allocation_point.program_counter alloc in
        let loc = Location.create_foreign ?executable pc in
        let annot = Trace.Foreign.Allocation_point.annotation alloc in
        f (loc :: backtrace) annot acc
      | Trace.Foreign.Field.Call call ->
        let site = Trace.Foreign.Call_point.call_site call in
        let loc = Location.create_foreign ?executable site in
        let node = Trace.Foreign.Call_point.callee_node call in
        fold_node ?executable ~frame_table ~shape_table
          visited f (loc :: backtrace) acc node
    in
    match Trace.Foreign.Field.next field with
    | None -> acc
    | Some field ->
      fold_foreign_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field

  and fold_foreign_node ?executable ~frame_table ~shape_table visited f
        backtrace acc node =
    match Trace.Foreign.Node.fields node with
    | None -> acc
    | Some field ->
      fold_foreign_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field

  and fold_node ?executable ~frame_table ~shape_table visited f backtrace
        acc node =
    match Trace.Node.classify node with
    | Trace.Node.OCaml node ->
      fold_ocaml_node ?executable ~frame_table ~shape_table visited f
        backtrace acc node
    | Trace.Node.Foreign node ->
      fold_foreign_node ?executable ~frame_table ~shape_table visited f
        backtrace acc node

  let allocation_table ~snapshot =
    let entries = Heap_snapshot.entries snapshot in
    let length = Heap_snapshot.Entries.length entries in
    let tbl = Hashtbl.create 42 in
    for entry = 0 to length - 1 do
      let annotation = Heap_snapshot.Entries.annotation entries entry in
      let num_blocks = Heap_snapshot.Entries.num_blocks entries entry in
      let num_words =
        Heap_snapshot.Entries.num_words_including_headers entries entry
      in
      assert (not (Hashtbl.mem tbl annotation));
      Hashtbl.add tbl annotation (num_blocks, num_words)
    done;
    tbl

  let create ?executable ~trace ~frame_table ~shape_table ~snapshot =
    let time = Heap_snapshot.timestamp snapshot in
    let stats = Heap_snapshot.gc_stats snapshot in
    let allocs = allocation_table ~snapshot in
    let entries =
      match Trace.root trace with
      | None -> Entries.empty
      | Some node ->
        let visited = ref Trace.Node.Set.empty in
        fold_node ?executable ~frame_table ~shape_table visited
          (fun backtrace alloc acc ->
             match Hashtbl.find allocs alloc with
             | (blocks, words) ->
               let entry = { Entry.backtrace; blocks; words } in
               Entries.add entry acc
             | exception Not_found -> acc)
          [] Entries.empty node
    in
    { time; stats; entries }

  let compare x y =
    let cmp = Pervasives.compare x.time y.time in
    if cmp <> 0 then cmp
    else begin
      let cmp = Stats.compare x.stats y.stats in
      if cmp <> 0 then cmp
      else Entries.compare x.entries y.entries
    end

  let hash = Hashtbl.hash

end

module Series = struct

  type t = Snapshot.t list

  let create ?executable profile =
    let series = Heap_snapshot.Series.read ~pathname_prefix:profile in
    let trace =
      (* CR mshinwell: fix thread handling! *)
      match
        Heap_snapshot.Series.trace series
          ~kind:Heap_snapshot.Series.Normal
          ~thread_index:0
      with
      | None -> failwith "No threads"
      | Some trace -> trace
    in
    let frame_table = Heap_snapshot.Series.frame_table series in
    let shape_table = Heap_snapshot.Series.shape_table series in
    let length = Heap_snapshot.Series.num_snapshots series in
    let snapshots =
      let rec loop acc n =
        if n < 0 then List.rev acc
        else begin
          let snapshot = Heap_snapshot.Series.snapshot series ~index:n in
          loop (snapshot :: acc) (n - 1)
        end
      in
      loop [] (length - 1)
    in
      List.map
        (fun snapshot ->
           Snapshot.create ?executable ~trace ~frame_table ~shape_table
             ~snapshot)
        snapshots

end
