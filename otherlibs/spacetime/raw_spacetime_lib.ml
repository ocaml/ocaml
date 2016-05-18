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

module Gc_stats : sig
  type t

  val minor_words : t -> int
  val promoted_words : t -> int
  val major_words : t -> int
  val minor_collections : t -> int
  val major_collections : t -> int
  val heap_words : t -> int
  val heap_chunks : t -> int
  val compactions : t -> int
  val top_heap_words : t -> int
end = struct
  type t = {
    minor_words : int;
    promoted_words : int;
    major_words : int;
    minor_collections : int;
    major_collections : int;
    heap_words : int;
    heap_chunks : int;
    compactions : int;
    top_heap_words : int;
  }

  let minor_words t = t.minor_words
  let promoted_words t = t.promoted_words
  let major_words t = t.major_words
  let minor_collections t = t.minor_collections
  let major_collections t = t.major_collections
  let heap_words t = t.heap_words
  let heap_chunks t = t.heap_chunks
  let compactions t = t.compactions
  let top_heap_words t = t.top_heap_words
end

module Program_counter = struct
  module OCaml = struct
    type t = Int64.t

    let to_int64 t = t
  end

  module Foreign = struct
    type t = Int64.t

    let to_int64 t = t
  end
end

module Function_identifier = struct
  type t = Int64.t

  let to_int64 t = t
end

module Function_entry_point = struct
  type t = Int64.t

  let to_int64 t = t
end

module Int64_map = Map.Make (Int64)

module Frame_table = struct
  type raw = (Int64.t * Printexc.Slot.t) list

  type t = Printexc.Slot.t Int64_map.t

  let demarshal chn : t =
    let raw : raw = Marshal.from_channel chn in
    List.fold_left (fun map (key, data) -> Int64_map.add key data map)
      Int64_map.empty
      raw

  let find_exn = Int64_map.find
end

module Shape_table = struct
  type part_of_shape =
    | Direct_call of { call_site : Int64.t; callee : Int64.t; }
    | Indirect_call of Int64.t
    | Allocation_point of Int64.t

  let _ = Direct_call { call_site = 0L; callee = 0L; }
  let _ = Indirect_call 0L
  let _ = Allocation_point 0L

  let part_of_shape_size = function
    | Direct_call _
    | Indirect_call _ -> 1
    | Allocation_point _ -> 3

  type raw = (Int64.t * (part_of_shape list)) list

  type t = part_of_shape list Int64_map.t

  let demarshal chn : t =
    let raw : raw = Marshal.from_channel chn in
    List.fold_left (fun map (key, data) -> Int64_map.add key data map)
      Int64_map.empty
      raw

  let find_exn = Int64_map.find
end

module Annotation = struct
  type t = int

  let to_int t = t
end

module Trace = struct
  type node
  type ocaml_node
  type foreign_node
  type uninstrumented_node

  type t = node option

  (* This function unmarshals into malloc blocks, which mean that we
     obtain a straightforward means of writing [compare] on [node]s. *)
  external unmarshal : in_channel -> 'a
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_unmarshal_trie"

  let unmarshal in_channel =
    let trace = unmarshal in_channel in
    if trace = () then
      None
    else
      Some ((Obj.magic trace) : node)

  let node_is_null (node : node) =
    ((Obj.magic node) : unit) == ()

  let foreign_node_is_null (node : foreign_node) =
    ((Obj.magic node) : unit) == ()

  external node_num_header_words : unit -> int
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_node_num_header_words" "noalloc"

  let num_header_words = lazy (node_num_header_words ())

  module OCaml = struct
    type field_iterator = {
      node : ocaml_node;
      offset : int;
      part_of_shape : Shape_table.part_of_shape;
      remaining_layout : Shape_table.part_of_shape list;
      shape_table : Shape_table.t;
    }

    module Allocation_point = struct
      type t = field_iterator

      let program_counter t =
        match t.part_of_shape with
        | Shape_table.Allocation_point call_site -> call_site
        | _ -> assert false

      external annotation : ocaml_node -> int -> Annotation.t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_allocation_point_annotation"
          "noalloc"

      let annotation t = annotation t.node t.offset

      external count : ocaml_node -> int -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_allocation_point_count"
          "noalloc"

      let count t = count t.node t.offset
    end

    module Direct_call_point = struct
      type _ t = field_iterator

      let call_site t =
        match t.part_of_shape with
        | Shape_table.Direct_call { call_site; _ } -> call_site
        | _ -> assert false

      let callee t =
        match t.part_of_shape with
        | Shape_table.Direct_call { callee; _ } -> callee
        | _ -> assert false

      external callee_node : ocaml_node -> int -> 'target
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_direct_call_point_callee_node"

      let callee_node (type target) (t : target t) : target =
        callee_node t.node t.offset
    end

    module Indirect_call_point = struct
      type t = field_iterator

      let call_site t =
        match t.part_of_shape with
        | Shape_table.Indirect_call call_site -> call_site
        | _ -> assert false

      module Callee = struct
        (* CR mshinwell: we should think about the names again.  This is
           a "c_node" but it isn't foreign. *)
        type t = foreign_node

        let is_null = foreign_node_is_null

        (* CR mshinwell: maybe rename ...c_node_call_site -> c_node_pc,
           since it isn't a call site in this case. *)
        external callee : t -> Function_entry_point.t
          = "caml_spacetime_only_works_for_native_code"
            "caml_spacetime_c_node_call_site"

        (* This can return a node satisfying "is_null" in the case of an
           uninitialised tail call point.  See the comment in the C code. *)
        external callee_node : t -> node
          = "caml_spacetime_only_works_for_native_code"
            "caml_spacetime_c_node_callee_node" "noalloc"

        external next : t -> foreign_node
          = "caml_spacetime_only_works_for_native_code"
            "caml_spacetime_c_node_next" "noalloc"

        let next t =
          let next = next t in
          if foreign_node_is_null next then None
          else Some next
      end

      external callees : ocaml_node -> int -> Callee.t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_indirect_call_point_callees"
          "noalloc"

      let callees t =
        let callees = callees t.node t.offset in
        if Callee.is_null callees then None
        else Some callees
    end

    module Field = struct
      type t = field_iterator

      type direct_call_point =
        | To_ocaml of ocaml_node Direct_call_point.t
        | To_foreign of foreign_node Direct_call_point.t
        | To_uninstrumented of
            uninstrumented_node Direct_call_point.t

      type classification =
        | Allocation of Allocation_point.t
        | Direct_call of direct_call_point
        | Indirect_call of Indirect_call_point.t

      external classify_direct_call_point : ocaml_node -> int -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_classify_direct_call_point"
          "noalloc"

      let classify t =
        match t.part_of_shape with
        | Shape_table.Direct_call callee ->
          let direct_call_point =
            match classify_direct_call_point t.node t.offset with
            | 0 ->
              (* We should never classify uninitialised call points here. *)
              assert false
            | 1 -> To_ocaml t
            | 2 -> To_foreign t
            | _ -> assert false
          in
          Direct_call direct_call_point
        | Shape_table.Indirect_call _ -> Indirect_call t
        | Shape_table.Allocation_point _ -> Allocation t

      (* CR-soon mshinwell: change to "is_unused"? *)
      let is_uninitialised t =
        let offset_to_node_hole =
          match t.part_of_shape with
          | Shape_table.Direct_call _ -> Some 0
          | Shape_table.Indirect_call _ -> Some 0
          | Shape_table.Allocation_point _ -> None
        in
        match offset_to_node_hole with
        | None -> false
        | Some offset_to_node_hole ->
          (* There are actually two cases:
             1. A normal unused node hole, which says Val_unit;
             2. An unused tail call point.  This will contain a pointer to the
                start of the current node, but it also has the bottom bit
                set. *)
          let offset = t.offset + offset_to_node_hole in
          Obj.is_int (Obj.field (Obj.repr t.node) offset)

      let rec next t =
        match t.remaining_layout with
        | [] -> None
        | part_of_shape::remaining_layout ->
          let size = Shape_table.part_of_shape_size t.part_of_shape in
          let offset = t.offset + size in
          assert (offset < Obj.size (Obj.repr t.node));
          let t =
            { node = t.node;
              offset;
              part_of_shape;
              remaining_layout;
              shape_table = t.shape_table;
            }
          in
          skip_uninitialised t

      and skip_uninitialised t =
        if not (is_uninitialised t) then Some t
        else next t
    end

    module Node = struct
      type t = ocaml_node

      external function_identifier : t -> Function_identifier.t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_function_identifier"

      external next_in_tail_call_chain : t -> t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_ocaml_tail_chain" "noalloc"

      external compare : t -> t -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_compare_node" "noalloc"

      let fields t ~shape_table =
        match Shape_table.find_exn (function_identifier t) shape_table with
        | exception Not_found -> None
        | [] -> None
        | part_of_shape::remaining_layout ->
          let t =
            { node = t;
              offset = Lazy.force num_header_words;
              part_of_shape;
              remaining_layout;
              shape_table;
            }
          in
          Field.skip_uninitialised t
    end
  end

  module Foreign = struct
    module Node = struct
      type t = foreign_node

      external compare : t -> t -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_compare_node" "noalloc"

      let fields t =
        if foreign_node_is_null t then None
        else Some t
    end

    module Allocation_point = struct
      type t = foreign_node

      external program_counter : t -> Program_counter.Foreign.t
        (* This is not a mistake; the same C function works. *)
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_call_site"

      external annotation : t -> Annotation.t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_profinfo" "noalloc"

      external count : t -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_allocation_count" "noalloc"
    end

    module Call_point = struct
      type t = foreign_node

      external call_site : t -> Program_counter.Foreign.t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_call_site"

      (* May return a null node.  See comment above and the C code. *)
      external callee_node : t -> node
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_callee_node" "noalloc"
    end

    module Field = struct
      type t = foreign_node

      type classification =
        | Allocation of Allocation_point.t
        | Call of Call_point.t

      external is_call : t -> bool
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_is_call" "noalloc"

      let classify t =
        if is_call t then Call t
        else Allocation t

      external next : t -> t
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_c_node_next" "noalloc"

      let next t =
        let next = next t in
        if foreign_node_is_null next then None
        else Some next
    end
  end

  module Node = struct
    module T = struct
      type t = node

      external compare : t -> t -> int
        = "caml_spacetime_only_works_for_native_code"
          "caml_spacetime_compare_node" "noalloc"
    end

    include T

    type classification =
      | OCaml of OCaml.Node.t
      | Foreign of Foreign.Node.t

    (* CR lwhite: These functions should work in bytecode *)
    external is_ocaml_node : t -> bool
      = "caml_spacetime_only_works_for_native_code"
        "caml_spacetime_is_ocaml_node" "noalloc"

    let classify t =
      if is_ocaml_node t then OCaml ((Obj.magic t) : ocaml_node)
      else Foreign ((Obj.magic t) : foreign_node)

    let of_ocaml_node (node : ocaml_node) : t = Obj.magic node
    let of_foreign_node (node : foreign_node) : t = Obj.magic node

    module Map = Map.Make (T)
    module Set = Set.Make (T)
  end

  let root t = t

  let debug_ocaml t ~shape_table ~resolve_return_address =
    let next_id = ref 0 in
    let visited = ref Node.Map.empty in
    let print_backtrace backtrace =
      String.concat "->" (List.map (fun return_address ->
          match resolve_return_address return_address with
          | None -> Printf.sprintf "0x%Lx" return_address
          | Some loc -> loc)
        backtrace)
    in
    let rec print_node node ~backtrace =
      match Node.Map.find node !visited with
      | id -> Printf.printf "Node %d visited before.\n%!" id
      | exception Not_found ->
        let id = !next_id in
        incr next_id;
        visited := Node.Map.add node id !visited;
        match Node.classify node with
        | Node.OCaml node ->
          Printf.printf "Node %d (OCaml node):\n%!" id;
          let module O = OCaml.Node in
          let fun_id = O.function_identifier node in
          Printf.printf "Function identifier for node: %Lx\n%!"
            (Function_identifier.to_int64 fun_id);
          Printf.printf "Tail chain for node:\n%!";
          let rec print_tail_chain node' =
            if Node.compare (Node.of_ocaml_node node)
                (Node.of_ocaml_node node') = 0
            then ()
            else begin
              let id =
                match Node.Map.find (Node.of_ocaml_node node') !visited with
                | id -> id
                | exception Not_found ->
                  let id = !next_id in
                  incr next_id;
                  (* CR mshinwell: any non-visted ones will never be
                     printed now *)
                  visited :=
                    Node.Map.add (Node.of_ocaml_node node') id !visited;
                  id
              in
              Printf.printf "  Node %d\n%!" id;
              print_tail_chain (O.next_in_tail_call_chain node')
            end
          in
          print_tail_chain (O.next_in_tail_call_chain node);
          let rec iter_fields index = function
            | None -> ()
            | Some field ->
              Printf.printf "Node %d field %d:\n%!" id index;
              let module F = OCaml.Field in
              begin match F.classify field with
              | F.Allocation alloc ->
                let pc = OCaml.Allocation_point.program_counter alloc in
                let annot = OCaml.Allocation_point.annotation alloc in
                Printf.printf "Allocation point, pc=%Lx annot=%d \
                    backtrace=%s\n%!"
                  (Program_counter.OCaml.to_int64 pc)
                  (Annotation.to_int annot)
                  (print_backtrace (List.rev backtrace))
              | F.Direct_call (F.To_ocaml direct) ->
                let module D = OCaml.Direct_call_point in
                let call_site = D.call_site direct in
                let callee = D.callee direct in
                let callee_node = D.callee_node direct in
                Printf.printf "Direct OCaml -> OCaml call point, pc=%Lx, \
                    callee=%Lx.  Callee node is:\n%!"
                  (Program_counter.OCaml.to_int64 call_site)
                  (Function_entry_point.to_int64 callee);
                print_node (Node.of_ocaml_node callee_node)
                  ~backtrace:(call_site::backtrace);
                Printf.printf "End of call point\n%!"
              | F.Direct_call (F.To_foreign direct) ->
                let module D = OCaml.Direct_call_point in
                let call_site = D.call_site direct in
                let callee = D.callee direct in
                let callee_node = D.callee_node direct in
                Printf.printf "Direct OCaml -> C call point, pc=%Lx, \
                    callee=%Lx.  Callee node is:\n%!"
                  (Program_counter.OCaml.to_int64 call_site)
                  (Function_entry_point.to_int64 callee);
                print_node (Node.of_foreign_node callee_node)
                  ~backtrace:(call_site::backtrace);
                Printf.printf "End of call point\n%!"
              | F.Direct_call (F.To_uninstrumented direct) ->
                let module D = OCaml.Direct_call_point in
                let call_site = D.call_site direct in
                let callee = D.callee direct in
                Printf.printf "Direct OCaml -> uninstrumented call point, \
                    pc=%Lx, callee=%Lx.\n%!"
                  (Program_counter.OCaml.to_int64 call_site)
                  (Function_entry_point.to_int64 callee)
              | F.Indirect_call indirect ->
                let module I = OCaml.Indirect_call_point in
                let call_site = I.call_site indirect in
                Printf.printf "Indirect call point in OCaml code, pc=%Lx:\n%!"
                  (Program_counter.OCaml.to_int64 call_site);
                let callees = I.callees indirect in
                let rec iter_callees index = function
                  | None ->
                    Printf.printf "End of callees for indirect call point.\n%!"
                  | Some callee_iterator ->
                    let module C = I.Callee in
                    let callee = C.callee callee_iterator in
                    let callee_node = C.callee_node callee_iterator in
                    if node_is_null callee_node then begin
                      Printf.printf "... uninitialised tail call point\n%!"
                    end else begin
                      Printf.printf "... callee=%Lx.  \
                          Callee node is:\n%!"
                        (Function_entry_point.to_int64 callee);
                      print_node callee_node ~backtrace:(call_site::backtrace)
                    end;
                    iter_callees (index + 1) (C.next callee_iterator)
                in
                iter_callees 0 callees
              end;
              iter_fields (index + 1) (F.next field)
          in
          iter_fields 0 (O.fields node ~shape_table);
          Printf.printf "End of node %d.\n%!" id
        | Node.Foreign node ->
          Printf.printf "Node %d (C node):\n%!" id;
          let rec iter_fields index = function
            | None -> ()
            | Some field ->
              Printf.printf "Node %d field %d:\n%!" id index;
              let module F = Foreign.Field in
              begin match F.classify field with
              | F.Allocation alloc ->
                let pc = Foreign.Allocation_point.program_counter alloc in
                let annot = Foreign.Allocation_point.annotation alloc in
                Printf.printf "Allocation point, pc=%Lx annot=%d, \
                    backtrace=%s\n%!"
                  (Program_counter.Foreign.to_int64 pc)
                  (Annotation.to_int annot)
                  (print_backtrace (List.rev backtrace))
              | F.Call call ->
                let call_site = Foreign.Call_point.call_site call in
                let callee_node = Foreign.Call_point.callee_node call in
                if node_is_null callee_node then begin
                  Printf.printf "... uninitialised tail call point\n%!"
                end else begin
                  Printf.printf "Call point, pc=%Lx.  Callee node is:\n%!"
                    (Program_counter.Foreign.to_int64 call_site);
                  print_node callee_node ~backtrace:(call_site::backtrace)
                end;
                Printf.printf "End of call point\n%!"
              end;
              iter_fields (index + 1) (F.next field)
          in
          iter_fields 0 (Foreign.Node.fields node);
          Printf.printf "End of node %d.\n%!" id
    in
    match root t with
    | None -> Printf.printf "Trace is empty.\n%!"
    | Some node -> print_node node ~backtrace:[]

  let to_json t channel ~shape_table
      ~(resolve_address : ?long:unit -> Program_counter.OCaml.t -> string) =
    output_string channel "{\n";
    output_string channel "\"nodes\":[\n";
    let seen_a_node = ref false in
    let next_id = ref 0 in
    let visited = ref Node.Map.empty in
    let allocation_nodes = true in
    let rec print_node node =
      match Node.Map.find node !visited with
      | id -> ()
      | exception Not_found ->
        let id = !next_id in
        incr next_id;
        visited := Node.Map.add node id !visited;
        if !seen_a_node then begin
          (* Trailing commas are not allowed. *)
          Printf.fprintf channel ",\n"
        end;
        let first_node = (!seen_a_node = false) in
        seen_a_node := true;
        match Node.classify node with
        | Node.OCaml node ->
          let module O = OCaml.Node in
          let fun_id = O.function_identifier node in
          Printf.fprintf channel "{\"name\":\"%Lx\",\"colour\":%d}%!"
            (Function_identifier.to_int64 fun_id)
            (if first_node then 17 else 2);
          let rec iter_fields = function
            | None -> ()
            | Some field ->
              let module F = OCaml.Field in
              begin match F.classify field with
              | F.Allocation alloc ->
                if allocation_nodes then begin
                  let pc = OCaml.Allocation_point.program_counter alloc in
                  Printf.fprintf channel ",\n{\"name\":\"%s\",\"colour\":3}%!"
                    (resolve_address ~long:() pc);
                  incr next_id
                end
              | F.Direct_call (F.To_ocaml direct) ->
                let module D = OCaml.Direct_call_point in
                let callee_node = D.callee_node direct in
                print_node (Node.of_ocaml_node callee_node)
              | F.Direct_call (F.To_foreign direct) ->
                let module D = OCaml.Direct_call_point in
                let callee_node = D.callee_node direct in
                print_node (Node.of_foreign_node callee_node)
              | F.Direct_call (F.To_uninstrumented _direct) -> ()
              | F.Indirect_call indirect ->
                let module I = OCaml.Indirect_call_point in
                let callees = I.callees indirect in
                let rec iter_callees = function
                  | None -> ()
                  | Some callee_iterator ->
                    let module C = I.Callee in
                    let callee_node = C.callee_node callee_iterator in
                    if not (node_is_null callee_node) then begin
                      print_node callee_node
                    end;
                    iter_callees (C.next callee_iterator)
                in
                iter_callees callees
              end;
              iter_fields (F.next field)
          in
          iter_fields (O.fields node ~shape_table)
        | Node.Foreign node ->
          let name =
            (* CR mshinwell: instead of doing this we should find out the
               address of the top of the function and use that. *)
            let rec iter_fields name = function
              | None -> name
              | Some field ->
                let module F = Foreign.Field in
                let name =
                  match F.classify field with
                  | F.Allocation _alloc -> name
                  | F.Call call ->
                    let call_site = Foreign.Call_point.call_site call in
                    Printf.sprintf "%s %Lx"
                      name
                      (Program_counter.Foreign.to_int64 call_site)
                in
                iter_fields name (F.next field)
            in
            iter_fields "C, calls: " (Foreign.Node.fields node)
          in
          Printf.fprintf channel "{\"name\":\"%s\",\"colour\":0}%!" name;
          let rec iter_fields = function
            | None -> ()
            | Some field ->
              let module F = Foreign.Field in
              begin match F.classify field with
              | F.Allocation _alloc ->
                if allocation_nodes then begin
                  Printf.fprintf channel ",\n{\"name\":\"C\",\"colour\":3}%!";
                  incr next_id
                end
              | F.Call call ->
                let callee_node = Foreign.Call_point.callee_node call in
                if not (node_is_null callee_node) then begin
                  print_node callee_node
                end
              end;
              iter_fields (F.next field)
          in
          iter_fields (Foreign.Node.fields node)
    in
    begin match root t with
    | None -> ()
    | Some node -> print_node node
    end;
    seen_a_node := false;
    next_id := 0;
    let link_id = ref 0 in
    visited := Node.Map.empty;
    output_string channel "],\n";
    output_string channel "\"links\":[\n";
    let rec print_node ?come_from node =
      let check_come_from ~id ~comma =
        begin match come_from with
        | None -> ()
        | Some (come_from, colour, label) ->
          if comma && !seen_a_node then begin
            Printf.fprintf channel ",\n"
          end;
          Printf.fprintf channel
            "{\"source\":%d,\"target\":%d,\"value\":10,\"colour\":%d,\
              \"label\":\"%s\",\"id\":%d}%!"
            come_from id colour label !link_id;
          incr link_id;
          seen_a_node := true
        end
      in
      match Node.Map.find node !visited with
      | id -> check_come_from ~id ~comma:true
      | exception Not_found ->
        let id = !next_id in
        incr next_id;
        visited := Node.Map.add node id !visited;
        if !seen_a_node then begin
          (* Apparently JSON doesn't allow trailing commas. *)
          Printf.fprintf channel ",\n"
        end;
        let c_colour = 16 in
        let direct_colour = 5 in
        let external_colour = 9 in
        let indirect_colour = 14 in
        check_come_from ~id ~comma:false;
        match Node.classify node with
        | Node.OCaml node ->
          let module O = OCaml.Node in
          let rec iter_fields = function
            | None -> ()
            | Some field ->
              let module F = OCaml.Field in
              begin match F.classify field with
              | F.Allocation alloc ->
                if allocation_nodes then begin
                  Printf.fprintf channel
                    ",\n{\"source\":%d,\"target\":%d,\"value\":1,\
                      \"colour\":3,\"id\":%d}%!"
                    id !next_id !link_id;
                  incr link_id;
                  incr next_id
                end
              | F.Direct_call (F.To_ocaml direct) ->
                let module D = OCaml.Direct_call_point in
                let callee_node = D.callee_node direct in
                let call_site = D.call_site direct in
                let label = resolve_address call_site in
                print_node (Node.of_ocaml_node callee_node)
                  ~come_from:(id, direct_colour, label)
              | F.Direct_call (F.To_foreign direct) ->
                let module D = OCaml.Direct_call_point in
                let callee_node = D.callee_node direct in
                let call_site = D.call_site direct in
                let label = resolve_address call_site in
                print_node (Node.of_foreign_node callee_node)
                  ~come_from:(id, external_colour, label)
              | F.Direct_call (F.To_uninstrumented _direct) -> ()
              | F.Indirect_call indirect ->
                let module I = OCaml.Indirect_call_point in
                let callees = I.callees indirect in
                let call_site = I.call_site indirect in
                let label = resolve_address call_site in
                let rec iter_callees = function
                  | None -> ()
                  | Some callee_iterator ->
                    let module C = I.Callee in
                    let callee_node = C.callee_node callee_iterator in
                    if not (node_is_null callee_node) then begin
                      print_node callee_node
                        ~come_from:(id, indirect_colour, label)
                    end;
                    iter_callees (C.next callee_iterator)
                in
                iter_callees callees
              end;
              iter_fields (F.next field)
          in
          iter_fields (O.fields node ~shape_table)
        | Node.Foreign node ->
          let rec iter_fields = function
            | None -> ()
            | Some field ->
              let module F = Foreign.Field in
              begin match F.classify field with
              | F.Allocation _alloc ->
                if allocation_nodes then begin
                  Printf.fprintf channel
                    ",\n{\"source\":%d,\"target\":%d,\"value\":1,\
                      \"colour\":3,\"id\":%d}%!"
                    id !next_id !link_id;
                  incr link_id;
                  incr next_id
                end
              | F.Call call ->
                let callee_node = Foreign.Call_point.callee_node call in
                if not (node_is_null callee_node) then begin
                  print_node callee_node ~come_from:(id, c_colour, "")
                end
              end;
              iter_fields (F.next field)
          in
          iter_fields (Foreign.Node.fields node)
    in
    begin match root t with
    | None -> ()
    | Some node -> print_node node
    end;
    output_string channel "]\n";
    output_string channel "}"
end

module Heap_snapshot = struct

  module Entries = struct
    type t = int array  (* == "struct snapshot_entries" *)

    let length t =
      let length = Array.length t in
      assert (length mod 3 = 0);
      length / 3

    let annotation t idx = t.(idx*3)
    let num_blocks t idx = t.(idx*3 + 1)
    let num_words_including_headers t idx = t.(idx*3 + 2)
  end

  type total_allocations =
    | End
    | Total of {
        annotation : Annotation.t;
        count : int;
        next : total_allocations;
      }

  let (_ : total_allocations) =  (* suppress compiler warning *)
    Total { annotation = 0; count = 0; next = End; }

  type t = {
    timestamp : float;
    gc_stats : Gc_stats.t;
    entries : Entries.t;
    words_scanned : int;
    words_scanned_with_profinfo : int;
    total_allocations : total_allocations;
  }

  type heap_snapshot = t

  let timestamp t = t.timestamp
  let gc_stats t = t.gc_stats
  let entries t = t.entries
  let words_scanned t = t.words_scanned
  let words_scanned_with_profinfo t = t.words_scanned_with_profinfo

  module Total_allocation = struct
    type t = total_allocations  (* [End] is forbidden *)

    let annotation = function
      | End -> assert false
      | Total { annotation; _ } -> annotation

    let count = function
      | End -> assert false
      | Total { count; _ } -> count

    let next = function
      | End -> assert false
      | Total { next = End; _ } -> None
      | Total { next; _ } -> Some next
  end

  let total_allocations t =
    match t.total_allocations with
    | End -> None
    | (Total _) as totals -> Some totals

  module Series = struct
    type t = {
      num_snapshots : int;
      time_of_writer_close : float;
      frame_table : Frame_table.t;
      shape_table : Shape_table.t;
      traces_by_thread : Trace.t array;
      finaliser_traces_by_thread : Trace.t array;
      snapshots : heap_snapshot array;
    }

    let pathname_suffix_trace = "trace"

    let rec read_snapshots chn acc =
      let finished : bool = Marshal.from_channel chn in
      if finished then Array.of_list (List.rev acc)
      else begin
        let snapshot : heap_snapshot = Marshal.from_channel chn in
        read_snapshots chn (snapshot :: acc)
      end

    let read ~path =
      let chn = open_in path in
      let snapshots = read_snapshots chn [] in
      let num_snapshots = Array.length snapshots in
      let time_of_writer_close : float = Marshal.from_channel chn in
      let frame_table = Frame_table.demarshal chn in
      let shape_table = Shape_table.demarshal chn in
      let num_threads : int = Marshal.from_channel chn in
      let traces_by_thread = Array.init num_threads (fun _ -> None) in
      let finaliser_traces_by_thread =
        Array.init num_threads (fun _ -> None)
      in
      for thread = 0 to num_threads - 1 do
        let trace : Trace.t = Trace.unmarshal chn in
        let finaliser_trace : Trace.t = Trace.unmarshal chn in
        traces_by_thread.(thread) <- trace;
        finaliser_traces_by_thread.(thread) <- finaliser_trace
      done;
      close_in chn;
      { num_snapshots;
        time_of_writer_close;
        frame_table;
        shape_table;
        traces_by_thread;
        finaliser_traces_by_thread;
        snapshots;
      }

    type trace_kind = Normal | Finaliser

    let num_threads t = Array.length t.traces_by_thread

    let trace t ~kind ~thread_index =
      if thread_index < 0 || thread_index >= num_threads t then None
      else
        match kind with
        | Normal -> Some t.traces_by_thread.(thread_index)
        | Finaliser -> Some t.finaliser_traces_by_thread.(thread_index)

    let num_snapshots t = t.num_snapshots
    let snapshot t ~index = t.snapshots.(index)
    let frame_table t = t.frame_table
    let shape_table t = t.shape_table
    let time_of_writer_close t = t.time_of_writer_close
  end
end
