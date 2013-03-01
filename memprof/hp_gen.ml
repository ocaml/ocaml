open Typeopt
open HPTypes
open HPGlobals

(* (\* TODO: *)

(* Some simple computations: *)
(* * Repartition of blocks per size *)
(* * Repartition of blocks per tag *)
(* * Repartition of blocks per path *)
(* * Repartition of memory per root *)

(* *\) *)


(* (\*************************************************************************\) *)
(* (\*                                                                       *\) *)
(* (\*                         compute_memory_per_module                     *\) *)
(* (\*                                                                       *\) *)
(* (\*************************************************************************\) *)

let compute_memory_per_module hp =
  let h = hp.hp_info in
  let modules = ref [] in
  for i = 0 to Array.length h.caml_globals -1 do
    let name = h.globals_map.(i) in
    let pointer = h.caml_globals.(i) in
    let mem = Scan_heap.scan hp pointer in
    modules := (mem, name) :: !modules
  done;

  let modules = List.sort (fun (m1,_) (m2,_) -> compare m2 m1) !modules in


  print_newline ();
  Printf.printf "----------------------------------";
  print_newline ();

  Printf.printf "Modules: %d modules" (List.length modules); print_newline ();
  List.iter (fun (mem, name) ->
        Printf.printf "%7d %s\n" mem name;
  ) modules;
  modules

(* (\*************************************************************************\) *)
(* (\*                                                                       *\) *)
(* (\*                         compute_memory_per_root                       *\) *)
(* (\*                                                                       *\) *)
(* (\*************************************************************************\) *)

let compute_memory_per_root hp =

  let roots = ref [] in


  print_newline ();
  Printf.printf "----------------------------------";
  print_newline ();
  let h = hp.hp_info in
  for i = 0 to Array.length h.caml_globals -1 do
    let name = h.globals_map.(i) in
    let pointer = h.caml_globals.(i) in
    let b = hp.hp_blocks.(pointer) in
    let info = h.mem_repr.(i).global_names in
    Printf.printf "%-20s : \n" name;
    for j = 0 to (min (Array.length info) (Array.length b.block_content))
      - 1 do
      let mem = Scan_heap.scan hp b.block_content.(j) in
      let root = info.(j) in
      Printf.printf "  %-40s %d\n" root mem;
      if mem > 0 && root <> "-" then
        roots := (mem, Printf.sprintf "%s.%s" name root) :: !roots
    done;
    print_newline ();
  done;


  let roots = List.sort (fun (m1,_) (m2,_) -> compare m2 m1) !roots in

  print_newline ();
  Printf.printf "----------------------------------";
  print_newline ();

  Printf.printf "Roots:"; print_newline ();
  List.iter (fun (mem, name) ->
        Printf.printf "%7d %s\n" mem name;
  ) roots;
  roots



(* (\*************************************************************************\) *)
(* (\*                                                                       *\) *)
(* (\*                         a                                             *\) *)
(* (\*                                                                       *\) *)
(* (\*************************************************************************\) *)

let close_graph hp =

  Printf.printf "Closing graph..."; print_newline ();

  for p1 = 2 to Array.length hp.hp_blocks - 1 do

    let b1 = hp.hp_blocks.(p1) in
    for i = 0 to Array.length b1.block_content - 1 do
      let p2 = b1.block_content.(i) in
      if p2 > 1 then
        let b2 = hp.hp_blocks.(p2) in
        b2.block_reverse <- p1 :: b2.block_reverse
    done

  done;

  Printf.printf "Graph closed."; print_newline ();
  ()

(* (\*************************************************************************\) *)
(* (\*                                                                       *\) *)
(* (\*                         parse_repr                                    *\) *)
(* (\*                                                                       *\) *)
(* (\*************************************************************************\) *)

let dummy_block =  {
    repr_tag = None;
    repr_size = None;
    repr_content = None;
    repr_labels = None;
  }

let parse_repr hp =
  let h = hp.hp_info in
  let paths = Hashtbl.create 111 in

  Array.iter (fun m ->
      Hashtbl.iter (fun path rr ->
          try
            let rr' = Hashtbl.find paths rr.repr_path in
            if rr'.repr_level < rr.repr_level then begin
                rr'.repr_level <- rr.repr_level;
                rr'.repr_repr <- rr.repr_repr
              end
          with Not_found ->
              Hashtbl.add paths rr.repr_path rr;
              rr.repr_repr <- rr.repr_repr;
      ) m.representations
  ) h.mem_repr;

  (*
  Hashtbl.iter (fun path rr ->
      print_representation paths rr
  ) paths;
*)

  let objects = Hashtbl.create 111 in
  let equiv = ref [] in
  let reprs = Hashtbl.create 111 in

  List.iter (fun (tag, name) ->

      let repr =
        Repr_block { dummy_block with repr_tag = Some tag } in
      let rec r = {
          repr_path = name (*Path.Pident (Ident.create name) *);
          repr_repr = repr;
          repr_level = 6;
        } in

      Hashtbl.add objects (tag, None) (ref [r]);
      Hashtbl.add paths r.repr_path r;
      Hashtbl.add reprs r.repr_repr r.repr_path
  ) [
    Obj.closure_tag, "closure";
    Obj.double_array_tag, "double_array";
    Obj.custom_tag, "custom";
    Obj.double_tag, "float";
    Obj.abstract_tag, "abstract";
    Obj.lazy_tag, "lazy";
    Obj.object_tag, "object";
  ];

  let rec insert_path path r =
    match r with
      Repr_block b ->
        begin
          match b.repr_tag with
            None -> ()
          | Some tag when tag <> Obj.string_tag &&
            (tag = 0 || tag >= Obj.no_scan_tag) -> ()
          | Some tag ->
              let key = (tag, b.repr_size) in
              try
                let list = Hashtbl.find objects key in
(*
                Printf.printf "Adding %s with tag %d"
                  (Path.name path.repr_path) tag; print_newline (); *)
                list := path :: !list
              with Not_found ->
                  (*
                  Printf.printf "Insert %s with tag %d"
                  (Path.name path.repr_path) tag; print_newline (); *)
                  Hashtbl.add objects key (ref [path])
        end
    | Repr_path (args, rr) -> ()
    | Repr_integer -> ()
    | Repr_choice list ->
        List.iter (fun (name, r) -> insert_path path r) list
    | Repr_unknown -> ()
    | Repr_variable i -> ()
  in

  Hashtbl.iter (fun path rr ->
      try
        let path = Hashtbl.find reprs rr.repr_repr in
        equiv := (path, rr.repr_path) :: !equiv
      with _ ->
          Hashtbl.add reprs rr.repr_repr rr.repr_path;
(*          if Path.name rr.repr_path = "CommonTypes.gui_result_handler" then begin
              Printf.printf "Re-adding closure !!"; print_newline ();
              print_representation paths rr;
              print_newline ();
            end; *)
          insert_path rr rr.repr_repr
  ) paths;

  if arg_verbose_types () then begin

      List.iter (fun (p1,p2) ->
          Printf.printf "Equivalent types: %s and %s"
            ( (*Path.name*) p1) ( (*Path.name*) p2); print_newline ();
      ) !equiv;

      Hashtbl.iter (fun (tag, size) list ->
          Printf.printf "tag %d " tag;
          (match size with
              Some size -> Printf.printf "size %d " size
            | _ -> Printf.printf "size unknown ");
          Printf.printf "    %d objects " (List.length !list);
          List.iter (fun r ->
              Printf.printf "%s " ( (*Path.name*) r.repr_path);
          ) !list;
          print_newline ();
      ) objects;
    end;

  paths, objects

(* (\*************************************************************************\) *)
(* (\*                                                                       *\) *)
(* (\*                         subst                                         *\) *)
(* (\*                                                                       *\) *)
(* (\*************************************************************************\) *)

let rec subst args r =
  match r with
    Repr_unknown
  | Repr_integer -> r
  | Repr_variable i -> args.(i-1)
  | Repr_choice list ->
      Repr_choice (List.map (fun (name,r) -> name, subst args r) list)
  | Repr_path (nargs, path) ->
      Repr_path (List.map (subst args) nargs, path)
  | Repr_block b ->
      let content = match b.repr_content with
          None -> None
        | Some list -> Some (List.map (subst args) list)
      in
      Repr_block { b with repr_content = content }

(* (\*************************************************************************\) *)
(* (\*                                                                       *\) *)
(* (\*                         propagate_repr                                *\) *)
(* (\*                                                                       *\) *)
(* (\*************************************************************************\) *)

let rec propagate_repr types blocks continue r b =
  match r with
    Repr_unknown | Repr_integer | Repr_variable _ -> ()
  | Repr_block bb ->

      let continue = match b.block_type with
          None ->
            b.block_type <- Some r;
            Printf.printf "Setting:";
            print_repr types "" 5 "  " r; print_newline ();
            3
        | _ -> continue
      in
      if continue > 0 then
        begin
          match bb.repr_tag with
            None -> ()
          | Some tag ->
              if tag = b.block_tag then
                match bb.repr_content with
                  None -> ()
                | Some list ->
                    let array = Array.of_list list in
                    for i = 0 to Array.length array - 1 do
                      let p = b.block_content.(i) in
                      if p > 1 then
                        let b = blocks.(p) in
                        propagate_repr types blocks (continue-1) array.(i) b
                    done
              else assert false
        end

  | Repr_path (args, path) ->
      let continue = match b.block_type with
          None ->
            b.block_type <- Some r;
            Printf.printf "Setting:";
            print_repr types "" 5 "  " r; print_newline ();
            3
        | Some rr when r = rr -> continue

        | Some (Repr_path ([],path')) when
          path = path' && List.length args > 0 ->
            Printf.printf "Better args"; print_newline ();
            b.block_type <- Some r;
            5

        | Some rr ->
            Printf.printf "different"; print_newline ();
            print_repr types "" 5 "  " r; print_newline ();
            print_repr types "" 5 "  "  rr; print_newline ();
            print_newline ();
            0
      in
      if continue > 0 then
        begin try
            let r = Hashtbl.find types path in
            let args = Array.of_list args in
            let r = subst args r.repr_repr in
            propagate_repr types blocks (continue-1) r b
          with _ -> ()
        end

  | Repr_choice list ->
      List.iter (fun (_, r) ->
          match r with
            Repr_block bb ->
              begin
                match bb.repr_tag with
                  None -> ()
                | Some tag ->
                    if tag = b.block_tag then
                      propagate_repr types blocks continue r b
              end
          | _ -> ()
      ) list

(* (\*************************************************************************\) *)
(* (\*                                                                       *\) *)
(* (\*                         propagate_types                               *\) *)
(* (\*                                                                       *\) *)
(* (\*************************************************************************\) *)

let propagate_types types hp =
  (* let prop_blocks = ref 0 in *)

  for i = 2 to Array.length hp.hp_blocks - 1 do
    let b = hp.hp_blocks.(i) in
    match b.block_type with
    | Some r ->
        propagate_repr types hp.hp_blocks 3 r b;
    | None -> ()
  done;

  ()

(* (\*************************************************************************\) *)
(* (\*                                                                       *\) *)
(* (\*                         discriminate                                  *\) *)
(* (\*                                                                       *\) *)
(* (\*************************************************************************\) *)

let rec discriminate paths level h p1 r =
  if level = 0 then true else
  if p1 = 1 && r <> Repr_integer then true else
  match r with
    Repr_unknown -> true
  | Repr_integer -> p1 = 0
  | Repr_choice list ->
      List.exists (fun (name,r) ->
          discriminate paths level h p1 r
      ) list
  | Repr_path (args, path) ->
      begin
        try
          let rr = Hashtbl.find paths path in
          (* let r = subst (Array.of_list args) rr.repr_repr in *)
          discriminate paths level h p1 rr.repr_repr
        with _ ->
            if arg_verbose_types2 () then begin
                Printf.printf "Could not find description of %s"
                  ((*Path.name*) path); print_newline ();
              end;
            true
      end
  | Repr_variable i -> true
  | Repr_block b ->
      if p1 = 1 then true else
      let b1 = h.hp_blocks.(p1) in
      (match b.repr_tag with
          Some tag -> tag = b1.block_tag
        | _ -> true) &&
      (match b.repr_size with
          Some size -> size = b1.block_size
        | _ -> true) &&
      (match b.repr_content with
          None -> true
        | Some list ->
            let array = Array.of_list list in
            let len = Array.length array in
            if len  <> Array.length b1.block_content then
              false
            else
            try
              for i = 0 to len - 1 do
                if not (discriminate paths (level-1) h b1.block_content.(i)
                    array.(i)) then raise Exit
              done;
              true
            with _ -> false
      )

(* (\*************************************************************************\) *)
(* (\*                                                                       *\) *)
(* (\*                         type_graph                                    *\) *)
(* (\*                                                                       *\) *)
(* (\*************************************************************************\) *)

let type_graph (types,o) hp =

  if arg_verbose_types2 () then begin
      Printf.printf "Typing graph..."; print_newline ();
    end;

  for p1 = 2 to Array.length hp.hp_blocks - 1 do

    let b1 = hp.hp_blocks.(p1) in
    match b1.block_type with
      Some _ -> ()
    | None ->
(*    Printf.printf "For tag %d" b1.block_tag; print_newline (); *)
        (*if b1.block_tag > 0 && b1.block_tag < Obj.module_tag then *)
          let list1 =
            try !(Hashtbl.find o (b1.block_tag,Some b1.block_size)) with _->[]
          in
          let list2 =
            try ! (Hashtbl.find o (b1.block_tag, None)) with _ -> []
          in
          let list = list1 @ list2 in
          match list with
          [] ->
            if arg_verbose_types2 () then begin
                Printf.printf "Could not find tag=%d size=%d"
                  b1.block_tag b1.block_size; print_newline ();
              end;
          | list ->
              let newlist = match list with
                  [r] -> [r]
                | _ ->
                    List.filter (fun r ->
                        discriminate types 5 hp p1 r.repr_repr) list in
              match newlist with
                [] ->
                  if arg_verbose_types () then begin
                      Printf.printf "After discrimination, could not find tag=%d size=%d"
                        b1.block_tag b1.block_size; print_newline ();

                      begin
                        Array.iteri (fun i p ->
                            Printf.printf " b[%d] = %d " i p;
                            (if p > 1 then
                                let b = hp.hp_blocks.(p) in
                                Printf.printf " tag=%d size=%d"
                                  b.block_tag b.block_size);
                            print_newline ();
                        ) b1.block_content
                      end;

                      List.iter (fun r ->
                          print_representation types r) list;
                    end

              | _ :: _ :: _ ->

                  if arg_verbose_types () then begin
                      Printf.printf "Could not discriminate block tag=%d size=%d over %d possibilities"
                        b1.block_tag b1.block_size (List.length list);
                      print_newline ();

                      if b1.block_size > 6 then
                        begin
                          Array.iteri (fun i p ->
                              Printf.printf " b[%d] = %d " i p;
                              (if p > 1 then
                                  let b = hp.hp_blocks.(p) in
                                  Printf.printf " tag=%d size=%d"
                                    b.block_tag b.block_size);
                              print_newline ();
                          ) b1.block_content;

                          if List.length list < 5 then
                            List.iter (fun r ->
                                print_representation types r) newlist;
                        end;
                    end

              | [r] ->
                  (* let p = r.repr_path in *)
                  b1.block_type <- Some (Repr_path ([], r.repr_path));
  done;

  if arg_verbose_types2 () then begin
      Printf.printf "Graph typed."; print_newline ();
    end;
  ()


(*************************************************************************)
(*                                                                       *)
(*                         count_types                                   *)
(*                                                                       *)
(*************************************************************************)

let count_types hp =

  let paths = Hashtbl.create 111 in
  let block_total = Array.length hp.hp_blocks in
  let block_unknown = ref 0 in
  let size_unknown = ref 0 in
  let size_total = ref 0 in
  for p1 = 2 to block_total - 1 do

    let b1 = hp.hp_blocks.(p1) in
    size_total := !size_total + (b1.block_size + 1);
    match b1.block_type with
      Some (Repr_path (_, p)) ->
        (try
            let block_counter, size_counter = Hashtbl.find paths p in
            incr block_counter;
            size_counter := !size_counter + (b1.block_size + 1)
          with Not_found ->
              Hashtbl.add paths p (ref 1, ref (b1.block_size + 1)))
    | _ ->
        incr block_unknown;
        size_unknown := !size_unknown + b1.block_size + 1
  done;

  let list = ref [!block_unknown, "unknown"] in
  Hashtbl.iter (fun path (counter,_) ->
      list := (!counter, (*Path.name*) path) :: !list
  ) paths;
  let list = List.sort (fun (s1,_) (s2,_) -> compare s2 s1) !list in
  let blocks = list in

  let list = ref [!size_unknown, "unknown"] in
  Hashtbl.iter (fun path (_,counter) ->
      list := (!counter, (*Path.name*) path) :: !list
  ) paths;
  let list = List.sort (fun (s1,_) (s2,_) -> compare s2 s1) !list in
  let sizes = list in

  block_total, !size_total, blocks, sizes

let print_types (block_total, size_total, blocks, sizes) =

  print_newline ();
  Printf.printf "----------------------------------";
  print_newline ();
  Printf.printf "Blocks: total %d" block_total; print_newline ();
  List.iter (fun (size, name) ->
      Printf.printf "%d7 %s" size name; print_newline ();
  ) blocks;

  print_newline ();
  Printf.printf "----------------------------------";
  print_newline ();

  Printf.printf "Size: total %d" size_total; print_newline ();
  List.iter (fun (size, name) ->
      Printf.printf "%d7 %s" size name; print_newline ();
  ) sizes;
  ()

let heaps pid =
  let samples = ref [] in
  let o =
    let name = Printf.sprintf "heap.dump.%d.0" pid in
    let h, _ = HPLoadHeap.read_heap name in
    parse_repr h in

  (try
      for i = 0 to 100000 do
        let name = Printf.sprintf "heap.dump.%d.%d" pid i in
        if Sys.file_exists name then
        let h, _ = HPLoadHeap.read_heap name in
        type_graph o h;
        let r = count_types h in
        samples := (i, r) :: !samples
      done
    with e ->
      Printf.eprintf "Exception %s\n%!" (Printexc.to_string e);
      print_newline ();
  );


(************************************************************)
  let name = Printf.sprintf "blocks_per_type.%d.hp" pid in
  let oc = open_out name in
  Printf.fprintf oc "JOB \"%s\"\n" "types";
  Printf.fprintf oc "DATE \"---\"\n";
  Printf.fprintf oc "SAMPLE_UNIT \"GC\"\n";
  Printf.fprintf oc "VALUE_UNIT \"values\"\n";

  List.iter (fun (n, (block_total, size_total, blocks, sizes)) ->
      Printf.fprintf oc "BEGIN_SAMPLE %d.\n" n;

      List.iter (fun (size, name) ->
          Printf.fprintf oc "  %s %d\n" name size) blocks;

      Printf.fprintf oc "END_SAMPLE %d.\n" n;
  ) (List.rev !samples);
  close_out oc;
  Printf.printf "%s Generated" name; print_newline ();

(************************************************************)
  let name = Printf.sprintf "sizes_per_type.%d.hp" pid in
  let oc = open_out name in
  Printf.fprintf oc "JOB \"%s\"\n" "types";
  Printf.fprintf oc "DATE \"---\"\n";
  Printf.fprintf oc "SAMPLE_UNIT \"GC\"\n";
  Printf.fprintf oc "VALUE_UNIT \"values\"\n";

  List.iter (fun (n, (block_total, size_total, blocks, sizes)) ->
      Printf.fprintf oc "BEGIN_SAMPLE %d.\n" n;

      List.iter (fun (size, name) ->
          Printf.fprintf oc "  %s %d\n" name size) sizes;

      Printf.fprintf oc "END_SAMPLE %d.\n" n;
  ) (List.rev !samples);
  close_out oc;
  Printf.printf "%s Generated" name; print_newline ();

(* JOB "FOO -hC" *)
(* DATE "Thu Dec 26 18:17 2002" *)
(* SAMPLE_UNIT "seconds" *)
(* VALUE_UNIT "bytes" *)
(* BEGIN_SAMPLE 0.00 *)
(* END_SAMPLE 0.00 *)
(* BEGIN_SAMPLE 15.07 *)
(*   ... sample data ... *)
(* END_SAMPLE 15.07 *)
(* BEGIN_SAMPLE 30.23 *)
(*   ... sample data ... *)
(* END_SAMPLE 30.23 *)
(* ... etc. *)
(* BEGIN_SAMPLE 11695.47 *)
(* END_SAMPLE 11695.47 *)
