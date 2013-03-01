open Typedtree
open Utils

let inheap : ((Location.t * string), int) Hashtbl.t = Hashtbl.create 43
let locty : (Location.t, Types.type_expr) Hashtbl.t = Hashtbl.create 43

let update ty loc =
  try
    let i = Hashtbl.find inheap (loc, ty) in
    Hashtbl.replace inheap (loc, ty) (i + 1)
  with Not_found ->
    Hashtbl.add inheap (loc, ty) 1

let save_type loc ty =  Hashtbl.replace locty loc ty

let find_ty loc =
  try
    Some (Hashtbl.find locty loc)
  with Not_found -> None

let min_cmts = ref []
let prof_files = ref []


let get_type_from_loc all_cmts heap_loc =
  debug "    ==== SIZE %d\n%!" (List.length heap_loc);
  List.iter (fun loc ->

    match find_ty loc with
    | Some ty ->
      debug " ==== (1) ALREADY SEEN THIS TYPE ====\n%!";
      (* save_type loc ty; *)

      Format.pp_set_margin Format.str_formatter 9999;
      Format.fprintf Format.str_formatter "%a" Printtyp.type_expr ty;
      let ty_str = Str.global_replace (Str.regexp " ") "_" (Format.flush_str_formatter ()) in
      update ty_str loc;

      if !d_cmt then begin
        Printtyp.reset ();
        Format.eprintf "\nloc: %a%a\n%!" Location.print loc Printtyp.type_expr ty
      end;
    | None ->
      debug " ==== (2) NEVER SEEN THIS TYPE ====\n%!";
      if loc <> Location.none then begin
        try
          let filename =
            try
              debug "   ==== (2 - 1) GETTING THE CMT FILE %s ====\n%!" loc.Location.loc_start.Lexing.pos_fname;
              List.assoc loc.Location.loc_start.Lexing.pos_fname !min_cmts
            with Not_found ->
              debug " ==== (2 - 2) GETTING THE CMT FILE %s ====\n%!" loc.Location.loc_start.Lexing.pos_fname;

              List.find (fun cmt ->
                let f_from_loc = loc.Location.loc_start.Lexing.pos_fname in
                let base_f = Filename.basename f_from_loc in
                let f_length = String.length base_f in
                let cmt_file = Filename.basename cmt in
                let cmt_length = String.length cmt_file - 4 in

                if  (String.sub base_f 0 (f_length - 3)) =
                    (String.sub cmt_file 0 cmt_length) ||
                    (String.sub base_f 0 (f_length - 4)) =
                      (String.sub cmt_file 0 cmt_length) then begin
                  min_cmts := (f_from_loc, cmt) :: !min_cmts;  true
                end else false
              ) all_cmts in
          debug " ==== (3) READING THE CMT %s ====\n%!" filename;

          let f = open_in filename in
          if !d_cmt then Printf.eprintf "Reading %s... OK\n%!" filename;
          let cmt = Cmt_format.read_cmt filename in
          debug " ==== (4) TYPING FROM THE CMT %s ====\n%!" filename;

          begin match cmt.Cmt_format.cmt_annots with
          | Cmt_format.Implementation typedtree ->
            begin
              try
                Retype.current_loc := loc;
                Retype.Iterator.iter_structure typedtree;
              with Retype.Type ty ->
                begin match ty.Types.desc with
                | Types.Tarrow _ -> () (* TOOD ? *)
                | _ ->
                  save_type loc ty;
                  Format.fprintf Format.str_formatter "%a" Printtyp.type_expr ty;
                  let ty_str = Str.global_replace (Str.regexp " ") "_" (Format.flush_str_formatter ()) in
                  update ty_str loc;
                  if !d_cmt then begin
                    Printtyp.reset ();
                    Format.eprintf "\nloc: %a%a\n%!" Location.print loc Printtyp.type_expr ty
                  end (* endif *)
                end
            end (* endtry *)
          | _ -> () (* FAIL WITH ./_build/camlp4/Camlp4.cmt *)
          end; (* endmatch *)
          debug "      ===> DONE\n%!";
          close_in f
        with Not_found -> (* Format.eprintf "Not found %s\n%!" loc.Location.loc_start.Lexing.pos_fname; *)();
      end (* endif *)
  ) heap_loc

let dummy_loc = Location.none
(* Special treatment for some id, see profiling_ids.h *)
let special_ty locs locid =
  match locid with
  | 01 -> update "dummy" dummy_loc; true
  | 10 -> update "string" dummy_loc; true
  | 13 -> update "md5" dummy_loc; true
  | 40 -> update "native_integer" dummy_loc; true
  | 42 -> update "unknown" dummy_loc; true
  | 50 | 51 ->  update "float" dummy_loc; true
  | 70 -> update "array" dummy_loc; true
  | 101 -> update "obj" dummy_loc; true
  | 130 -> update "alloc" dummy_loc; true
  | 150 -> update "io" dummy_loc; true
  | _ -> false


let main () =
  Arg.parse [
    "-v", Arg.Int ((:=) arg_verbose),  " : set verbosity (0=no, 1=loading, 2=more,...)";
    "-loc", Arg.Unit (fun () -> loc_info := true),  " : set verbosity (0=no, 1=loading, 2=more,...)";
    "-prof", Arg.String (fun s -> prof_files := [s]),  " <filename> : specify a .prof file";

    "-heap", Arg.String (fun name ->

      Format.eprintf " ==== Getting all .prof files ====\n%!";
      let locs =  (* Fill the global hashtbl locs *)
        if !prof_files = [] then get_all_locations () else get_locations !prof_files in
      Format.eprintf " ==== Getting all cmts ==== \n%!";
      let all_cmts =  get_all_cmts () in
      debug "%d\n%!" (List.length all_cmts);

      let pid = Scanf.sscanf name "heap.dump.%d." (fun x -> x) in
      let hp_name = Format.sprintf "%d.hp" pid in

      let oc = open_out hp_name in
      let oc_formatter = Format.formatter_of_out_channel oc in

      Format.fprintf oc_formatter "JOB \"Types\"\n";
      Format.fprintf oc_formatter "DATE \"---\"\n";
      Format.fprintf oc_formatter "SAMPLE_UNIT \"GC\"\n";
      Format.fprintf oc_formatter "VALUE_UNIT \"Values\"\n";

      if Sys.file_exists name then
        begin
          debug " ==== Loading heap image ====\n%!";
          let h, locids = Load_heap.read_heap name locs in
          debug " ==== Done%!";

          debug " ==== Computing heap locations ====\n%!";
          let heap_loc =
            List.fold_left (fun acc locid ->
              (* Special treatment for some id *)
              if special_ty locs locid then acc
              else if Hashtbl.mem locs locid then Hashtbl.find locs locid :: acc
              else acc
            ) [] locids in
          debug " ==== Typing from locations ====\n%!";
          get_type_from_loc all_cmts heap_loc;
          debug " DONE\n%!";
          (* Start dumping to hp file *)
          let buffer = (Buffer.create 1024) in
          let buf = Format.formatter_of_buffer buffer  in


          Hashtbl.iter (fun (loc, ty) v ->
            if not !loc_info then
              Format.fprintf buf "  %s %d\n" ty v
            else begin
              if loc = Location.none then
                Format.fprintf buf "  %s %d\n" ty v
              else
                Format.fprintf buf "  %s::%a %d\n" ty print_loc loc v
            end
          ) inheap;

          Hashtbl.reset inheap;
          Format.fprintf oc_formatter "BEGIN_SAMPLE 0.\n";
          Format.fprintf oc_formatter "%s" (Buffer.contents buffer);
          Format.fprintf oc_formatter "END_SAMPLE 0.\n";

          (* Bad hack... *)
          Format.fprintf oc_formatter "BEGIN_SAMPLE 1.\n";
          Format.fprintf oc_formatter "%s" (Buffer.contents buffer);
          Format.fprintf oc_formatter "END_SAMPLE 1.\n";
        (* ******************************************* *)
        end;

      close_out oc;
      let _  = Sys.command (hp2ps ^ " " ^ hp_name) in
      Format.eprintf "%d.ps Generated\n%!" pid

    )," <filename> : load the type description from <filename>";

    "-heaps", Arg.Int (fun pid ->
      Format.eprintf " ==== Getting all .prof files ====\n%!";
      let locs =  (* Fill the global hashtbl locs *)
        if !prof_files = [] then get_all_locations () else get_locations !prof_files in
      Format.eprintf " ==== Getting all cmts ==== \n%!";
      let all_cmts =  get_all_cmts () in
      debug "%d\n%!" (List.length all_cmts);

      let hp_name = Format.sprintf "%d.hp" pid in

      let oc = open_out hp_name in
      let oc_formatter = Format.formatter_of_out_channel oc in

      Format.fprintf oc_formatter "JOB \"Types\"\n";
      Format.fprintf oc_formatter "DATE \"---\"\n";
      Format.fprintf oc_formatter "SAMPLE_UNIT \"GC\"\n";
      Format.fprintf oc_formatter "VALUE_UNIT \"Values\"\n";

      (try
        for i = 0 to 100 do
          let name = Format.sprintf "heap.dump.%d.%d" pid i in
          if Sys.file_exists name then begin
            debug " ==== Loading heap image ====\n%!";
            let h, locids = Load_heap.read_heap name locs in
            debug " ==== Done%!";

            debug " ==== Computing heap locations ====\n%!";
            let heap_loc =
              List.fold_left (fun acc locid ->
                (* Special treatment for some id *)
                if special_ty locs locid then acc
                else if Hashtbl.mem locs locid then Hashtbl.find locs locid :: acc
                else acc
              ) [] locids in
            debug " ==== Typing from locations ====\n%!";
            get_type_from_loc all_cmts heap_loc;
            debug " DONE\n%!";
            (* Start dumping to hp file *)
            Format.fprintf oc_formatter "BEGIN_SAMPLE %d.\n" i;

            Hashtbl.iter (fun (loc, ty) v ->
              (* Format.fprintf oc_formatter "  %s %d\n"  ty v *)
              if not !loc_info then
                Format.fprintf oc_formatter "  %s %d\n"  ty v
              else begin
                if loc = Location.none then
                  Format.fprintf oc_formatter "  %s %d\n" ty v
                else
                  Format.fprintf oc_formatter "  %s::%a %d\n" ty print_loc loc v
              end
            ) inheap;

            Hashtbl.reset inheap;
            Format.fprintf oc_formatter "END_SAMPLE %d.\n" i;
          end
        done
      with e ->
        Format.eprintf "Exception %s\n%!" (Printexc.to_string e);
        print_newline ();
      );

      close_out oc;
      let _  = Sys.command (hp2ps ^ " " ^ hp_name) in
      Format.eprintf "%d.ps Generated\n%!" pid

    ), "<pid> : ......";
  ]
    (fun s ->
      Printf.printf "Error: don't know what to do with %s\n%!" s;
      exit 1)
    "Ocaml Heap Profiler"


let _ =
  try
    main ()
  with e ->
    Printf.printf "Exception %s\n%!" (Printexc.to_string e);
    exit 2
