(* Configuration *)
let debug_flag = ref false
let d_prof = ref false
let d_cmt = ref false
let ext_prof = ".prof"
let ext_cmt = ".cmt"
let prof_dirs = ["."]
let cmt_dirs = ["."; "/home/cago/dev/ocaml-private/stdlib"]
let hp2ps = "hp2ps"
let level = 3
let loc_info = ref false

let arg_verbose = ref 0

let arg_verbose_load () = !arg_verbose land 1 = 1
let arg_verbose_types () = !arg_verbose land 2 = 2
let arg_verbose_types2 () = !arg_verbose land 4 = 4

let is_block n = (n <> 0)

let unknown_block =
  let open Heap_types in {
    block_scanned = ref false;
    block_tag = 0;
    block_size = 0;
    block_content = [||];
    block_reverse = [];
    block_weight = 0;
    block_type = None;
  }

let debug fmt = if !debug_flag then Printf.eprintf fmt else Printf.ifprintf stderr fmt

(* Iterate in a directory on a specific file extension *)
let iter_dir extension accufiles dirname =
  let rec loop extension accufiles dirname nested =
    debug "Analyzing.... %s\n%!" dirname;
    let files = Sys.readdir dirname in
    Array.iter (fun file ->
      let file = Filename.concat dirname file in
      begin
        try
          if Sys.is_directory file && nested < level then loop extension accufiles file (nested + 1);
        with Sys_error _ -> Format.eprintf "  - ignore... %s\n%!" file
      end;
      if Filename.check_suffix file extension then accufiles := file :: !accufiles
    ) files in
  loop extension accufiles dirname 0

(* id -> loc *)
let iter_prof dirs =
  let prof_files = ref [] in
  List.iter (iter_dir ext_prof prof_files) dirs;
  !prof_files

(* loc -> ty *)
let iter_cmt dirs =
  let cmt_files = ref [] in
  List.iter (iter_dir ext_cmt cmt_files) dirs;
  !cmt_files

let get_all_cmts () = iter_cmt cmt_dirs

let print_loc ppf loc =
  let open Location in let open Lexing in let open Format in
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  if file = "//toplevel//" then begin
    if highlight_locations ppf loc none then () else
      fprintf ppf "[%i-%i]" loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  end else begin
    fprintf ppf "%a[%i]" print_filename file line;
    if startchar >= 0 then
      fprintf ppf "[%i-%i]" startchar endchar
  end

let print_hashtbl h =
  Format.printf  "Size: %d\n" (Hashtbl.length h);
  Hashtbl.iter (fun k v ->
    Format.printf "  k: %d  loc: %a\n%!" k print_loc v;
  ) h

(* let get_minimal_cmts locs = *)
(*   let min_cmts = Hashtbl.create 46 in *)
(*   let add_cmt orig cmt = Hashtbl.replace min_cmts orig cmt in *)

(*   let files_from_locs = *)
(*     List.fold_left (fun acc loc -> *)
(*       if loc <> Location.none then loc.Location.loc_start.Lexing.pos_fname :: acc else acc *)
(*   ) [] locs in *)
(*   let all_cmts = iter_cmt cmt_dirs in *)
(*   List.iter (fun cmt -> *)
(*     List.iter (fun f_from_loc -> *)
(*       if match_str (String.sub f_from_loc 0 (String.length f_from_loc - 2)) cmt then *)
(*         add_cmt f_from_loc cmt *)
(*     ) files_from_locs; *)
(*   ) all_cmts; *)
(*   min_cmts *)


let get_minimal_cmts all_cmts locs =
  Format.eprintf "UTILS (1)\n%!";
  let files_from_locs =
    List.fold_left (fun acc loc ->
      if loc <> Location.none then loc.Location.loc_start.Lexing.pos_fname :: acc else acc
    ) [] locs in

  Format.eprintf " UTILS (2) %d \n%!" (List.length all_cmts);

  let res =  List.fold_left (fun acc cmt ->
      List.fold_left (fun acc f_from_loc->
        if f_from_loc = Filename.basename cmt then (f_from_loc, cmt) :: acc else acc
      ) acc files_from_locs;
    ) [] all_cmts in
  Format.eprintf " UTILS (3) %d \n%!" (List.length res);
  res

(* Id to Location table *)
let get_all_locations =
  let locs : (int, Location.t) Hashtbl.t = Hashtbl.create 44 in
  let prof_files = iter_prof prof_dirs in
  fun () ->
    List.iter (fun filename ->
      let f = open_in filename in
      if not !d_prof then Format.eprintf "  - reading %s... OK\n%!" filename;
      let h : (int, Location.t) Hashtbl.t = input_value f in
      Hashtbl.iter (fun id loc -> if loc <> Location.none then Hashtbl.replace locs id loc) h;
      if !d_prof then print_hashtbl h;
      close_in f
    ) prof_files;
    locs

let get_locations =
  let locs : (int, Location.t) Hashtbl.t = Hashtbl.create 44 in
  fun prof_files ->
    List.iter (fun filename ->
      let f = open_in filename in
      if not !d_prof then Format.eprintf "  - reading %s... OK\n%!" filename;
      let h : (int, Location.t) Hashtbl.t = input_value f in
      Hashtbl.iter (fun id loc -> if loc <> Location.none then Hashtbl.replace locs id loc) h;
      if !d_prof then print_hashtbl h;
      close_in f
    ) prof_files;
    locs
