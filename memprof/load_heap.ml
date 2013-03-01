open Heap_types
open Utils

type loader = {
  new_block : (int -> int -> int -> int array -> unit);
  load_pointer : (int -> in_channel -> int);
}

(*************************************************************************)
(*                                                                       *)
(*                         load_int                                      *)
(*                                                                       *)
(*************************************************************************)

let module_tag = 245

let load_pointer size ic =
  if size = 4 then
    let c0 =  int_of_char (input_char ic) in
    let c1 =  int_of_char (input_char ic) in
    let c2 =  int_of_char (input_char ic) in
    let c3 =  int_of_char (input_char ic) in
    if c0 land 1 = 1 then 0 else
      (c0 lsr 1) lor
        (c1 lsl 7) lor
        (c2 lsl 15) lor
        (c3 lsl 23)
  else
    let c0 =  int_of_char (input_char ic) in
    let c1 =  int_of_char (input_char ic) in
    let c2 =  int_of_char (input_char ic) in
    let c3 =  int_of_char (input_char ic) in
    let c4 =  int_of_char (input_char ic) in
    let c5 =  int_of_char (input_char ic) in
    let c6 =  int_of_char (input_char ic) in
    let c7 =  int_of_char (input_char ic) in
    if c0 land 1 = 1 then 0 else
      (c0 lsr 1) lor
        (c1 lsl 7) lor
        (c2 lsl 15) lor
        (c3 lsl 23) lor
        (c4 lsl 31) lor
        (c5 lsl 39) lor
        (c6 lsl 47) lor
        (c7 lsl 55)


let load_int size ic =
  if size = 4 then
    let c0 =  int_of_char (input_char ic) in
    let c1 =  int_of_char (input_char ic) in
    let c2 =  int_of_char (input_char ic) in
    let c3 =  int_of_char (input_char ic) in
    c0 lor
      (c1 lsl 8) lor
      (c2 lsl 16) lor
      (c3 lsl 24)
  else
    let c0 =  int_of_char (input_char ic) in
    let c1 =  int_of_char (input_char ic) in
    let c2 =  int_of_char (input_char ic) in
    let c3 =  int_of_char (input_char ic) in
    let c4 =  int_of_char (input_char ic) in
    let c5 =  int_of_char (input_char ic) in
    let c6 =  int_of_char (input_char ic) in
    let c7 =  int_of_char (input_char ic) in
    c0 lor
      (c1 lsl 8) lor
      (c2 lsl 16) lor
      (c3 lsl 24) lor
      (c4 lsl 32) lor
      (c5 lsl 40) lor
      (c6 lsl 48) lor
      (c7 lsl 56)

      (*************************************************************************)
      (*                                                                       *)
      (*                         load_file                                     *)
      (*                                                                       *)
      (*************************************************************************)

open Utils

let load_file filename l locs =
  let ic = open_in_bin filename in
  (* Getting location identifier *)

  let archsize = int_of_char (input_char ic) in
  if arg_verbose_load () then Printf.printf "sizeof(value): %d\n%!" archsize;

  let namesize = load_int archsize ic in
  let binary_name = String.create namesize in
  really_input ic binary_name 0 namesize;
  if arg_verbose_load () then Printf.printf "Binary: %s (%d)\n%!" binary_name namesize;

  let locids = ref [] in
  let rec iter_chunk chunks =
    let opcode = int_of_char (input_char ic) in
    if opcode = 0 then begin
      for i = 1 to 2 * archsize do ignore (input_char ic); done;
      (* Printf.printf "chunk";  print_newline (); *)
      iter_chunk chunks
    end else
    if opcode = 1 then begin
      (* Printf.printf "load value"; print_newline (); *)
      let pointer = l.load_pointer archsize ic in
      let tag = int_of_char (input_char ic) in
      let size = load_int archsize ic in
      let locid = load_int archsize ic in
      locids := locid :: !locids;

      if tag < 251 then
        let b = Array.create size 0 in
        for i = 0 to size - 1 do
          (*            Printf.printf "load field %d" i; print_newline ();  *)
          b.(i) <- l.load_pointer archsize ic;
        done;
        l.new_block pointer tag size b
      else
        l.new_block pointer tag size [||];
      iter_chunk chunks
    end else
    if opcode = 10 then
      iter_chunk chunks
    else
      begin
        chunks
      end
  in
  let _chunks = iter_chunk [] in
  if arg_verbose_load () then Printf.printf "Iter on blocks done\n%!";

  (* Printf.eprintf " #string: %d\n #todo: %d\n #floats: %d\n #array: %d\n #obj: %d\n%!" *)
  (*   !str_cpt !unknown_cpt !float_cpt !array_cpt !obj_cpt; *)

  let _code_area_start = l.load_pointer archsize ic in
  let _code_area_end = l.load_pointer archsize ic in

  let delim = load_int archsize ic in
  (*  Printf.printf "Native code: %b | Bytecode: %b\n%!" (delim = 0) (delim = 3);*)

  if delim = 0 then begin   (* ##### NATIVE CODE ##### *)
    Printf.eprintf "## NATIVE CODE ##\n%!";
    let len = load_int archsize ic in
    let globals_map = String.create len in
    really_input ic globals_map 0 len;
    let (globals_map : (string * string) list) =
      Marshal.from_string globals_map 0 in
    (*    Printf.eprintf "(1)\n%!";*)
    let globals_map = List.map fst globals_map in
    (*    Printf.eprintf "(2)\n%!";*)
    let globals_map = Array.of_list globals_map in

    let rec iter list =
      let v = load_int archsize ic in
      if v = 0 then List.rev list else
        let tag = module_tag in
        let pointer = l.load_pointer archsize ic in
        let size = load_int archsize ic in
        let b = Array.create size 0 in
        for i = 0 to size - 1 do
          b.(i) <- l.load_pointer archsize ic;
        done;
        l.new_block pointer tag size b;
        iter (pointer :: list)
    in
    let caml_globals = iter [] in
    let caml_globals = Array.of_list caml_globals in
    (*    Printf.eprintf "(3)\n%!";*)
    let rec iter list =
      let v = load_int archsize ic in
      if v = 0 then List.rev list else
        let len = load_int archsize ic in
        let info = String.create len in
        really_input ic info 0 len;
        iter (info :: list)
    in
    let infos = iter [] in
    (*    Printf.eprintf "(4)\n%!";*)
    let infos =
      List.rev
        (List.rev_map (fun s -> (Marshal.from_string s 0 : Heap_types.mem_repr))
           infos) in
    let infos = Array.of_list infos in
    (*    Printf.eprintf "(5)\n%!";*)
    close_in ic;
    {
      binary_name = binary_name;
      caml_globals = caml_globals;
      mem_repr = infos;
      globals_map = globals_map;
      archsize = archsize;
    }, !locids
  end else
  if delim = 3 then begin   (* ###### BYTECODE ###### *)
    (* Printf.eprintf "## BYTECODE ## \n%!"; *)
    let globals_data_ptr = l.load_pointer archsize ic in (* global table *)
    (*    Printf.eprintf "(1)\n%!";*)
    let _nb_entry = load_int archsize ic in (* number of entries in the stack  *)
    (* Printf.eprintf "VALUES: : %d\n%!" (_nb_entry lsr 1); *)
    (*    Printf.eprintf "(2)\n%!";*)
    let rec iter_sp list =
      let v = load_int archsize ic in
      if v = 0 then  (* END *)
        List.rev list
      else
        let value = l.load_pointer archsize ic in
        iter_sp (value :: list)
    in
    let _stack_pointers = iter_sp [] in

    let rec iter_roots list =
      let value = load_int archsize ic in
      if value = 1 then List.rev list
      else iter_roots (value :: list)
    in
    let _roots = iter_roots [] in
  if arg_verbose_load () then Printf.printf " .... done\n%!";

    close_in ic;
    {
      binary_name = binary_name;
      caml_globals = (* caml_globals *)[|globals_data_ptr|];
      mem_repr = (* infos *)[||];
      globals_map = (* globals_map *)[||];
      archsize = archsize;
    }, !locids
  end else begin
    close_in ic;
    assert false
  end

(* let load_file2 filename l = *)

(*   let ic = open_in_bin filename in *)
(*   (\* Getting location identifier *\) *)
(*   iter_dir "."; *)
(*   List.iter (fun filename -> *)
(*     let f = open_in filename in *)
(*     let h : (int, Location.t) Hashtbl.t = input_value f in *)
(*     Hashtbl.iter (Hashtbl.replace locs) h; *)
(*     close_in f *)
(*   ) !prof_files; *)

(*   let archsize = int_of_char (input_char ic) in *)
(*   if arg_verbose_load () then Printf.printf "sizeof(value): %d\n%!" archsize; *)

(*   let namesize = load_int archsize ic in *)
(*   let binary_name = String.create namesize in *)
(*   really_input ic binary_name 0 namesize; *)
(*   if arg_verbose_load () then Printf.printf "Binary: %s (%d)\n%!" binary_name namesize; *)

(*   let str_cpt = ref 0 in *)
(*   let float_cpt = ref 0 in *)
(*   let unknown_cpt = ref 0 in *)
(*   let array_cpt = ref 0 in *)
(*   let obj_cpt = ref 0 in *)
(*   let rec iter_chunk chunks = *)
(*     let opcode = int_of_char (input_char ic) in *)
(*     if opcode = 0 then begin *)
(*       for i = 1 to 2 * archsize do ignore (input_char ic); done; *)
(*       (\* Printf.printf "chunk";  print_newline (); *\) *)
(*       iter_chunk chunks *)
(*     end else *)
(*     if opcode = 1 then begin *)
(*       (\* Printf.printf "load value"; print_newline (); *\) *)
(*       let pointer = l.load_pointer archsize ic in *)
(*       let tag = int_of_char (input_char ic) in *)
(*       let size = load_int archsize ic in *)
(*       let locid = load_int archsize ic in *)
(*       if locid = 10 then incr str_cpt *)
(*       else if locid = 42 then incr unknown_cpt *)
(*       else if locid = 50 || locid = 51 then incr float_cpt *)
(*       else if locid = 70 then incr array_cpt *)
(*       else if locid = 101 then incr obj_cpt *)
(*       else begin *)
(*         try *)
(*           Format.eprintf "bsize: %d|tag: %d|locid: %d|loc:   %a\n%!" *)
(*             size tag locid Location.print_loc (Hashtbl.find locs locid); *)
(*         with Not_found -> *)
(*           Format.eprintf "bsize: %d|tag: %d|locid: %d\n%!" size tag locid; *)
(*       end; *)
(*       if tag < 251 then *)
(*         let b = Array.create size 0 in *)
(*         for i = 0 to size - 1 do *)
(*           (\*            Printf.printf "load field %d" i; print_newline ();  *\) *)
(*           b.(i) <- l.load_pointer archsize ic; *)
(*         done; *)
(*         l.new_block pointer tag size b *)
(*       else *)
(*         l.new_block pointer tag size [||]; *)
(*       iter_chunk chunks *)
(*     end else *)
(*     if opcode = 10 then *)
(*       iter_chunk chunks *)
(*     else *)
(*       begin *)
(*         chunks *)
(*       end *)
(*   in *)
(*   let _chunks = iter_chunk [] in *)
(*   Printf.eprintf " #string: %d\n #todo: %d\n #floats: %d\n #array: %d\n #obj: %d\n%!" *)
(*     !str_cpt !unknown_cpt !float_cpt !array_cpt !obj_cpt; *)

(*   let _code_area_start = l.load_pointer archsize ic in *)
(*   let _code_area_end = l.load_pointer archsize ic in *)

(*   let delim = load_int archsize ic in *)
(*   (\*  Printf.printf "Native code: %b | Bytecode: %b\n%!" (delim = 0) (delim = 3);*\) *)

(*   if delim = 0 then begin   (\* ##### NATIVE CODE ##### *\) *)
(*     Printf.eprintf "## NATIVE CODE ##\n%!"; *)
(*     let len = load_int archsize ic in *)
(*     let globals_map = String.create len in *)
(*     really_input ic globals_map 0 len; *)
(*     let (globals_map : (string * string) list) = *)
(*       Marshal.from_string globals_map 0 in *)
(*     (\*    Printf.eprintf "(1)\n%!";*\) *)
(*     let globals_map = List.map fst globals_map in *)
(*     (\*    Printf.eprintf "(2)\n%!";*\) *)
(*     let globals_map = Array.of_list globals_map in *)

(*     let rec iter list = *)
(*       let v = load_int archsize ic in *)
(*       if v = 0 then List.rev list else *)
(*         let tag = module_tag in *)
(*         let pointer = l.load_pointer archsize ic in *)
(*         let size = load_int archsize ic in *)
(*         let b = Array.create size 0 in *)
(*         for i = 0 to size - 1 do *)
(*           b.(i) <- l.load_pointer archsize ic; *)
(*         done; *)
(*         l.new_block pointer tag size b; *)
(*         iter (pointer :: list) *)
(*     in *)
(*     let caml_globals = iter [] in *)
(*     let caml_globals = Array.of_list caml_globals in *)
(*     (\*    Printf.eprintf "(3)\n%!";*\) *)
(*     let rec iter list = *)
(*       let v = load_int archsize ic in *)
(*       if v = 0 then List.rev list else *)
(*         let len = load_int archsize ic in *)
(*         let info = String.create len in *)
(*         really_input ic info 0 len; *)
(*         iter (info :: list) *)
(*     in *)
(*     let infos = iter [] in *)
(*     (\*    Printf.eprintf "(4)\n%!";*\) *)
(*     let infos = *)
(*       List.rev *)
(*         (List.rev_map (fun s -> (Marshal.from_string s 0 : HPTypes.mem_repr)) *)
(*            infos) in *)
(*     let infos = Array.of_list infos in *)
(*     (\*    Printf.eprintf "(5)\n%!";*\) *)
(*     close_in ic; *)
(*     { *)
(*       binary_name = binary_name; *)
(*       caml_globals = caml_globals; *)
(*       mem_repr = infos; *)
(*       globals_map = globals_map; *)
(*       archsize = archsize; *)
(*     } *)
(*   end else *)
(*   if delim = 3 then begin   (\* ###### BYTECODE ###### *\) *)
(*     (\* Printf.eprintf "## BYTECODE ## \n%!"; *\) *)
(*     let globals_data_ptr = l.load_pointer archsize ic in (\* global table *\) *)
(*     (\*    Printf.eprintf "(1)\n%!";*\) *)
(*     let _nb_entry = load_int archsize ic in (\* number of entries in the stack  *\) *)
(*     (\* Printf.eprintf "VALUES: : %d\n%!" (_nb_entry lsr 1); *\) *)
(*     (\*    Printf.eprintf "(2)\n%!";*\) *)
(*     let rec iter_sp list = *)
(*       let v = load_int archsize ic in *)
(*       if v = 0 then  (\* END *\) *)
(*         List.rev list *)
(*       else *)
(*         let value = l.load_pointer archsize ic in *)
(*         iter_sp (value :: list) *)
(*     in *)
(*     let _stack_pointers = iter_sp [] in *)

(*     let rec iter_roots list = *)
(*       let value = load_int archsize ic in *)
(*       if value = 1 then List.rev list *)
(*       else iter_roots (value :: list) *)
(*     in *)
(*     let _roots = iter_roots [] in *)

(*     close_in ic; *)
(*     { *)
(*       binary_name = binary_name; *)
(*       caml_globals = (\* caml_globals *\)[|globals_data_ptr|]; *)
(*       mem_repr = (\* infos *\)[||]; *)
(*       globals_map = (\* globals_map *\)[||]; *)
(*       archsize = archsize; *)
(*     } *)
(*   end else begin *)
(*     close_in ic; *)
(*     assert false *)
(*   end *)

  (*************************************************************************)
  (*                                                                       *)
  (*                         load_pointer                                  *)
  (*                                                                       *)
  (*************************************************************************)

  (*************************************************************************)
  (*                                                                       *)
  (*                         read_heap                                     *)
  (*                                                                       *)
  (*************************************************************************)

let read_heap filename locs =
  Format.eprintf "Loading %s\n%!" filename;
  let blocks = Hashtbl.create 1111 in
  let counter = ref 2 in (* 0 is INTEGER, 1 is UNKNOWN *)

  let new_block pointer tag size b =
    Hashtbl.add blocks pointer !counter;
    incr counter
  in

  let loader = {
    new_block = new_block;
    load_pointer = load_pointer;
  } in

  let array = Array.create !counter unknown_block in

  let h, locids = load_file filename loader locs in
  let h = {
    hp_blocks = array;
    hp_info = h;
  } in
  h, locids
