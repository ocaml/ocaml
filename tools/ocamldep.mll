(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

{
(* Remember the possibly free structure identifiers *)

module StringSet = 
  Set.Make(struct type t = string let compare = compare end)

let free_structure_names = ref StringSet.empty

let add_structure name =
  free_structure_names := StringSet.add name !free_structure_names

(* For nested comments *)

let comment_depth = ref 0

}

rule main = parse
    "open" [' ' '\010' '\013' '\009' '\012'] +
      { struct_name lexbuf; main lexbuf }
  | ['A'-'Z' '\192'-'\214' '\216'-'\222' ]
    (['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
      '\'' '0'-'9' ]) * '.'
      { let s = Lexing.lexeme lexbuf in
        add_structure(String.sub s 0 (String.length s - 1));
        main lexbuf }
  | "\""
      { string lexbuf; main lexbuf }
  | "(*"
      { comment_depth := 1; comment lexbuf; main lexbuf }
  | eof
      { () }
  | _
      { main lexbuf }

and struct_name = parse
    ['A'-'Z' '\192'-'\214' '\216'-'\222' ]
    (['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
      '\'' '0'-'9' ]) *
      { add_structure(Lexing.lexeme lexbuf) }
  | ""
      { () }

and comment = parse
    "(*"
      { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then comment lexbuf }
  | "\""
      { string lexbuf; comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof
      { () }
  | _
      { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\010\013") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { string lexbuf }
  | eof
      { () }
  | _
      { string lexbuf }

{
(* Print the dependencies *)

let load_path = ref [""]

let opt_flag = ref true

let find_dependency modname (byt_deps, opt_deps) =
  let name = String.uncapitalize modname in
  try
    let filename = Misc.find_in_path !load_path (name ^ ".mli") in
    let basename = Filename.chop_suffix filename ".mli" in
    ((basename ^ ".cmi") :: byt_deps,
     (if !opt_flag & Sys.file_exists (basename ^ ".ml")
      then basename ^ ".cmx"
      else basename ^ ".cmi") :: opt_deps)
  with Not_found ->
  try
    let filename = Misc.find_in_path !load_path (name ^ ".ml") in
    let basename = Filename.chop_suffix filename ".ml" in
    ((basename ^ ".cmo") :: byt_deps,
     (basename ^ ".cmx") :: opt_deps)
  with Not_found ->
    (byt_deps, opt_deps)

let (depends_on, escaped_eol) =
  match (Sys.get_config ()).Sys.os_type with
  | "Unix" | "Win32" -> (": ", "\\\n    ")
  | "MacOS" -> ("\196 ", "\182\n    ")
  | x -> failwith ("Ocamldep: unknown system type" ^ x)
;;

let print_dependencies target_file deps =
  match deps with
    [] -> ()
  | _ ->
    print_string target_file; print_string depends_on;
    let rec print_items pos = function
      [] -> print_string "\n"
    | dep :: rem ->
        if pos + String.length dep <= 77 then begin
          print_string dep; print_string " ";
          print_items (pos + String.length dep + 1) rem
        end else begin
          print_string escaped_eol; print_string dep; print_string " ";
          print_items (String.length dep + 5) rem
        end in
    print_items (String.length target_file + 2) deps

let file_dependencies source_file =
  try
    free_structure_names := StringSet.empty;
    let ic = open_in source_file in
    let lb = Lexing.from_channel ic in
    main lb;
    if Filename.check_suffix source_file ".ml" then begin
      let basename = Filename.chop_suffix source_file ".ml" in
      let init_deps =
        if Sys.file_exists (basename ^ ".mli")
        then let cmi_name = basename ^ ".cmi" in ([cmi_name], [cmi_name])
        else ([], []) in
      let (byt_deps, opt_deps) =
        StringSet.fold find_dependency !free_structure_names init_deps in
      print_dependencies (basename ^ ".cmo") byt_deps;
      print_dependencies (basename ^ ".cmx") opt_deps
    end else
    if Filename.check_suffix source_file ".mli" then begin
      let basename = Filename.chop_suffix source_file ".mli" in
      let (byt_deps, opt_deps) =
        StringSet.fold find_dependency !free_structure_names ([], []) in
      print_dependencies (basename ^ ".cmi") byt_deps
    end else
      ();
    close_in ic
  with Sys_error msg ->
    ()

(* Entry point *)

let usage = "Usage: ocamldep [-I <dir>] <files>"

let _ =
  Arg.parse [
     "-I", Arg.String(fun dir -> load_path := !load_path @ [dir]),
           "<dir>  Add <dir> to the list of include directories";
     "-opt", Arg.Set opt_flag, " (undocumented)";
     "-noopt", Arg.Clear opt_flag, " (undocumented)"
    ] file_dependencies usage;
  exit 0
    
}
