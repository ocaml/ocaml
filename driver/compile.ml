(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree

(* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory. *)

let init_path () =
  load_path :=
    "" :: List.rev (Config.standard_library :: !Clflags.include_dirs);
  Env.reset_cache()

(* Return the initial environment in which compilation proceeds. *)

let initial_env () =
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    fatal_error "cannot open Pervasives.cmi"

(* Optionally preprocess a source file *)

let preprocess sourcefile tmpfile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      let comm = pp ^ " " ^ sourcefile ^ " > " ^ tmpfile in
      if Sys.command comm <> 0 then begin
        Printf.eprintf "Preprocessing error\n";
        flush stderr;
        exit 2
      end;
      tmpfile

let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> remove_file inputfile

(* Parse a file or get a dumped syntax tree in it *)

let parse_file inputfile parse_fun ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = String.create (String.length ast_magic) in
      really_input ic buffer 0 (String.length ast_magic);
      buffer = ast_magic
    with _ -> false
  in
  let ast =
    try
      if is_ast_file then begin
        Location.input_name := input_value ic;
        input_value ic
      end else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        parse_fun (Lexing.from_channel ic)
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast

(* Compile a .mli file *)

let interface sourcefile =
  init_path();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = capitalize(Filename.basename prefixname) in
  let inputfile = preprocess sourcefile (prefixname ^ ".ppi") in
  let ast = parse_file inputfile Parse.interface ast_intf_magic_number in
  let sg = Typemod.transl_signature (initial_env()) ast in
  if !Clflags.print_types then (Printtyp.signature sg; print_flush());
  Env.save_signature sg modulename (prefixname ^ ".cmi");
  remove_preprocessed inputfile

(* Compile a .ml file *)

let print_if flag printer arg =
  if !flag then begin printer arg; print_newline() end;
  arg

let implementation sourcefile =
  init_path();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = capitalize(Filename.basename prefixname) in
  let inputfile = preprocess sourcefile (prefixname ^ ".ppo") in
  let ast = parse_file inputfile Parse.implementation ast_impl_magic_number in
  let objfile = prefixname ^ ".cmo" in
  let oc = open_out_bin objfile in
  try
    let (str, sg, finalenv) =
      Typemod.type_structure (initial_env()) ast in
    if !Clflags.print_types then (Printtyp.signature sg; print_flush());
    let (coercion, crc) =
      if Sys.file_exists (prefixname ^ ".mli") then begin
        let intf_file =
          try find_in_path !load_path (prefixname ^ ".cmi")
          with Not_found -> prefixname ^ ".cmi" in
        let (dclsig, crc) = Env.read_signature modulename intf_file in
        (Includemod.compunit sourcefile sg intf_file dclsig, crc)
      end else begin
        let crc = Env.save_signature sg modulename (prefixname ^ ".cmi") in
        Typemod.check_nongen_schemes finalenv str;
        (Tcoerce_none, crc)
      end in
    Emitcode.to_file oc modulename crc
      (print_if Clflags.dump_instr Printinstr.instrlist
        (Bytegen.compile_implementation
          (print_if Clflags.dump_lambda Printlambda.lambda
            (Simplif.simplify_lambda
              (print_if Clflags.dump_rawlambda Printlambda.lambda
                (Translmod.transl_implementation modulename str coercion))))));
    remove_preprocessed inputfile;
    close_out oc
  with x ->
    close_out oc;
    remove_file objfile;
    raise x

let c_file name =
  if Sys.command
     (Printf.sprintf
       "%s -c %s -I%s %s"
       Config.bytecomp_c_compiler
       (String.concat " "
         (List.map (fun dir -> "-I" ^ dir) 
                   (List.rev !Clflags.include_dirs)))
       Config.standard_library
       name)
     <> 0
  then exit 2
