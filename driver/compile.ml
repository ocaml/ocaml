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
  init_path();
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    fatal_error "cannot open Pervasives.cmi"

(* Compile a .mli file *)

let interface sourcefile =
  let prefixname = Filename.chop_suffix sourcefile ".mli" in
  let modulename = capitalize(Filename.basename prefixname) in
  let ic = open_in_bin sourcefile in
  let lb = Lexing.from_channel ic in
  Location.input_name := sourcefile;
  try
    let sg = Typemod.transl_signature (initial_env()) (Parse.interface lb) in
    close_in ic;
    if !Clflags.print_types then (Printtyp.signature sg; print_flush());
    Env.save_signature sg modulename (prefixname ^ ".cmi");
    ()
  with x ->
    close_in ic;
    raise x

(* Compile a .ml file *)

let print_if flag printer arg =
  if !flag then begin printer arg; print_newline() end;
  arg

let implementation sourcefile =
  let prefixname = Filename.chop_suffix sourcefile ".ml" in
  let modulename = capitalize(Filename.basename prefixname) in
  let objfile = prefixname ^ ".cmo" in
  let ic = open_in_bin sourcefile in
  let oc = open_out_bin objfile in
  let lb = Lexing.from_channel ic in
  Location.input_name := sourcefile;
  try
    let (str, sg, finalenv) =
      Typemod.type_structure (initial_env()) (Parse.implementation lb) in
    if !Clflags.print_types then (Printtyp.signature sg; print_flush());
    let (coercion, crc) =
      if Sys.file_exists (prefixname ^ ".mli") then begin
        let intf_file =
          try find_in_path !load_path (prefixname ^ ".cmi")
          with Not_found -> prefixname ^ ".cmi" in
        let (dclsig, crc) = Env.read_signature modulename intf_file in
        (Includemod.signatures Env.initial sg dclsig, crc)
      end else begin
        let crc = Env.save_signature sg modulename (prefixname ^ ".cmi") in
        (Tcoerce_none, crc)
      end in
    Emitcode.to_file oc modulename crc
      (print_if Clflags.dump_instr Printinstr.instrlist
        (Bytegen.compile_implementation
          (print_if Clflags.dump_lambda Printlambda.lambda
            (Translmod.transl_implementation modulename str coercion))));
    close_in ic;
    close_out oc
  with x ->
    close_in ic;
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
