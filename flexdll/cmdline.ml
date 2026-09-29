(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

open Compat

let underscore = ref true
    (* Are "normal" symbols prefixed with an underscore? *)

let machine : [ `x86 | `x64 ] ref = ref `x86

let noexport = ref false
let custom_crt = ref false
let reexport_from_implibs = ref true
let use_default_libs = ref true
let subsystem = ref "console"
let explain = ref false
let builtin_linker = ref false
let toolchain : [ `MSVC | `MSVC64 | `MINGW | `MINGW64 | `GNAT | `GNAT64 | `CYGWIN64 | `LIGHTLD ] ref = ref `MSVC
let use_linker = ref None
let use_mt = ref None
let save_temps = ref false
let show_exports = ref false
let show_imports = ref false
let dry_mode = ref false
let verbose = ref 0
let dirs = ref []
let no_merge_manifest = ref false
let merge_manifest = ref false
let real_manifest = ref true
let add_flexdll_obj = ref true
let files = ref []
let exts = ref []
let output_file = ref ""
let exe_mode : [`DLL | `EXE | `MAINDLL] ref = ref `DLL
let extra_args = ref []
let mode : [`NORMAL | `DUMP | `PATCH] ref = ref `NORMAL
let defexports = ref []
let noentry = ref false
let use_cygpath : [`Yes | `No | `Try] ref = ref `Try
let implib = ref false
let deffile = ref None
let stack_reserve = ref None
let no_rel_relocs = ref false
let base_addr = ref "0x10000"

let usage_msg =
  Printf.sprintf
    "FlexDLL version %s\n\nUsage:\n  flexlink -o <result.dll/exe> file1.obj file2.obj ... -- <extra linker arguments>\n"
    Version.version

let footer =
"Notes:
* The -I, -l and -L options do not need to be separated from their argument.
* An option like /linkXXX is an abbrevation for '-link XXX'.
* An option like -Wl,-XXX is an abbreviation for '-link -XXX'.
* FlexDLL's object files are searched by default in the same directory as
  flexlink, or in the directory given by the environment variable FLEXDIR
  if it is defined.
* Extra argument can be passed in the environment variable FLEXLINKFLAGS.

Homepage: https://github.com/ocaml/flexdll"

let specs = [

  "-o", Arg.Set_string output_file,
  " Choose the name of the output file";

  "-exe", Arg.Unit (fun () -> exe_mode := `EXE),
  " Link the main program as an exe file";

  "-maindll", Arg.Unit (fun () -> exe_mode := `MAINDLL),
  " Link the main program as a dll file";

  "-noflexdllobj", Arg.Clear add_flexdll_obj,
  " Do not add the Flexdll runtime object (for exe)";

  "-noentry", Arg.Set noentry,
  " Do not use the Flexdll entry point (for dll)";

  "-noexport", Arg.Set noexport,
  " Do not export any symbol";

  "-norelrelocs", Arg.Set no_rel_relocs,
  " Ensure that no relative relocation is generated";

  "-base", Arg.String (fun s -> base_addr := s),
  " Specify base address (Win64 only)";

  "-pthread", Arg.Unit (fun () -> extra_args := "-pthread" :: !extra_args),
  " Pass -pthread to the linker";

  "-I", Arg.String (fun dir -> dirs := dir :: !dirs),
  "<dir> Add a directory where to search for files";

  "-L", Arg.String (fun dir -> dirs := dir :: !dirs),
  "<dir> Add a directory where to search for files";

  "-l", Arg.String (fun s -> files := ("-l" ^ s) :: !files),
  "<lib> Library file";

  "-chain", Arg.Symbol (["msvc";"msvc64";"cygwin64";"mingw";"mingw64";"gnat";"gnat64";"ld"],
			(fun s ->
                          machine := `x86; underscore := true;
                          toolchain := match s with
			  | "msvc" -> `MSVC
			  | "msvc64" -> machine := `x64; underscore := false; `MSVC64
			  | "cygwin64" -> machine := `x64; underscore := false; `CYGWIN64
			  | "mingw" -> `MINGW
			  | "gnat" -> `GNAT
			  | "gnat64" -> machine := `x64; underscore := false; `GNAT64
			  | "mingw64" -> machine := `x64; underscore := false; `MINGW64
                          | "ld" -> `LIGHTLD
			  | _ -> assert false)),
  " Choose which linker to use";

  "-use-linker", Arg.String (fun s -> use_linker := Some s),
  "<cmd> Choose an alternative linker to use";

  "-use-mt", Arg.String (fun s -> use_mt := Some s),
  "<cmd> Choose an alternative manifest tool to use";

  "-x64", Arg.Unit (fun () -> machine := `x64; underscore := false; toolchain := `MSVC64),
  " (Deprecated)";

  "-defaultlib", Arg.String (fun s -> exts := s :: !exts),
  "<obj> External object (no export, no import)";

  "-save-temps", Arg.Set save_temps,
  " Do not delete intermediate files";

  "-implib", Arg.Set implib,
  " Do not delete the generated import library";

  "-outdef", Arg.String (fun s -> deffile := Some s),
  " Produce a def file with exported symbols";

  "-v", Arg.Unit (fun () -> incr verbose),
  " Increment verbosity (can be repeated)";

  "-show-exports", Arg.Set show_exports,
  " Show exported symbols";

  "-show-imports", Arg.Set show_imports,
  " Show imported symbols";

  "-dry", Arg.Set dry_mode,
  " Show the linker command line, do not actually run it";

  "-dump", Arg.Unit (fun () -> mode := `DUMP),
  " Only dump the content of object files";

  "-patch", Arg.Unit (fun () -> mode := `PATCH),
  " Only patch the target image (to be used with -stack)";

  "-nocygpath", Arg.Unit (fun () -> use_cygpath := `No),
  " Do not use cygpath (default for msvc, mingw)";

  "-cygpath", Arg.Unit (fun () -> use_cygpath := `Yes),
  " Use cygpath (default for cygwin)";

  "-no-merge-manifest", Arg.Set no_merge_manifest,
  " Do not merge the manifest (takes precedence over -merge-manifest)";

  "-merge-manifest", Arg.Set merge_manifest,
  " Merge manifest to the dll or exe (if generated)";

  "-real-manifest", Arg.Set real_manifest,
  " Use the generated manifest (default behavior)";

  "-default-manifest", Arg.Clear real_manifest,
  " Use the default manifest (default.manifest/default_amd64.manifest)";

  "-export", Arg.String (fun s -> defexports := s :: !defexports),
  "<sym> Explicitly export a symbol";

  "-noreexport", Arg.Clear reexport_from_implibs,
  " Do not reexport symbols imported from import libraries";

  "-where", Arg.Unit
    (fun () ->
      print_endline (Filename.dirname Sys.executable_name);
      exit 0
    ),
  " Show the FlexDLL directory";

  "-nounderscore", Arg.Clear underscore,
  " Normal symbols are not prefixed with an underscore";

  "-nodefaultlibs", Arg.Clear use_default_libs,
  " Do not assume any default library";

  "-builtin", Arg.Set builtin_linker,
  " Use built-in linker to produce a dll";

  "-explain", Arg.Set explain,
  " Explain why library objects are linked";

  "-subsystem", Arg.Set_string subsystem,
  "<id> Set the subsystem (default: console)";

  "-custom-crt", Arg.Set custom_crt,
  " Use a custom CRT";

  "-stack", Arg.String (fun s -> try stack_reserve := Some (Int32.of_string s) with _ -> raise (Arg.Bad "integer argument expected for -stack")),
  "<int> Set the stack reserve in the resulting image";

  "-link", Arg.String (fun s -> extra_args := s :: !extra_args),
  "<option> Next argument is passed verbatim to the linker";

  "-g", Arg.Unit (fun () -> ()),
  " (Ignored)";

  "-D", Arg.String (fun _ -> ()),
  "<symbol> (Ignored)";

  "-U", Arg.String (fun _ -> ()),
  "<symbol> (Ignored)";

  "--", Arg.Rest (fun s -> extra_args := s :: !extra_args),
  " Following arguments are passed verbatim to the linker";

  "-version", Arg.Unit
    (fun () ->
      Printf.printf "FlexDLL version %s\nFlexDLL directory: %s\n"
                    Version.version
                    (Filename.dirname Sys.executable_name);
      exit 0
    ),
  " Print linker version and FlexDLL directory and exit";

  "-vnum", Arg.Unit (fun () -> print_endline Version.version; exit 0),
  " Print linker version number and exit";
]


let flexlinkflags =
  let s =
    try Sys.getenv "FLEXLINKFLAGS"
    with Not_found -> ""
  in
  let n = String.length s in
  let rec skip_ws i = if i < n && s.[i] = ' ' then skip_ws (i + 1) else i in
  let rec scan_quote i = if i = n then i else if s.[i] = '"' then i + 1 else scan_quote (i+1) in
  let rec scan_arg i =
    if i = n || s.[i] = ' ' then i
    else if s.[i] = '"' then scan_arg (scan_quote (i + 1))
    else scan_arg (i + 1) in
  let rec args i =
    let i = skip_ws i in
    if i = n then []
    else let j = scan_arg i in String.sub s i (j - i) :: args j
  in
  args 0

let parse_cmdline () =
  (* Split -lXXX, -LXXX and -IXXX options *)
  let tosplit = function
    | "-l" | "-L" | "-I" | "-D" | "-U" -> true
    | _ -> false
  in

  let rec tr = function
    | (("-defaultlib"|"-link") as d) :: x :: rest -> d :: x :: tr rest
    | "/link" :: x :: rest -> "-link" :: x :: tr rest
    | s :: rest when String.length s > 2 && tosplit (String.sub s 0 2) ->
        String.sub s 0 2 :: String.sub s 2 (String.length s - 2) :: tr rest
    | s :: rest when String.length s >= 5 && String.sub s 0 5 = "/link" ->
        "-link" :: String.sub s 5 (String.length s - 5) :: tr rest
    (* Convert gcc linker option prefix -Wl, to flexlink linker prefix -link *)
    | s :: rest when String.length s >= 6 && String.sub s 0 5 = "-Wl,-" ->
        let args =
          String.split_on_char ',' (String.sub s 4 (String.length s - 4))
        in
          List.fold_right (fun arg args -> "-link" :: arg :: args) args (tr rest)
    | "-arg" :: x :: rest ->
        tr (Array.to_list (Arg.read_arg x)) @ rest
    | "-arg0" :: x :: rest ->
        tr (Array.to_list (Arg.read_arg0 x)) @ rest
    | x :: rest when x <> "" && x.[0] = '-' ->
        begin
          try
            let i = String.index x ':' in
            String.sub x 0 i :: String.sub x (i + 1) (String.length x - i - 1)
            :: tr rest
          with Not_found ->
            x :: tr rest
        end
    | s::rest when String.length s  > 1 && s.[0] = '@' ->
        let ic = open_in (String.sub s 1 (String.length s - 1)) in
        let opts = ref [] in
        begin
          try
            while true do
              let fn = input_line ic in
              if fn <> "" then
                (* todo: better unquoting *)
                let fn =
                  if fn.[0] = '\"' && fn.[String.length fn - 1] = '\"'
                  then String.sub fn 1 (String.length fn - 2)
                  else fn
                in
                opts := fn :: !opts
            done
          with End_of_file -> ()
        end;
        close_in ic;
        tr (List.rev_append !opts rest)
    | x :: rest -> x :: tr rest
    | [] -> []
  in
  let args =
    match Array.to_list Sys.argv with
    | pgm :: args -> pgm :: tr (flexlinkflags @ args)
    | _ -> assert false
  in

  let add_file s =
       files := s :: !files
  in
  Arg.parse_argv (Array.of_list args) (Arg.align specs)
    add_file usage_msg;
  if !output_file = "" && !mode <> `DUMP then begin
    Printf.eprintf
      "Please specify an output file (-help to get some usage information)\n";
    exit 1
  end

let usym s = if !underscore then "_" ^ s else s
