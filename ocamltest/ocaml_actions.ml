(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Actions specific to the OCaml compilers *)

open Ocamltest_stdlib
open Actions
open A.Infix

let reference_variable = function
  | `Toplevel t -> Ocaml_toplevels.reference_variable t
  | `Compiler t -> Ocaml_compilers.reference_variable t
  | `Ocamldoc -> Ocaml_variables.ocamldoc_reference

let directory = function
  | `Compiler t -> Ocaml_compilers.directory t
  | `Ocamldoc -> "ocamldoc"
  | `Toplevel t -> Ocaml_toplevels.directory t

let output_variable = function
  | `Compiler t -> Ocaml_compilers.output_variable t
  | `Ocamldoc -> Ocaml_variables.ocamldoc_output
  | `Toplevel t -> Ocaml_toplevels.output_variable t

let family = function
  | `Compiler _ -> "compiler"
  | `Ocamldoc -> "doc"
  | `Toplevel _ -> "toplevel"

let ocamldoc_reference_file_suffix =
  let+ backend = A.safe_lookup Ocaml_variables.ocamldoc_backend in
  if backend = "" then
    ".reference"
  else
    "." ^ backend ^ ".reference"

let reference_file t prefix =
  match t with
  | `Toplevel t -> Ocaml_toplevels.reference_file t prefix
  | `Compiler t -> Ocaml_compilers.reference_file t prefix
  | `Ocamldoc ->
      let+ prefix = prefix
      and+ suffix = ocamldoc_reference_file_suffix in
      Filename.make_filename prefix (directory `Ocamldoc) ^ suffix

(* Extracting information from environment *)

let native_support = Ocamltest_config.arch <> "none"

let no_native_compilers =
  A.with_env (A.return (Eff.skip_with_reason "native compilers disabled"))

let native_action a =
  if native_support then a else (Actions.update a no_native_compilers)

let get_backend_value_from_env bytecode_var native_var target =
  let+ bytecode_var = A.safe_lookup bytecode_var
  and+ native_var = A.safe_lookup native_var in
  Ocaml_backends.make_backend_function bytecode_var native_var target

let modules =
  Actions_helpers.words_of_variable Ocaml_variables.modules

let plugins =
  Actions_helpers.words_of_variable Ocaml_variables.plugins

let directories =
  Actions_helpers.words_of_variable Ocaml_variables.directories

let directory_flags =
  let+ dirs = directories in
  String.concat " " (List.map (fun dir -> "-I " ^ dir) dirs)

let flags =
  let open A in
  safe_lookup Ocaml_variables.flags

let last_flags =
  A.safe_lookup Ocaml_variables.last_flags

let ocamllex_flags =
  A.safe_lookup Ocaml_variables.ocamllex_flags

let ocamlyacc_flags =
  A.safe_lookup Ocaml_variables.ocamlyacc_flags

let filelist variable extension =
  let+ value = A.safe_lookup variable in
  let filenames = String.words value in
  let add_extension filename = Filename.make_filename filename extension in
  List.map add_extension filenames

let libraries backend =
  let extension = Ocaml_backends.library_extension backend in
  filelist Ocaml_variables.libraries extension

let binary_modules backend =
  let extension = Ocaml_backends.module_extension backend in
  filelist Ocaml_variables.binary_modules extension

let backend_default_flags target =
  get_backend_value_from_env
    Ocaml_variables.ocamlc_default_flags
    Ocaml_variables.ocamlopt_default_flags target

let backend_flags target =
  get_backend_value_from_env
    Ocaml_variables.ocamlc_flags
    Ocaml_variables.ocamlopt_flags target

let env_setting env_reader default_setting =
  Printf.sprintf "%s=%s"
    env_reader.Clflags.env_var
    (env_reader.Clflags.print default_setting)

let default_ocaml_env = [|
  "TERM=dumb";
  env_setting Clflags.color_reader Misc.Color.default_setting;
  env_setting Clflags.error_style_reader Misc.Error_style.default_setting;
|]

type module_generator =
  {
    description : string;
    command : string;
    flags : string A.t;
    generated_compilation_units :
      string -> (string * Ocaml_filetypes.t) list
  }

let ocamllex =
  {
    description = "lexer";
    command = Ocaml_commands.ocamlrun_ocamllex;
    flags = ocamllex_flags;
    generated_compilation_units =
      fun lexer_name -> [(lexer_name, Ocaml_filetypes.Implementation)]
  }

let ocamlyacc =
  {
    description = "parser";
    command = Ocaml_files.ocamlyacc;
    flags = ocamlyacc_flags;
    generated_compilation_units =
      fun parser_name ->
        [
          (parser_name, Ocaml_filetypes.Interface);
          (parser_name, Ocaml_filetypes.Implementation)
        ]
  }

(* let generate_module generator output_variable input = *)
(*   let basename = fst input in *)
(*   let input_file = Ocaml_filetypes.make_filename input in *)
(*   (\* let what = *\) *)
(*   (\*   Printf.sprintf "Generating %s module from %s" *\) *)
(*   (\*   generator.description input_file *\) *)
(*   (\* in *\) *)
(*   (\* Printf.fprintf log "%s\n%!" what; *\) *)
(*   let+ cmdline = *)
(*     let+ flags = generator.flags in *)
(*     [ *)
(*       generator.command; *)
(*       flags; *)
(*       input_file *)
(*     ] *)
(*   and+ run_params = *)
(*     Actions_helpers.run_params *)
(*       ~environment:(A.return default_ocaml_env) *)
(*       ~stdin_variable: Ocaml_variables.compiler_stdin *)
(*       ~stdout_variable:output_variable *)
(*       ~stderr_variable:output_variable *)
(*       ~append:true () *)
(*   in *)
(*   Eff.run_cmd run_params cmdline *)
(*   (\* if exit_status = 0 *\) *)
(*   (\* then generator.generated_compilation_units basename *\) *)
(*   (\* else begin *\) *)
(*   (\*   (\\* let reason = *\\) *\) *)
(*   (\*   (\\*   (Actions_helpers.mkreason *\\) *\) *)
(*   (\*   (\\*     what (String.concat " " commandline) exit_status) in *\\) *\) *)
(*   (\*   (\\* Printf.fprintf log "%s\n%!" reason; *\\) *\) *)
(*   (\*   [] *\) *)
(*   (\* end *\) *)

let prepare_modules output_variable inputs =
  (* let open Ocaml_filetypes in *)
  let+ inputs = inputs
  and+ run_params =
    Actions_helpers.run_params
      ~environment:(A.return default_ocaml_env)
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:output_variable
      ~stderr_variable:output_variable
      ~append:true ()
  and+ ocamllex_flags = ocamllex.flags
  and+ ocamlyacc_flags = ocamlyacc.flags in
  let effects, inputs =
    List.split @@
    List.map (fun ((basename, input_type) as input) ->
        let gen g flags =
          let input_file = Ocaml_filetypes.make_filename input in
          let cmdline =
            [
              g.command;
              flags;
              input_file
            ]
          in
          Eff.run_cmd run_params cmdline,
          g.generated_compilation_units basename
        in
        match input_type with
        | Implementation | Interface | C | Obj ->
            Eff.seq [], [input]
        | Binary_interface ->
            Eff.seq [], [input]
        | Backend_specific _ ->
            Eff.seq [], [input]
        | C_minus_minus ->
            assert false
        | Lexer ->
            gen ocamllex ocamllex_flags
        | Grammar ->
            gen ocamlyacc ocamlyacc_flags
        | Text ->
            assert false
      ) inputs
  in
  Eff.seq effects, List.flatten inputs

let get_program_file backend =
  let+ testfile_basename = A.map Filename.chop_extension Actions_helpers.testfile
  and+ test_build_directory = Actions_helpers.test_build_directory in
  let program_filename =
    Filename.mkexe
      (Filename.make_filename
        testfile_basename (Ocaml_backends.executable_extension backend)) in
  Filename.make_path [test_build_directory; program_filename]

let is_c_file (_filename, filetype) = filetype=Ocaml_filetypes.C

let cmas_need_dynamic_loading directories libraries =
  let+ libraries = libraries
  and+ directories = directories in
  let loads_c_code library =
    let library = Misc.find_in_path directories library in
    let ic = open_in_bin library in
    try
      let len_magic_number = String.length Config.cma_magic_number in
      let magic_number = really_input_string ic len_magic_number in
      if magic_number = Config.cma_magic_number then
        let toc_pos = input_binary_int ic in
        seek_in ic toc_pos;
        let toc = (input_value ic : Cmo_format.library) in
        close_in ic;
        if toc.Cmo_format.lib_dllibs <> [] then Some (Ok ()) else None
      else
        raise End_of_file
    with
    | End_of_file | Sys_error _ ->
        close_in_noerr ic;
        Some (Error ("Corrupt or non-CMA file: " ^ library))
  in
  List.find_map loads_c_code libraries

let compile_program compiler =
  let target = Ocaml_compilers.target compiler in
  let program_variable = Ocaml_compilers.program_variable compiler in
  let output_variable = Ocaml_compilers.output_variable compiler in
  let effects_and_modules =
    let all_modules =
      A.map (List.map Ocaml_filetypes.filetype)
        (Actions_helpers.words_of_variable Ocaml_variables.all_modules)
    in
    prepare_modules output_variable all_modules
  in
  let modules = A.map snd effects_and_modules in
  let has_c_file = A.map (List.exists is_c_file) modules in
  let module_names =
    let+ binary_modules = binary_modules target
    and+ modules = modules in
    binary_modules @ List.map Ocaml_filetypes.make_filename modules
  in
  let libraries = libraries target in
  let cmas_need_dynamic_loading =
    if not Config.supports_shared_libraries &&
       target = Ocaml_backends.Bytecode then
      cmas_need_dynamic_loading directories libraries
    else
      A.return None
  in
  let bytecode_links_c_code =
    A.map (function Some (Ok ()) -> true | _ -> false)
      cmas_need_dynamic_loading
  in
  let+ has_c_file = has_c_file
  and+ effects = A.map fst effects_and_modules
  and+ program_file = A.safe_lookup program_variable
  and+ runtime_flags =
    Ocaml_flags.runtime_flags target
      (has_c_file ||+ bytecode_links_c_code)
  and+ compile_only =
    A.map (Option.value ~default:false)
      (A.lookup_as_bool Ocaml_variables.compile_only)
  and+ directory_flags = directory_flags
  and+ flags = flags
  and+ libraries = libraries
  and+ backend_default_flags = backend_default_flags target
  and+ backend_flags = backend_flags target
  and+ last_flags = last_flags
  and+ filetype_flag = A.safe_lookup Ocaml_variables.ocaml_filetype_flag
  and+ module_names = module_names
  and+ run_params =
    let expected_exit_status =
      Actions_helpers.int_of_variable
        (Ocaml_compilers.exit_status_variable compiler)
    in
    Actions_helpers.run_params
      ~environment:(A.return default_ocaml_env)
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:(Ocaml_compilers.output_variable compiler)
      ~stderr_variable:(Ocaml_compilers.output_variable compiler)
      ~append:true
      ~expected_exit_codes:[expected_exit_status] ()
  and+ cmas_need_dynamic_loading = cmas_need_dynamic_loading in
  let what =
    let module_names = String.concat " " module_names in
    Printf.sprintf "Compiling program %s from modules %s"
      (relative_to_initial_cwd program_file) module_names
  in
  Eff.seq
    [
      Eff.echo "%s" what;
      effects;
      (match cmas_need_dynamic_loading with
      | Some (Error reason) ->
          Eff.fail_with_reason reason
      | _ ->
          let c_headers_flags = if has_c_file then Ocaml_flags.c_includes else "" in
          let compile_flags = if compile_only then " -c " else "" in
          let output = if compile_only then "" else "-o " ^ program_file in
          let commandline =
            Ocaml_compilers.name compiler ::
            runtime_flags ::
            c_headers_flags ::
            Ocaml_flags.stdlib ::
            directory_flags ::
            flags ::
            libraries @
            backend_default_flags ::
            backend_flags ::
            compile_flags ::
            output ::
            filetype_flag ::
            module_names @
            last_flags :: []
          in
          Eff.run_cmd run_params commandline)
    ]
(* if exit_status = expected_exit_status *)
(* then Result.pass *)
(* else *)
(*   let what = *)
(*     let module_names = String.concat " " module_names in *)
(*     Printf.sprintf "Compiling program %s from modules %s" *)
(*       (relative_to_initial_cwd program_file) module_names *)
(*   in *)
  (* Printf.fprintf log "%s\n%!" what; *)
(*   let reason = *)
(*     (Actions_helpers.mkreason *)
(*        what (String.concat " " commandline) exit_status) *)
(*   in *)
(*   Result.fail_with_reason reason *)

let compile_module compiler module_ =
  let target = Ocaml_compilers.target compiler in
  let+ module_filetype = A.map Ocaml_filetypes.filetype module_
  and+ directory_flags = directory_flags
  and+ libraries = libraries target
  and+ backend_default_flags = backend_default_flags target
  and+ backend_flags = backend_flags target
  and+ flags = flags
  and+ module_ = module_
  and+ run_params =
    let expected_exit_status =
      Actions_helpers.int_of_variable (Ocaml_compilers.exit_status_variable compiler)
    in
    Actions_helpers.run_params
      ~environment:(A.return default_ocaml_env)
      ~stdin_variable:Ocaml_variables.compiler_stdin
      ~stdout_variable:(Ocaml_compilers.output_variable compiler)
      ~stderr_variable:(Ocaml_compilers.output_variable compiler)
      ~append:true
      ~expected_exit_codes:[expected_exit_status] ()
  in
  let c_headers_flags =
    if is_c_file module_filetype then Ocaml_flags.c_includes else ""
  in
  let commandline =
    Ocaml_compilers.name compiler ::
    Ocaml_flags.stdlib ::
    c_headers_flags ::
    directory_flags ::
    flags ::
    libraries @
    backend_default_flags ::
    backend_flags ::
    "-c" ::
    module_ :: []
  in
  Eff.run_cmd run_params commandline
  (* if exit_status = expected_exit_status *)
  (* then Result.pass *)
  (* else *)
  (*   let reason = *)
  (*     let what = Printf.sprintf "Compiling module %s" module_ in *)
  (*     Actions_helpers.mkreason *)
  (*       what (String.concat " " commandline) exit_status *)
  (*   in *)
  (*   Result.fail_with_reason reason *)

let module_has_interface directory module_name =
  let interface_name =
    Ocaml_filetypes.make_filename (module_name, Ocaml_filetypes.Interface) in
  let interface_fullpath = Filename.make_path [directory;interface_name] in
  Sys.file_exists interface_fullpath

let add_module_interface directory module_description =
  match module_description with
    | (filename, Ocaml_filetypes.Implementation) when
      module_has_interface directory filename ->
        [(filename, Ocaml_filetypes.Interface); module_description]
  | _ -> [module_description]

let _print_module_names description modules =
  Eff.echo "%s modules: %s\n%!"
    description
    (String.concat " " (List.map Ocaml_filetypes.make_filename modules))

let find_source_modules =
  (* print_module_names log "Specified" specified_modules; *)
  let source_modules =
    let+ source_directory = Actions_helpers.test_source_directory
    and+ specified_modules =
      let+ plugins = plugins
      and+ modules = modules
      and+ testfile = Actions_helpers.testfile in
      List.map Ocaml_filetypes.filetype (plugins @ modules @ [testfile])
    in
    List.concatmap
      (add_module_interface source_directory)
      specified_modules
  in
  (* print_module_names log "Source" source_modules; *)
  A.add
    Ocaml_variables.all_modules
    (A.map (fun l -> String.concat " " (List.map Ocaml_filetypes.make_filename l)) source_modules)
    (let+ (), env = A.with_env (A.return ()) in
     env)

let setup_tool_build_env tool x =
  let source_directory = Actions_helpers.test_source_directory in
  let testfile = Actions_helpers.testfile in
  let testfile_basename = A.map Filename.chop_extension testfile in
  let tool_reference_variable = reference_variable tool in
  let tool_reference_prefix =
    let+ source_dir = source_directory
    and+ testfile_basename = testfile_basename in
    Filename.make_path [source_dir; testfile_basename]
  in
  let tool_reference_file = reference_file tool tool_reference_prefix in
  let source_modules =
    Actions_helpers.words_of_variable Ocaml_variables.all_modules in
  let build_dir =
    let+ build_dir_prefix = A.safe_lookup Builtin_variables.test_build_directory_prefix
    and+ tool_dir_suffix = A.safe_lookup Ocaml_variables.compiler_directory_suffix in
    Filename.concat build_dir_prefix (directory tool ^ tool_dir_suffix)
  in
  let tool_output_variable = output_variable tool in
  let tool_output_file =
    let tool_output_filename = Filename.make_filename (directory tool) "output" in
    let+ build_dir = build_dir in
    Filename.make_path [build_dir; tool_output_filename]
  in
  A.add_if_undefined tool_reference_variable tool_reference_file
    (A.add_if_undefined tool_output_variable tool_output_file
       (A.add Builtin_variables.test_build_directory build_dir
          (let+ remove_output_file = A.map Eff.force_remove tool_output_file
           and+ setup_build_env = Actions_helpers.setup_build_env false source_modules
           and+ env = x in
           Eff.seq [remove_output_file; setup_build_env], env)))

let setup_compiler_build_env compiler =
  setup_tool_build_env (`Compiler compiler)
    (let prog_var = Ocaml_compilers.program_variable compiler in
     let prog_output_var = Ocaml_compilers.program_output_variable compiler in
     let prog_output_file =
       let+ prog_file = A.safe_lookup prog_var in
       prog_file ^ ".output"
     in
     let default_prog_file = get_program_file (Ocaml_compilers.target compiler) in
     A.add_if_undefined prog_var default_prog_file
       (match prog_output_var with
        | None -> let+ (), env = A.with_env (A.return ()) in env
        | Some outputvar ->
            A.add_if_undefined outputvar
              prog_output_file (let+ (), env = A.with_env (A.return ()) in env))
    )

let mk_compiler_env_setup name compiler =
  Actions.make name
    (setup_compiler_build_env compiler)

let mk_toplevel_env_setup name toplevel =
  Actions.make name (setup_tool_build_env toplevel
                       (let+ (), env = A.with_env (A.return ()) in env))

let setup_ocamlc_byte_build_env =
  mk_compiler_env_setup
    "setup-ocamlc.byte-build-env"
    Ocaml_compilers.ocamlc_byte

let setup_ocamlc_opt_build_env =
  native_action
    (mk_compiler_env_setup
      "setup-ocamlc.opt-build-env"
      Ocaml_compilers.ocamlc_opt)

let setup_ocamlopt_byte_build_env =
  native_action
    (mk_compiler_env_setup
      "setup-ocamlopt.byte-build-env"
      Ocaml_compilers.ocamlopt_byte)

let setup_ocamlopt_opt_build_env =
  native_action
    (mk_compiler_env_setup
      "setup-ocamlopt.opt-build-env"
      Ocaml_compilers.ocamlopt_opt)

let setup_ocaml_build_env =
  mk_toplevel_env_setup
    "setup-ocaml-build-env"
    (`Toplevel Ocaml_toplevels.ocaml)

let setup_ocamlnat_build_env =
  native_action
    (mk_toplevel_env_setup
      "setup-ocamlnat-build-env"
      (`Toplevel Ocaml_toplevels.ocamlnat))

let compile compiler =
  let commandline = A.lookup_nonempty Builtin_variables.commandline in
  A.if_ (A.map Option.is_none commandline)
    (let module_ = A.lookup_nonempty Ocaml_variables.module_ in
     A.if_ (A.map Option.is_none module_)
       (compile_program compiler)
       (compile_module compiler (A.map Option.get module_)))
    (let+ run_params =
       let expected_exit_status =
         Actions_helpers.int_of_variable
           (Ocaml_compilers.exit_status_variable compiler)
       in
       Actions_helpers.run_params
         ~environment:(A.return default_ocaml_env)
         ~stdin_variable: Ocaml_variables.compiler_stdin
         ~stdout_variable:(Ocaml_compilers.output_variable compiler)
         ~stderr_variable:(Ocaml_compilers.output_variable compiler)
         ~append:true
         ~expected_exit_codes:[expected_exit_status] ()
     and+ commandline = A.map Option.get commandline in
     let what = Printf.sprintf "Compiling using commandline %s" commandline in
     let commandline = [Ocaml_compilers.name compiler; commandline] in
     Eff.seq
       [
         Eff.echo "%s" what;
         Eff.run_cmd run_params commandline;
       ]
     (* if exit_status = expected_exit_status *)
     (* then Result.pass *)
     (* else *)
     (*   let reason = *)
     (*     (Actions_helpers.mkreason *)
     (*        what (String.concat " " commandline) exit_status) in *)
     (*   Result.fail_with_reason reason *)
   )

let compile compiler =
  A.with_env (compile compiler)

(* Compile actions *)

let ocamlc_byte =
  Actions.make
    "ocamlc.byte"
    (compile Ocaml_compilers.ocamlc_byte)

let ocamlc_opt =
  native_action
    (Actions.make
      "ocamlc.opt"
      (compile Ocaml_compilers.ocamlc_opt))

let ocamlopt_byte =
  native_action
    (Actions.make
      "ocamlopt.byte"
      (compile Ocaml_compilers.ocamlopt_byte))

let ocamlopt_opt =
  native_action
    (Actions.make
      "ocamlopt.opt"
      (compile Ocaml_compilers.ocamlopt_opt))

let env_with_lib_unix x =
  let newlibs =
    let+ ld_lib_path = A.lookup Ocaml_variables.caml_ld_library_path in
    match ld_lib_path with
    | None -> Ocaml_directories.libunix
    | Some libs -> Ocaml_directories.libunix ^ " " ^ libs
  in
  A.add Ocaml_variables.caml_ld_library_path newlibs x

let debug =
  let+ program = A.safe_lookup Builtin_variables.program
  and+ run_params =
    env_with_lib_unix
      (Actions_helpers.run_params
         ~environment:(A.return default_ocaml_env)
         ~stdin_variable: Ocaml_variables.ocamldebug_script
         ~stdout_variable:Builtin_variables.output
         ~stderr_variable:Builtin_variables.output
         ~append:true ())
  in
  let commandline =
    [
      Ocaml_commands.ocamlrun_ocamldebug;
      Ocaml_flags.ocamldebug_default_flags;
      program
    ]
  in
  let what = Printf.sprintf "Debugging program %s" program in
  Eff.seq
    [ Eff.echo "%s" what;
      Eff.run_cmd run_params commandline ]
  (* if exit_status = 0 *)
  (* then Result.pass *)
  (* else begin *)
  (*   let reason = *)
  (*     (Actions_helpers.mkreason *)
  (*       what (String.concat " " commandline) exit_status) in *)
  (*   Result.fail_with_reason reason *)
  (* end *)

let ocamldebug = Actions.make "ocamldebug" (A.with_env debug)

let objinfo =
  let+ program = A.safe_lookup Builtin_variables.program
  and+ run_params =
    let tools_directory = Ocaml_directories.tools in
    let ocamllib = [| Printf.sprintf "OCAMLLIB=%s" tools_directory |] in
    env_with_lib_unix
      (let environment =
         Array.concat
           [
             default_ocaml_env;
             ocamllib;
           ]
       in
       Actions_helpers.run_params
         ~environment:(A.return environment)
         ~stdout_variable:Builtin_variables.output
         ~stderr_variable:Builtin_variables.output
         ~append:true ())
  in
  let commandline =
    [
      Ocaml_commands.ocamlrun_ocamlobjinfo;
      Ocaml_flags.ocamlobjinfo_default_flags;
      program
    ]
  in
  let what = Printf.sprintf "Running ocamlobjinfo on %s" program in
  Eff.seq
    [ Eff.echo "%s" what;
      Eff.run_cmd run_params commandline ]
  (* if exit_status = 0 *)
  (* then Result.pass *)
  (* else begin *)
  (*   let reason = *)
  (*     (Actions_helpers.mkreason *)
  (*       what (String.concat " " commandline) exit_status) in *)
  (*   Result.fail_with_reason reason *)
  (* end *)

let ocamlobjinfo = Actions.make "ocamlobjinfo" (A.with_env objinfo)

let mklib =
  let+ program = A.safe_lookup Builtin_variables.program
  and+ modules = modules
  and+ run_params =
    Actions_helpers.run_params
      ~environment:(A.return default_ocaml_env)
      ~stdout_variable:Ocaml_variables.compiler_output
      ~stderr_variable:Ocaml_variables.compiler_output
      ~append:true ()
  in
  let commandline =
    let ocamlc_command =
      String.concat " "
        [
          Ocaml_commands.ocamlrun_ocamlc;
          Ocaml_flags.stdlib;
        ]
    in
    Ocaml_commands.ocamlrun_ocamlmklib ::
    ("-ocamlc '" ^ ocamlc_command ^ "'") ::
    ("-o " ^ program) ::
    modules
  in
  let what = Printf.sprintf "Running ocamlmklib to produce %s" program in
  Eff.seq
    [ Eff.echo "%s" what;
      Eff.run_cmd run_params commandline ]
  (* if exit_status = 0 *)
  (* then Result.pass *)
  (* else begin *)
  (*   let reason = *)
  (*     (Actions_helpers.mkreason *)
  (*       what (String.concat " " commandline) exit_status) in *)
  (*   Result.fail_with_reason reason *)
  (* end *)

let ocamlmklib = Actions.make "ocamlmklib" (A.with_env mklib)

let finalise_codegen_cc test_basename =
  let test_module =
    let+ test_basename = test_basename in
    Filename.make_filename test_basename "s"
  in
  let modules =
    let+ test_module = test_module in
    test_module ^ " " ^ Ocaml_files.asmgen_archmod
  in
  let program =
    let+ test_basename = test_basename in
    Filename.make_filename test_basename "out"
  in
  A.add Ocaml_variables.modules modules
    (A.add Builtin_variables.program program
       (A.with_env (A.return Eff.pass)))

let finalise_codegen_msvc test_basename =
  let obj = A.map (fun s -> Filename.make_filename s Ocamltest_config.objext) test_basename in
  let src = A.map (fun s -> Filename.make_filename s "s") test_basename in
  let modules =
    let+ obj = obj in
    obj ^ " " ^ Ocaml_files.asmgen_archmod
  in
  let program =
    let+ test_basename = test_basename in
    Filename.make_filename test_basename "out"
  in
  A.add Ocaml_variables.modules modules
    (A.add Builtin_variables.program program
       (A.with_env
          (let+ run_params =
             Actions_helpers.run_params
               ~environment:(A.return default_ocaml_env)
               ~stdout_variable:Ocaml_variables.compiler_output
               ~stderr_variable:Ocaml_variables.compiler_output
               ~append:true ()
           and+ obj = obj
           and+ src = src in
           let what = "Running Microsoft assembler" in
           let commandline = [Ocamltest_config.asm; obj; src] in
           Eff.seq
             [ Eff.echo "%s" what;
               Eff.run_cmd run_params commandline ])))

  (* A.if_ (A.map ((=) 0) exit_status) *)
  (*   ((\* let env = *\) *)
  (*    (\*   Environments.add_bindings *\) *)
  (*    (\*     [ *\) *)
  (*    (\*       Ocaml_variables.modules, modules; *\) *)
  (*    (\*       Builtin_variables.program, program; *\) *)
  (*    (\*     ] env *\) *)
  (*    (\* in *\) *)
  (*    A.add Ocaml_variables.modules modules *)
  (*      (A.add Builtin_variables.program program *)
  (*         (A.with_env (A.return Result.pass)))) *)
  (*   ((\* let reason = *\) *)
  (*     (\*   (Actions_helpers.mkreason *\) *)
  (*     (\*     what (String.concat " " commandline) exit_status) in *\) *)
  (*     A.with_env (A.return (Result.fail_with_reason ""))) *)

let run_codegen =
  let testfile_basename = A.map Filename.chop_extension Actions_helpers.testfile in
  let compiler_output =
    let+ test_build_directory = Actions_helpers.test_build_directory in
    Filename.make_path [test_build_directory; "compiler-output"]
  in
  let output =
    let+ testfile_basename = testfile_basename
    and+ test_build_directory = Actions_helpers.test_build_directory in
    let output_file = Filename.make_filename testfile_basename "output" in
    Filename.make_path [test_build_directory; output_file]
  in
  A.add_if_undefined Ocaml_variables.compiler_output compiler_output
    (A.add Builtin_variables.output output
       (let+ eff =
          let+ testfile = Actions_helpers.testfile
          and+ flags = flags
          and+ run_params =
            Actions_helpers.run_params
              ~environment:(A.return default_ocaml_env)
              ~stdout_variable:Ocaml_variables.compiler_output
              ~stderr_variable:Ocaml_variables.compiler_output
              ~append:true ()
          in
          let commandline =
            [
              Ocaml_commands.ocamlrun_codegen;
              flags;
              "-S " ^ testfile
            ]
          in
          Eff.run_cmd run_params commandline
        and+ finalise, env =
          if Ocamltest_config.ccomptype="msvc"
          then finalise_codegen_msvc testfile_basename
          else finalise_codegen_cc testfile_basename
        and+ testfile = Actions_helpers.testfile in
        let what = Printf.sprintf "Running codegen on %s" testfile in
        Eff.seq
          [ Eff.echo "%s" what;
            Eff.if_pass eff finalise ],
        env))
(* ((\* let reason = *\) *)
          (*   (\*   (Actions_helpers.mkreason *\) *)
          (*   (\*     what (String.concat " " commandline) exit_status) in *\) *)
          (*   A.with_env (A.return (Result.fail_with_reason ""))))) *)

let codegen = Actions.make "codegen" run_codegen

let run_cc =
  let+ program = A.safe_lookup Builtin_variables.program
  and+ arguments = A.safe_lookup Builtin_variables.arguments
  and+ modules = modules
  and+ run_params =
    Actions_helpers.run_params
      ~environment:(A.return default_ocaml_env)
      ~stdout_variable:Ocaml_variables.compiler_output
      ~stderr_variable:Ocaml_variables.compiler_output
      ~append:true ()
  in
  let output_exe =
    if Ocamltest_config.ccomptype="msvc" then "/Fe" else "-o "
  in
  let commandline =
    Ocamltest_config.cc ::
    Ocamltest_config.cflags ::
    ("-I" ^ Ocaml_directories.runtime) ::
    (output_exe ^ program) ::
    arguments ::
    modules
  in
  let what = Printf.sprintf "Running C compiler to build %s" program in
  Eff.seq
    [ Eff.echo "%s" what;
      Eff.run_cmd run_params commandline ]
  (* if exit_status = 0 *)
  (* then Result.pass *)
  (* else begin *)
  (*   let reason = *)
  (*     (Actions_helpers.mkreason *)
  (*       what (String.concat " " commandline) exit_status) in *)
  (*   Result.fail_with_reason reason *)
  (* end *)

let cc = Actions.make "cc" (A.with_env run_cc)

let run_expect_once input_file principal =
  let expect_flags = Sys.safe_getenv "EXPECT_FLAGS" in
  let repo_root = "-repo-root " ^ Ocaml_directories.srcdir in
  let principal_flag = if principal then "-principal" else "" in
  let+ flags = flags
  and+ input_file = input_file
  and+ run_params =
    Actions_helpers.run_params
      ~environment:(A.return default_ocaml_env) ()
  in
  let commandline =
    [
      Ocaml_commands.ocamlrun_expect_test;
      expect_flags;
      flags;
      repo_root;
      principal_flag;
      input_file
    ]
  in
  Eff.run_cmd run_params commandline
  (* if exit_status = 0 then Result.pass *)
  (* else begin *)
  (*   let reason = (Actions_helpers.mkreason *)
  (*                   "expect" (String.concat " " commandline) exit_status) in *)
  (*   Result.fail_with_reason reason *)
  (* end *)

let run_expect_twice input_file =
  let corrected filename =
    let+ filename = filename in
    Filename.make_filename filename "corrected"
  in
  let intermediate_file = corrected input_file in
  let output_file = corrected intermediate_file in
  A.add Builtin_variables.reference input_file
    (A.add Builtin_variables.output output_file
       (A.with_env
          (let+ input_run = run_expect_once input_file false
           and+ intermediate_run = run_expect_once intermediate_file true in
           Eff.if_pass input_run intermediate_run)))

let run_expect =
  run_expect_twice Actions_helpers.testfile

let run_expect = Actions.make "run-expect" run_expect

let make_check_tool_output name tool =
  Actions.make name
    (A.with_env
       (Actions_helpers.check_output
          (family tool)
          (output_variable tool)
          (reference_variable tool)))

let check_ocamlc_byte_output =
  make_check_tool_output
    "check-ocamlc.byte-output"
    (`Compiler Ocaml_compilers.ocamlc_byte)

let check_ocamlc_opt_output =
  native_action
    (make_check_tool_output
       "check-ocamlc.opt-output"
       (`Compiler Ocaml_compilers.ocamlc_opt))

let check_ocamlopt_byte_output =
  native_action
    (make_check_tool_output
       "check-ocamlopt.byte-output"
       (`Compiler Ocaml_compilers.ocamlopt_byte))

let check_ocamlopt_opt_output =
  native_action
    (make_check_tool_output
       "check-ocamlopt.opt-output"
       (`Compiler Ocaml_compilers.ocamlopt_opt))

let really_compare_programs backend comparison_tool =
  let+ program = A.safe_lookup Builtin_variables.program
  and+ program2 = A.safe_lookup Builtin_variables.program2 in
  let files =
    { Filecompare.filetype = Filecompare.Binary;
      Filecompare.reference_filename = program;
      Filecompare.output_filename = program2 }
  in
  if Ocamltest_config.flambda && backend = Ocaml_backends.Native
  then begin
    let reason =
      "flambda temporarily disables comparison of native programs" in
    Eff.pass_with_reason reason
  end else
  if backend = Ocaml_backends.Native && (Sys.win32 || Sys.cygwin)
  then begin
    let reason =
      "comparison of native programs temporarily disabled under Windows" in
    Eff.pass_with_reason reason
  end else begin
    let tool =
      if backend = Ocaml_backends.Native && (Sys.win32 || Sys.cygwin) then
        let bytes_to_ignore = 512 (* comparison_start_address program *) in
        Filecompare.(make_cmp_tool ~ignore:{bytes=bytes_to_ignore; lines=0})
      else
        comparison_tool
    in
    let what = Printf.sprintf "Comparing %s programs %s and %s"
        (Ocaml_backends.string_of_backend backend)
        (relative_to_initial_cwd program)
        (relative_to_initial_cwd program2)
    in
    Eff.seq
      [ Eff.echo "%s" what;
        Eff.compare_files ~tool files ]
  end

let compare_programs backend comparison_tool =
  let compare_programs =
    A.lookup_as_bool Ocaml_variables.compare_programs in
  A.if_ (A.map ((=) (Some false)) compare_programs)
    (let reason = "program comparison disabled" in
     A.return (Eff.pass_with_reason reason))
    (really_compare_programs backend comparison_tool)

let make_bytecode_programs_comparison_tool =
  let ocamlrun = Ocaml_files.ocamlrun in
  let cmpbyt = Ocaml_files.cmpbyt in
  let tool_name = ocamlrun ^ " " ^ cmpbyt in
  Filecompare.make_comparison_tool tool_name ""

let native_programs_comparison_tool = Filecompare.default_comparison_tool

let compare_bytecode_programs_code =
  let bytecode_programs_comparison_tool =
    make_bytecode_programs_comparison_tool in
  compare_programs
    Ocaml_backends.Bytecode bytecode_programs_comparison_tool

let compare_bytecode_programs =
  native_action
    (Actions.make
      "compare-bytecode-programs"
      (A.with_env compare_bytecode_programs_code))

let compare_native_programs =
  native_action
    (Actions.make
      "compare-native-programs"
      (A.with_env (compare_programs Ocaml_backends.Native native_programs_comparison_tool)))

let compile_modules compiler compilername compileroutput
    module_basenames_filetypes =
    (* (module_basename, module_filetype) = *)
  let backend = Ocaml_compilers.target compiler in
  let+ filenames =
    A.map (List.map Ocaml_filetypes.make_filename) module_basenames_filetypes
  (* Ocaml_filetypes.make_filename (module_basename, module_filetype) in *)
  and+ module_basenames_filetypes = module_basenames_filetypes
  and+ run_params =
    let expected_exit_status =
      Actions_helpers.int_of_variable
        (Ocaml_compilers.exit_status_variable compiler) in
    Actions_helpers.run_params
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:compileroutput
      ~stderr_variable:compileroutput
      ~append:true
      ~expected_exit_codes:[expected_exit_status] ()
  and+ flags = flags
  and+ backend_flags = backend_flags backend in
  let compile_commandline input_file output_file optional_flags =
    let compile = "-c " ^ input_file in
    let output =
      match output_file with
      | None -> ""
      | Some file -> "-o " ^ file
    in
    compilername ::
    Ocaml_flags.stdlib ::
    flags ::
    backend_flags ::
    optional_flags ::
    compile ::
    output :: []
  in
  let exec commandline =
    (* Printf.fprintf log "%s\n%!" what; *)
    Eff.run_cmd run_params commandline
    (* if exit_status = expected_exit_status *)
    (* then Result.pass *)
    (* else *)
    (*   let what = Printf.sprintf "%s for file %s (expected exit status: %d)" *)
    (*       (Ocaml_filetypes.action_of_filetype module_filetype) filename *)
    (*       expected_exit_status in *)
    (*   let reason = *)
    (*     (Actions_helpers.mkreason *)
    (*        what (String.concat " " commandline) exit_status) in *)
    (*   Result.fail_with_reason reason *)
  in
  (* end in *)
  Eff.seq @@
  List.map2 (fun (module_basename, module_filetype) filename ->
      match module_filetype with
      | Ocaml_filetypes.Interface ->
          let interface_name =
            Ocaml_filetypes.make_filename
              (module_basename, Ocaml_filetypes.Interface)
          in
          exec (compile_commandline interface_name None "")
      | Ocaml_filetypes.Implementation ->
          let module_extension = Ocaml_backends.module_extension backend in
          let module_output_name =
            Filename.make_filename module_basename module_extension
          in
          exec (compile_commandline filename (Some module_output_name) "")
      | Ocaml_filetypes.C ->
          let object_extension = Config.ext_obj in
          let _object_filename = module_basename ^ object_extension in
          exec (compile_commandline filename None Ocaml_flags.c_includes)
      | _ ->
          let reason = Printf.sprintf "File %s of type %s not supported yet"
              filename (Ocaml_filetypes.string_of_filetype module_filetype) in
          Eff.fail_with_reason reason
    ) module_basenames_filetypes filenames

let run_test_program_in_toplevel toplevel =
  let backend = Ocaml_toplevels.backend toplevel in
  let compiler = Ocaml_toplevels.compiler toplevel in
  let compiler_name = Ocaml_compilers.name compiler in
  let compiler_output_variable = Ocaml_toplevels.output_variable toplevel in
  let libraries = libraries backend in
  (* This is a sub-optimal check - skip the test if any libraries requiring
     C stubs are loaded. It would be better at this point to build a custom
     toplevel. *)
  let toplevel_can_run =
    Config.supports_shared_libraries || backend <> Ocaml_backends.Bytecode
  in
  let ocaml_script_as_argument =
    A.map (Option.value ~default:false)
      (A.lookup_as_bool
         Ocaml_variables.ocaml_script_as_argument)
  in
  let+ cmas_need_dynamic_loading = cmas_need_dynamic_loading directories libraries
  and+ compile_modules =
    let modules_with_filetypes = A.map (List.map Ocaml_filetypes.filetype) modules in
    compile_modules
      compiler compiler_name compiler_output_variable
      modules_with_filetypes
  and+ ocaml_script_as_argument = ocaml_script_as_argument
  and+ directory_flags = directory_flags
  and+ flags = flags
  and+ binary_modules = binary_modules backend
  and+ arguments = A.safe_lookup Builtin_variables.arguments
  and+ libraries = libraries
  and+ testfile = Actions_helpers.testfile
  and+ run_params =
    let expected_exit_status =
      Actions_helpers.int_of_variable
        (Ocaml_toplevels.exit_status_variable toplevel)
    in
    A.if_ ocaml_script_as_argument
      (Actions_helpers.run_params
         ~environment:(A.return default_ocaml_env)
         ~stdout_variable:compiler_output_variable
         ~stderr_variable:compiler_output_variable
         ~expected_exit_codes:[expected_exit_status] ())
      (Actions_helpers.run_params
         ~environment:(A.return default_ocaml_env)
         ~stdin_variable:Builtin_variables.test_file
         ~stdout_variable:compiler_output_variable
         ~stderr_variable:compiler_output_variable
         ~expected_exit_codes:[expected_exit_status] ())
  in
  if not toplevel_can_run then
    Eff.skip
  else begin
    match cmas_need_dynamic_loading with
    | Some (Error reason) ->
        Eff.fail_with_reason reason
    | Some (Ok ()) ->
        Eff.skip
    | _ ->
        let toplevel_name = Ocaml_toplevels.name toplevel in
        (* let what = *)
        (*   Printf.sprintf "Running %s in %s toplevel \ *)
        (*                   (expected exit status: %d)" *)
        (*     testfile *)
        (*     (Ocaml_backends.string_of_backend backend) *)
        (*     expected_exit_status in *)
        (* Printf.fprintf log "%s\n%!" what; *)
        let commandline =
          let script_arg =
            if ocaml_script_as_argument then testfile else ""
          in
          toplevel_name ::
          Ocaml_flags.toplevel_default_flags ::
          (match Ocaml_toplevels.backend toplevel with
           | Ocaml_backends.Native -> "-S"
           | _ -> "") ::
          Ocaml_flags.stdlib ::
          directory_flags ::
          Ocaml_flags.include_toplevel_directory ::
          flags ::
          libraries @
          binary_modules @
          script_arg ::
          arguments :: []
        in
        Eff.if_pass compile_modules
          (Eff.run_cmd run_params commandline)
           (* if exit_status = expected_exit_status *)
           (* then Result.pass *)
           (* else begin *)
           (*   let reason = *)
           (*     (Actions_helpers.mkreason *)
           (*        what (String.concat " " commandline) exit_status) in *)
           (*   Result.fail_with_reason reason *)
  end
(* (assert false) *)
(* end else (result, env) *)

let ocaml = Actions.make
  "ocaml"
  (A.with_env (run_test_program_in_toplevel Ocaml_toplevels.ocaml))

let ocamlnat =
  native_action
    (Actions.make
      "ocamlnat"
      (A.with_env (run_test_program_in_toplevel Ocaml_toplevels.ocamlnat)))

let check_ocaml_output =
  make_check_tool_output
    "check-ocaml-output"
    (`Toplevel Ocaml_toplevels.ocaml)

let check_ocamlnat_output =
  native_action
    (make_check_tool_output
       "check-ocamlnat-output"
       (`Toplevel Ocaml_toplevels.ocamlnat))

let config_variables _log env =
  Environments.add_bindings
  [
    Ocaml_variables.arch, Ocamltest_config.arch;
    Ocaml_variables.ocamlrun, Ocaml_files.ocamlrun;
    Ocaml_variables.ocamlc_byte, Ocaml_files.ocamlc;
    Ocaml_variables.ocamlopt_byte, Ocaml_files.ocamlopt;
    Ocaml_variables.bytecc_libs, Ocamltest_config.bytecc_libs;
    Ocaml_variables.nativecc_libs, Ocamltest_config.nativecc_libs;
    Ocaml_variables.mkdll,
      Sys.getenv_with_default_value "MKDLL" Ocamltest_config.mkdll;
    Ocaml_variables.mkexe, Ocamltest_config.mkexe;
    Ocaml_variables.c_preprocessor, Ocamltest_config.c_preprocessor;
    Ocaml_variables.csc, Ocamltest_config.csc;
    Ocaml_variables.csc_flags, Ocamltest_config.csc_flags;
    Ocaml_variables.shared_library_cflags,
      Ocamltest_config.shared_library_cflags;
    Ocaml_variables.objext, Ocamltest_config.objext;
    Ocaml_variables.asmext, Ocamltest_config.asmext;
    Ocaml_variables.sharedobjext, Ocamltest_config.sharedobjext;
    Ocaml_variables.ocamlc_default_flags,
      Ocamltest_config.ocamlc_default_flags;
    Ocaml_variables.ocamlopt_default_flags,
      Ocamltest_config.ocamlopt_default_flags;
    Ocaml_variables.ocamlrunparam, Sys.safe_getenv "OCAMLRUNPARAM";
    Ocaml_variables.ocamlsrcdir, Ocaml_directories.srcdir;
    Ocaml_variables.os_type, Sys.os_type;
  ] env

let pass_or_skip b s1 s2 =
  A.with_env (Actions_helpers.pass_or_skip b s1 s2)

let flat_float_array = Actions.make
  "flat-float-array"
  (pass_or_skip Ocamltest_config.flat_float_array
    "compiler configured with -flat-float-array"
    "compiler configured with -no-flat-float-array")

let no_flat_float_array = make
  "no-flat-float-array"
  (pass_or_skip (not Ocamltest_config.flat_float_array)
    "compiler configured with -no-flat-float-array"
    "compiler configured with -flat-float-array")

let flambda = Actions.make
  "flambda"
  (pass_or_skip Ocamltest_config.flambda
    "support for flambda enabled"
    "support for flambda disabled")

let no_flambda = make
  "no-flambda"
  (pass_or_skip (not Ocamltest_config.flambda)
    "support for flambda disabled"
    "support for flambda enabled")

let spacetime = Actions.make
  "spacetime"
  (pass_or_skip Ocamltest_config.spacetime
    "support for spacetime enabled"
    "support for spacetime disabled")

let no_spacetime = make
  "no-spacetime"
  (pass_or_skip (not Ocamltest_config.spacetime)
    "support for spacetime disabled"
    "support for spacetime enabled")

let shared_libraries = Actions.make
  "shared-libraries"
  (pass_or_skip Ocamltest_config.shared_libraries
    "Shared libraries are supported."
    "Shared libraries are not supported.")

let no_shared_libraries = Actions.make
  "no-shared-libraries"
  (pass_or_skip (not Ocamltest_config.shared_libraries)
    "Shared libraries are not supported."
    "Shared libraries are supported.")

let native_compiler = Actions.make
  "native-compiler"
  (pass_or_skip (Ocamltest_config.arch <> "none")
    "native compiler available"
    "native compiler not available")

let native_dynlink = Actions.make
  "native-dynlink"
  (pass_or_skip (Ocamltest_config.native_dynlink)
    "native dynlink support available"
    "native dynlink support not available")

let debugger = Actions.make
  "debugger"
  (pass_or_skip Ocamltest_config.ocamldebug
     "debugger available"
     "debugger not available")

let csharp_compiler = Actions.make
  "csharp-compiler"
  (pass_or_skip (Ocamltest_config.csc<>"")
    "C# compiler available"
    "C# compiler not available")

let windows_unicode = Actions.make
  "windows-unicode"
  (pass_or_skip (Ocamltest_config.windows_unicode )
    "Windows Unicode support available"
    "Windows Unicode support not available")

let afl_instrument = Actions.make
  "afl-instrument"
  (pass_or_skip Ocamltest_config.afl_instrument
    "AFL instrumentation enabled"
    "AFL instrumentation disabled")

let no_afl_instrument = Actions.make
  "no-afl-instrument"
  (pass_or_skip (not Ocamltest_config.afl_instrument)
    "AFL instrumentation disabled"
    "AFL instrumentation enabled")

let ocamldoc_output_file prefix =
  let+ backend = A.safe_lookup Ocaml_variables.ocamldoc_backend
  and+ prefix = prefix in
  let suffix = match backend with
    | "latex" -> ".tex"
    | "html" -> ".html"
    | "man" -> ".3o"
    | _ -> ".result" in
  prefix ^ suffix

let check_ocamldoc_output =
  make_check_tool_output
    "check-ocamldoc-output" `Ocamldoc

let ocamldoc_flags =
  A.safe_lookup Ocaml_variables.ocamldoc_flags

let compiled_doc_name input = input ^ ".odoc"

(* The compiler used for compiling both cmi file
   and plugins *)
let compiler_for_ocamldoc =
  let compiler = Ocaml_compilers.ocamlc_byte in
  compile_modules compiler (Ocaml_compilers.name compiler)
    (Ocaml_compilers.output_variable compiler)

(* Within ocamldoc tests,
   modules="a.ml b.ml" is interpreted as a list of
   secondaries documentation modules that need to be
   compiled into cmi files and odoc file (serialized ocamldoc information)
   before the main documentation is generated *)
let compile_ocamldoc_all module_basenames_filetypes =
    (* (basename,filetype as module_) = *)
  let+ filenames =
    A.map (List.map Ocaml_filetypes.make_filename) module_basenames_filetypes
  and+ module_basenames_filetypes = module_basenames_filetypes
  and+ run_params =
    let expected_exit_status =
      Actions_helpers.int_of_variable
        Ocaml_variables.ocamldoc_exit_status
    in
    Actions_helpers.run_params
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:Ocaml_variables.ocamldoc_output
      ~stderr_variable:Ocaml_variables.ocamldoc_output
      ~append:true
      ~expected_exit_codes:[expected_exit_status] ()
  and+ compile = compiler_for_ocamldoc module_basenames_filetypes in
  (* let what = Printf.sprintf "Compiling documentation for module %s" basename in *)
(* Printf.fprintf log "%s\n%!" what; *)
  Eff.if_pass compile
    (Eff.seq @@ List.map2 (fun (basename, _) filename ->
         let commandline =
           (* currently, we are ignoring the global ocamldoc_flags, since we
              don't have per-module flags *)
           [
             Ocaml_commands.ocamlrun_ocamldoc;
             Ocaml_flags.stdlib;
             "-dump " ^ compiled_doc_name basename;
             filename;
           ]
         in
         (Eff.run_cmd run_params commandline)
       ) module_basenames_filetypes filenames)
     (* if exit_status = expected_exit_status *)
     (* then Result.pass *)
     (* else begin *)
     (*   let reason = *)
     (*     (Actions_helpers.mkreason *)
     (*       what (String.concat " " commandline) exit_status) in *)
     (*   Result.fail_with_reason reason *)

let setup_ocamldoc_build_env =
  Actions.make "setup_ocamldoc_build_env" @@
  setup_tool_build_env `Ocamldoc @@
  let source_directory = Actions_helpers.test_source_directory in
  let root_file = A.map Filename.chop_extension Actions_helpers.testfile in
  let reference_prefix =
    let+ source_directory = source_directory
    and+ root_file = root_file in
    Filename.make_path [source_directory; root_file] in
  let output = ocamldoc_output_file root_file in
  let reference =
    let+ reference_prefix = reference_prefix
    and+ suffix = ocamldoc_reference_file_suffix in
    reference_prefix ^ suffix
  in
  let backend = A.safe_lookup Ocaml_variables.ocamldoc_backend in
  A.apply_modifiers Ocaml_modifiers.(str @ unix)
    (A.add Builtin_variables.reference reference
       (A.add Builtin_variables.output output
          (A.if_ (A.map ((=) "man") backend)
             (A.add_if_undefined
                Builtin_variables.skip_header_lines (A.return "1")
                (let+ (), env = A.with_env (A.return ()) in env))
             (let+ (), env = A.with_env (A.return ()) in env))))

let ocamldoc_plugin name = name ^ ".cmo"

let ocamldoc_backend_flag =
  let+ backend = A.safe_lookup Ocaml_variables.ocamldoc_backend in
  if backend = "" then "" else "-" ^ backend

let ocamldoc_o_flag =
  let+ output =  A.safe_lookup Builtin_variables.output
  and+ backend = A.safe_lookup Ocaml_variables.ocamldoc_backend in
  match backend with
  | "html" | "manual" -> "index"
  | _ -> output

let run_ocamldoc =
  Actions.make "ocamldoc" @@ A.with_env @@
  (* modules corresponds to secondaries modules of which the
     documentation and cmi files need to be build before the main
     module documentation *)
  let plugins = A.map (List.map Ocaml_filetypes.filetype) plugins in
  let modules = A.map (List.map Ocaml_filetypes.filetype) modules in
  (* plugins are used for custom documentation generators *)
  let+ plugin_compile = compiler_for_ocamldoc plugins
  and+ all_compile = compile_ocamldoc_all modules
  and+ ocamldoc_backend_flag = ocamldoc_backend_flag
  and+ ocamldoc_flags = ocamldoc_flags
  and+ ocamldoc_o_flag = ocamldoc_o_flag
  and+ load_all =
    (* sort module in alphabetical order *)
    A.map (List.map (fun name -> "-load " ^ compiled_doc_name (fst name)))
      (A.map (List.sort Stdlib.compare) modules)
  and+ with_plugins =
    A.map (List.map (fun name -> "-g " ^ ocamldoc_plugin (fst name)))
      plugins
  and+ input_file = Actions_helpers.testfile
  and+ run_params =
    Actions_helpers.run_params
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:Ocaml_variables.ocamldoc_output
      ~stderr_variable:Ocaml_variables.ocamldoc_output
      ~append:true ()
  in
  let commandline =
    Ocaml_commands.ocamlrun_ocamldoc ::
    ocamldoc_backend_flag ::
    Ocaml_flags.stdlib ::
    ocamldoc_flags ::
    load_all @
    with_plugins @
    input_file ::
    "-o" ::
    ocamldoc_o_flag :: []
  in
  Eff.if_pass plugin_compile
    (Eff.if_pass all_compile
       ((* Printf.fprintf log "Generating documentation for %s\n%!" input_file; *)
         Eff.run_cmd run_params commandline))
         (* if exit_status = 0 then *)
         (*   Result.pass *)
         (* else begin *)
         (*   let reason = *)
         (*     Actions_helpers.mkreason *)
         (*       "ocamldoc" (String.concat " " commandline) exit_status in *)
         (*   Result.fail_with_reason reason *)
         (* end) *)

let _ =
  Environments.register_initializer "find_source_modules" (A.cast find_source_modules);
  Environments.register_initializer "config_variables" config_variables;
  List.iter register
  [
    setup_ocamlc_byte_build_env;
    ocamlc_byte;
    check_ocamlc_byte_output;
    setup_ocamlc_opt_build_env;
    ocamlc_opt;
    check_ocamlc_opt_output;
    setup_ocamlopt_byte_build_env;
    ocamlopt_byte;
    check_ocamlopt_byte_output;
    setup_ocamlopt_opt_build_env;
    ocamlopt_opt;
    check_ocamlopt_opt_output;
    run_expect;
    compare_bytecode_programs;
    compare_native_programs;
    setup_ocaml_build_env;
    ocaml;
    check_ocaml_output;
    setup_ocamlnat_build_env;
    ocamlnat;
    check_ocamlnat_output;
    flat_float_array;
    no_flat_float_array;
    flambda;
    no_flambda;
    spacetime;
    no_spacetime;
    shared_libraries;
    no_shared_libraries;
    native_compiler;
    native_dynlink;
    debugger;
    csharp_compiler;
    windows_unicode;
    afl_instrument;
    no_afl_instrument;
    setup_ocamldoc_build_env;
    run_ocamldoc;
    check_ocamldoc_output;
    ocamldebug;
    ocamlmklib;
    codegen;
    cc;
    ocamlobjinfo
  ]
