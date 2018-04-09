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

(* Extracting information from environment *)

let native_support = Ocamltest_config.arch <> "none"

let no_native_compilers _log env =
  (Result.skip_with_reason "native compilers disabled", env)

let native_action a =
  if native_support then a else (Actions.update a no_native_compilers)

let get_backend_value_from_env env bytecode_var native_var =
  Ocaml_backends.make_backend_function
    (Environments.safe_lookup bytecode_var env)
    (Environments.safe_lookup native_var env)

let modules env =
  Actions_helpers.words_of_variable env Ocaml_variables.modules

let plugins env =
  Actions_helpers.words_of_variable env Ocaml_variables.plugins

let directories env =
  Actions_helpers.words_of_variable env Ocaml_variables.directories

let directory_flags env =
  let f dir = ("-I " ^ dir) in
  let l = List.map f (directories env) in
  String.concat " " l

let flags env = Environments.safe_lookup Ocaml_variables.flags env

let ocamllex_flags env =
  Environments.safe_lookup Ocaml_variables.ocamllex_flags env

let ocamlyacc_flags env =
  Environments.safe_lookup Ocaml_variables.ocamlyacc_flags env

let filelist env variable extension =
  let value = Environments.safe_lookup variable env in
  let filenames = String.words value in
  let add_extension filename = Filename.make_filename filename extension in
  String.concat " " (List.map add_extension filenames)

let libraries backend env =
  let extension = Ocaml_backends.library_extension backend in
  filelist env Ocaml_variables.libraries extension

let binary_modules backend env =
  let extension = Ocaml_backends.module_extension backend in
  filelist env Ocaml_variables.binary_modules extension
  
let backend_default_flags env =
  get_backend_value_from_env env
    Ocaml_variables.ocamlc_default_flags
    Ocaml_variables.ocamlopt_default_flags

let backend_flags env =
  get_backend_value_from_env env
    Ocaml_variables.ocamlc_flags
    Ocaml_variables.ocamlopt_flags

let dumb_term = [|"TERM=dumb"|]

type module_generator = {
  description : string;
  command : string -> string;
  flags : Environments.t -> string;
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

let generate_module generator ocamlsrcdir output_variable input log env =
  let basename = fst input in
  let input_file = Ocaml_filetypes.make_filename input in
  let what =
    Printf.sprintf "Generating %s module from %s"
    generator.description input_file
  in
  Printf.fprintf log "%s\n%!" what;
  let commandline =
  [
    generator.command ocamlsrcdir;
    generator.flags env;
    input_file
  ] in
  let expected_exit_status = 0 in
  let exit_status =
    Actions_helpers.run_cmd
      ~environment:dumb_term
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:output_variable
      ~stderr_variable:output_variable
      ~append:true
      log env commandline in
  if exit_status=expected_exit_status
  then generator.generated_compilation_units basename
  else begin
    let reason =
      (Actions_helpers.mkreason
        what (String.concat " " commandline) exit_status) in
    Printf.fprintf log "%s\n%!" reason;
    []
  end

let generate_lexer = generate_module ocamllex

let generate_parser = generate_module ocamlyacc

let prepare_module ocamlsrcdir output_variable log env input =
  let input_type = snd input in
  let open Ocaml_filetypes in
  match input_type with
    | Implementation | Interface | C -> [input]
    | Binary_interface -> [input]
    | Backend_specific _ -> [input]
    | C_minus_minus -> assert false
    | Lexer ->
      generate_lexer ocamlsrcdir output_variable input log env
    | Grammar ->
      generate_parser ocamlsrcdir output_variable input log env
    | Text -> assert false

let get_program_file backend env =
  let testfile = Actions_helpers.testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let program_filename =
    Filename.mkexe
      (Filename.make_filename
        testfile_basename (Ocaml_backends.executable_extension backend)) in
  let test_build_directory =
    Actions_helpers.test_build_directory env in
  Filename.make_path [test_build_directory; program_filename]

let compile_program ocamlsrcdir (compiler : Ocaml_compilers.compiler) log env =
  let program_variable = compiler#program_variable in
  let program_file = Environments.safe_lookup program_variable env in
  let all_modules =
    Actions_helpers.words_of_variable env Ocaml_variables.all_modules in
  let output_variable = compiler#output_variable in
  let prepare = prepare_module ocamlsrcdir output_variable log env in
  let modules =
    List.concatmap prepare (List.map Ocaml_filetypes.filetype all_modules) in
  let is_c_file (_filename, filetype) = filetype=Ocaml_filetypes.C in
  let has_c_file = List.exists is_c_file modules in
  let c_headers_flags =
    if has_c_file then Ocaml_flags.c_includes ocamlsrcdir else "" in
  let expected_exit_status =
    Ocaml_tools.expected_exit_status env (compiler :> Ocaml_tools.tool) in
  let module_names =
    (binary_modules compiler#target env) ^ " " ^
    (String.concat " " (List.map Ocaml_filetypes.make_filename modules)) in
  let what = Printf.sprintf "Compiling program %s from modules %s"
    program_file module_names in
  Printf.fprintf log "%s\n%!" what;
  let compile_only =
    Environments.lookup_as_bool Ocaml_variables.compile_only env = Some true
  in
  let compile_flags =
    if compile_only then " -c " else ""
  in
  let output = if compile_only then "" else "-o " ^ program_file in
  let commandline =
  [
    compiler#name ocamlsrcdir;
    Ocaml_flags.runtime_flags ocamlsrcdir compiler#target has_c_file;
    c_headers_flags;
    Ocaml_flags.stdlib ocamlsrcdir;
    directory_flags env;
    flags env;
    libraries compiler#target env;
    backend_default_flags env compiler#target;
    backend_flags env compiler#target;
    compile_flags;
    output;
    module_names
  ] in
  let exit_status =
    Actions_helpers.run_cmd
      ~environment:dumb_term
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:compiler#output_variable
      ~stderr_variable:compiler#output_variable
      ~append:true
      log env commandline in
  if exit_status=expected_exit_status
  then (Result.pass, env)
  else begin
    let reason =
      (Actions_helpers.mkreason
        what (String.concat " " commandline) exit_status) in
    (Result.fail_with_reason reason, env)
  end

let compile_module ocamlsrcdir compiler module_ log env =
  let expected_exit_status =
    Ocaml_tools.expected_exit_status env (compiler :> Ocaml_tools.tool) in
  let what = Printf.sprintf "Compiling module %s" module_ in
  Printf.fprintf log "%s\n%!" what;
  let commandline =
  [
    compiler#name ocamlsrcdir;
    Ocaml_flags.stdlib ocamlsrcdir;
    directory_flags env;
    flags env;
    libraries compiler#target env;
    backend_default_flags env compiler#target;
    backend_flags env compiler#target;
    "-c " ^ module_;
  ] in
  let exit_status =
    Actions_helpers.run_cmd
      ~environment:dumb_term
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:compiler#output_variable
      ~stderr_variable:compiler#output_variable
      ~append:true
      log env commandline in
  if exit_status=expected_exit_status
  then (Result.pass, env)
  else begin
    let reason =
      (Actions_helpers.mkreason
        what (String.concat " " commandline) exit_status) in
    (Result.fail_with_reason reason, env)
  end

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

let print_module_names log description modules =
  Printf.fprintf log "%s modules: %s\n%!"
    description
    (String.concat " " (List.map Ocaml_filetypes.make_filename modules))

let find_source_modules log env =
  let source_directory = Actions_helpers.test_source_directory env in
  let specified_modules =
    List.map Ocaml_filetypes.filetype
      ((plugins env) @ (modules env) @ [(Actions_helpers.testfile env)]) in
  print_module_names log "Specified" specified_modules;
  let source_modules =
    List.concatmap
      (add_module_interface source_directory)
      specified_modules in
  print_module_names log "Source" source_modules;
  Environments.add
    Ocaml_variables.all_modules
    (String.concat " " (List.map Ocaml_filetypes.make_filename source_modules))
    env

let setup_tool_build_env tool log env =
  let source_directory = Actions_helpers.test_source_directory env in
  let testfile = Actions_helpers.testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let tool_reference_variable =
    tool#reference_variable in
  let tool_reference_prefix =
    Filename.make_path [source_directory; testfile_basename] in
  let tool_reference_file =
    tool#reference_file env tool_reference_prefix
  in
  let env =
    Environments.add_if_undefined
      tool_reference_variable
      tool_reference_file env
  in
  let source_modules =
    Actions_helpers.words_of_variable env Ocaml_variables.all_modules in
  let tool_directory_suffix =
    Environments.safe_lookup Ocaml_variables.compiler_directory_suffix env in
  let tool_directory_name =
    tool#directory ^ tool_directory_suffix in
  let build_dir = Filename.concat
    (Environments.safe_lookup
      Builtin_variables.test_build_directory_prefix env)
    tool_directory_name in
  let tool_output_variable = tool#output_variable in
  let tool_output_filename =
    Filename.make_filename tool#directory "output" in
  let tool_output_file =
    Filename.make_path [build_dir; tool_output_filename]
  in
  let env =
    Environments.add_if_undefined
      tool_output_variable
      tool_output_file env
  in
  Sys.force_remove tool_output_file;
  let env =
    Environments.add Builtin_variables.test_build_directory build_dir env in
  Actions_helpers.setup_build_env false source_modules log env

let setup_compiler_build_env (compiler : Ocaml_compilers.compiler) log env =
  let (r, env) = setup_tool_build_env compiler log env in
  if Result.is_pass r then
  begin
    let prog_var = compiler#program_variable in
    let prog_output_var = compiler#program_output_variable in
    let default_prog_file = get_program_file compiler#target env in
    let env = Environments.add_if_undefined prog_var default_prog_file env in
    let prog_file = Environments.safe_lookup prog_var env in
    let prog_output_file = prog_file ^ ".output" in
    let env = match prog_output_var with
      | None -> env
      | Some outputvar ->
        Environments.add_if_undefined outputvar prog_output_file env
    in
    (r, env)
  end else (r, env)

let setup_toplevel_build_env (toplevel : Ocaml_toplevels.toplevel) log env =
  setup_tool_build_env toplevel log env

let mk_compiler_env_setup name (compiler : Ocaml_compilers.compiler) =
  Actions.make name (setup_compiler_build_env compiler)

let mk_toplevel_env_setup name (toplevel : Ocaml_toplevels.toplevel) =
  Actions.make name (setup_toplevel_build_env toplevel)

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
    Ocaml_toplevels.ocaml

let setup_ocamlnat_build_env =
  native_action
    (mk_toplevel_env_setup
      "setup-ocamlnat-build-env"
      Ocaml_toplevels.ocamlnat)

let compile (compiler : Ocaml_compilers.compiler) log env =
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  match Environments.lookup_nonempty Ocaml_variables.module_ env with
    | None -> compile_program ocamlsrcdir compiler log env
    | Some module_ -> compile_module ocamlsrcdir compiler module_ log env

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

let env_with_lib_unix ocamlsrcdir env =
  let libunixdir = Ocaml_directories.libunix ocamlsrcdir in
  let newlibs =
    match Environments.lookup Ocaml_variables.caml_ld_library_path env with
    | None -> libunixdir
    | Some libs -> libunixdir ^ " " ^ libs
  in
  Environments.add Ocaml_variables.caml_ld_library_path newlibs env

let debug log env =
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let program = Environments.safe_lookup Builtin_variables.program env in
  let what = Printf.sprintf "Debugging program %s" program in
  Printf.fprintf log "%s\n%!" what;
  let commandline =
  [
    Ocaml_commands.ocamlrun_ocamldebug ocamlsrcdir;
    Ocaml_flags.ocamldebug_default_flags ocamlsrcdir;
    program
  ] in
  let systemenv =
    Array.append
      dumb_term
      (Environments.to_system_env (env_with_lib_unix ocamlsrcdir env))
  in
  let expected_exit_status = 0 in
  let exit_status =
    Actions_helpers.run_cmd
      ~environment:systemenv
      ~stdin_variable: Ocaml_variables.ocamldebug_script
      ~stdout_variable:Builtin_variables.output
      ~stderr_variable:Builtin_variables.output
      ~append:true
      log (env_with_lib_unix ocamlsrcdir env) commandline in
  if exit_status=expected_exit_status
  then (Result.pass, env)
  else begin
    let reason =
      (Actions_helpers.mkreason
        what (String.concat " " commandline) exit_status) in
    (Result.fail_with_reason reason, env)
  end

let ocamldebug = Actions.make "ocamldebug" debug

let objinfo log env =
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let tools_directory = Ocaml_directories.tools ocamlsrcdir in
  let program = Environments.safe_lookup Builtin_variables.program env in
  let what = Printf.sprintf "Running ocamlobjinfo on %s" program in
  Printf.fprintf log "%s\n%!" what;
  let commandline =
  [
    Ocaml_commands.ocamlrun_ocamlobjinfo ocamlsrcdir;
    Ocaml_flags.ocamlobjinfo_default_flags;
    program
  ] in
  let ocamllib = [| (Printf.sprintf "OCAMLLIB=%s" tools_directory) |] in
  let systemenv =
    Array.concat
    [
      dumb_term;
      ocamllib;
      (Environments.to_system_env (env_with_lib_unix ocamlsrcdir env))
    ]
  in
  let expected_exit_status = 0 in
  let exit_status =
    Actions_helpers.run_cmd
      ~environment:systemenv
      ~stdout_variable:Builtin_variables.output
      ~stderr_variable:Builtin_variables.output
      ~append:true
      log (env_with_lib_unix ocamlsrcdir env) commandline in
  if exit_status=expected_exit_status
  then (Result.pass, env)
  else begin
    let reason =
      (Actions_helpers.mkreason
        what (String.concat " " commandline) exit_status) in
    (Result.fail_with_reason reason, env)
  end

let ocamlobjinfo = Actions.make "ocamlobjinfo" objinfo

let run_expect_once ocamlsrcdir input_file principal log env =
  let expect_flags = Sys.safe_getenv "EXPECT_FLAGS" in
  let repo_root = "-repo-root " ^ ocamlsrcdir in
  let principal_flag = if principal then "-principal" else "" in
  let commandline =
  [
    Ocaml_commands.ocamlrun_expect_test ocamlsrcdir;
    expect_flags;
    flags env;
    repo_root;
    principal_flag;
    input_file
  ] in
  let exit_status =
    Actions_helpers.run_cmd ~environment:dumb_term log env commandline in
  if exit_status=0 then (Result.pass, env)
  else begin
    let reason = (Actions_helpers.mkreason
      "expect" (String.concat " " commandline) exit_status) in
    (Result.fail_with_reason reason, env)
  end

let run_expect_twice ocamlsrcdir input_file log env =
  let corrected filename = Filename.make_filename filename "corrected" in
  let (result1, env1) = run_expect_once ocamlsrcdir input_file false log env in
  if Result.is_pass result1 then begin
    let intermediate_file = corrected input_file in
    let (result2, env2) =
      run_expect_once ocamlsrcdir intermediate_file true log env1 in
    if Result.is_pass result2 then begin
      let output_file = corrected intermediate_file in
      let output_env = Environments.add_bindings
      [
        Builtin_variables.reference, input_file;
        Builtin_variables.output, output_file
      ] env2 in
      (Result.pass, output_env)
    end else (result2, env2)
  end else (result1, env1)

let run_expect log env =
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let input_file = Actions_helpers.testfile env in
  run_expect_twice ocamlsrcdir input_file log env

let run_expect = Actions.make "run-expect" run_expect

let make_check_tool_output name tool = Actions.make
  name
  (Actions_helpers.check_output
    tool#family
    tool#output_variable
    tool#reference_variable)

let check_ocamlc_byte_output = make_check_tool_output
  "check-ocamlc.byte-output" Ocaml_compilers.ocamlc_byte

let check_ocamlc_opt_output =
  native_action
    (make_check_tool_output
      "check-ocamlc.opt-output" Ocaml_compilers.ocamlc_opt)

let check_ocamlopt_byte_output =
  native_action
    (make_check_tool_output
      "check-ocamlopt.byte-output" Ocaml_compilers.ocamlopt_byte)

let check_ocamlopt_opt_output =
  native_action
    (make_check_tool_output
      "check-ocamlopt.opt-output" Ocaml_compilers.ocamlopt_opt)

let really_compare_programs backend comparison_tool log env =
  let program = Environments.safe_lookup Builtin_variables.program env in
  let program2 = Environments.safe_lookup Builtin_variables.program2 env in
  let what = Printf.sprintf "Comparing %s programs %s and %s"
    (Ocaml_backends.string_of_backend backend) program program2 in
  Printf.fprintf log "%s\n%!" what;
  let files = {
    Filecompare.filetype = Filecompare.Binary;
    Filecompare.reference_filename = program;
    Filecompare.output_filename = program2
  } in
  if Ocamltest_config.flambda && backend = Ocaml_backends.Native
  then begin
    let reason =
      "flambda temporarily disables comparison of native programs" in
    (Result.pass_with_reason reason, env)
  end else
  if backend = Ocaml_backends.Native &&
    (Sys.os_type="Win32" || Sys.os_type="Cygwin")
  then begin
    let reason =
      "comparison of native programs temporarily disabled under Windows" in
    (Result.pass_with_reason reason, env)
  end else begin
    let comparison_tool =
      if backend=Ocaml_backends.Native &&
        (Sys.os_type="Win32" || Sys.os_type="Cygwin")
        then
          let bytes_to_ignore = 512 (* comparison_start_address program *) in
          Filecompare.(make_cmp_tool ~ignore:{bytes=bytes_to_ignore; lines=0})
        else comparison_tool in
    match Filecompare.compare_files ~tool:comparison_tool files with
      | Filecompare.Same -> (Result.pass, env)
      | Filecompare.Different ->
        let reason = Printf.sprintf "Files %s and %s are different"
          program program2 in
        (Result.fail_with_reason reason, env)
      | Filecompare.Unexpected_output -> assert false
      | Filecompare.Error (commandline, exitcode) ->
        let reason = Actions_helpers.mkreason what commandline exitcode in
        (Result.fail_with_reason reason, env)
  end

let compare_programs backend comparison_tool log env =
  let compare_programs =
    Environments.lookup_as_bool Ocaml_variables.compare_programs env in
  if compare_programs = Some false then begin
    let reason = "program comparison disabled" in
    (Result.pass_with_reason reason, env)
  end else really_compare_programs backend comparison_tool log env

let make_bytecode_programs_comparison_tool ocamlsrcdir =
  let ocamlrun = Ocaml_files.ocamlrun ocamlsrcdir in
  let cmpbyt = Ocaml_files.cmpbyt ocamlsrcdir in
  let tool_name = ocamlrun ^ " " ^ cmpbyt in
  Filecompare.make_comparison_tool tool_name ""

let native_programs_comparison_tool = Filecompare.default_comparison_tool

let compare_bytecode_programs_code log env =
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let bytecode_programs_comparison_tool =
    make_bytecode_programs_comparison_tool ocamlsrcdir in
  compare_programs
    Ocaml_backends.Bytecode bytecode_programs_comparison_tool log env

let compare_bytecode_programs =
  native_action
    (Actions.make
      "compare-bytecode-programs"
      compare_bytecode_programs_code)

let compare_native_programs =
  native_action
    (Actions.make
      "compare-native-programs"
      (compare_programs Ocaml_backends.Native native_programs_comparison_tool))

let compile_module
  ocamlsrcdir compiler compilername compileroutput log env
  (module_basename, module_filetype) =
  let backend = compiler#target in
  let filename =
    Ocaml_filetypes.make_filename (module_basename, module_filetype) in
  let expected_exit_status =
    Ocaml_tools.expected_exit_status env (compiler :> Ocaml_tools.tool) in
  let what = Printf.sprintf "%s for file %s (expected exit status: %d)"
    (Ocaml_filetypes.action_of_filetype module_filetype) filename
      (expected_exit_status) in
  let compile_commandline input_file output_file optional_flags =
    let compile = "-c " ^ input_file in
    let output = match output_file with
      | None -> ""
      | Some file -> "-o " ^ file in
    [
      compilername;
      Ocaml_flags.stdlib ocamlsrcdir;
      flags env;
      backend_flags env backend;
      optional_flags;
      compile;
      output;
    ] in
  let exec commandline =
    Printf.fprintf log "%s\n%!" what;
    let exit_status =
      Actions_helpers.run_cmd
        ~stdin_variable: Ocaml_variables.compiler_stdin
        ~stdout_variable:compileroutput
        ~stderr_variable:compileroutput
        ~append:true log env commandline in
    if exit_status=expected_exit_status
    then (Result.pass, env)
    else begin
      let reason =
        (Actions_helpers.mkreason
          what (String.concat " " commandline) exit_status) in
      (Result.fail_with_reason reason, env)
    end in
  match module_filetype with
    | Ocaml_filetypes.Interface ->
      let interface_name =
        Ocaml_filetypes.make_filename
          (module_basename, Ocaml_filetypes.Interface) in
      let commandline = compile_commandline interface_name None "" in
      exec commandline
    | Ocaml_filetypes.Implementation ->
      let module_extension = Ocaml_backends.module_extension backend in
      let module_output_name =
        Filename.make_filename module_basename module_extension in
      let commandline =
        compile_commandline filename (Some module_output_name) "" in
      exec commandline
    | Ocaml_filetypes.C ->
      let object_extension = Config.ext_obj in
      let _object_filename = module_basename ^ object_extension in
      let commandline =
        compile_commandline filename None
          (Ocaml_flags.c_includes ocamlsrcdir) in
      exec commandline
    | _ ->
      let reason = Printf.sprintf "File %s of type %s not supported yet"
        filename (Ocaml_filetypes.string_of_filetype module_filetype) in
      (Result.fail_with_reason reason, env)

let compile_modules
    ocamlsrcdir compiler compilername compileroutput
    modules_with_filetypes log initial_env
  =
  let compile_mod env mod_ =
    compile_module ocamlsrcdir compiler compilername compileroutput
    log env mod_ in
  let rec compile_mods env = function
    | [] -> (Result.pass, env)
    | m::ms ->
      (let (result, newenv) = compile_mod env m in
      if Result.is_pass result then (compile_mods newenv ms)
      else (result, newenv)) in
  compile_mods initial_env modules_with_filetypes

let run_test_program_in_toplevel (toplevel : Ocaml_toplevels.toplevel) log env =
  let testfile = Actions_helpers.testfile env in
  let expected_exit_status =
    Ocaml_tools.expected_exit_status env (toplevel :> Ocaml_tools.tool) in
  let compiler_output_variable = toplevel#output_variable in
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let compiler = toplevel#compiler in
  let compiler_name = compiler#name ocamlsrcdir in
  let modules_with_filetypes =
    List.map Ocaml_filetypes.filetype (modules env) in
  let (result, env) = compile_modules
    ocamlsrcdir compiler compiler_name compiler_output_variable
    modules_with_filetypes log env in
  if Result.is_pass result then begin
    let what =
      Printf.sprintf "Running %s in %s toplevel (expected exit status: %d)"
      testfile
      (Ocaml_backends.string_of_backend toplevel#backend)
      expected_exit_status in
    Printf.fprintf log "%s\n%!" what;
    let toplevel_name = toplevel#name ocamlsrcdir in
    let ocaml_script_as_argument =
      match
        Environments.lookup_as_bool
          Ocaml_variables.ocaml_script_as_argument env
      with
      | None -> false 
      | Some b -> b
    in
    let commandline =
    [
      toplevel_name;
      Ocaml_flags.toplevel_default_flags;
      toplevel#flags;
      Ocaml_flags.stdlib ocamlsrcdir;
      directory_flags env;
      Ocaml_flags.include_toplevel_directory ocamlsrcdir;
      flags env;
      libraries toplevel#backend env;
      binary_modules toplevel#backend env;
      if ocaml_script_as_argument then testfile else "";
      Environments.safe_lookup Builtin_variables.arguments env
    ] in
    let exit_status =
      if ocaml_script_as_argument
      then Actions_helpers.run_cmd
        ~environment:dumb_term
        ~stdout_variable:compiler_output_variable
        ~stderr_variable:compiler_output_variable
        log env commandline
      else Actions_helpers.run_cmd
        ~environment:dumb_term
        ~stdin_variable:Builtin_variables.test_file
        ~stdout_variable:compiler_output_variable
        ~stderr_variable:compiler_output_variable
        log env commandline
    in
    if exit_status=expected_exit_status
    then (Result.pass, env)
    else begin
      let reason =
        (Actions_helpers.mkreason
          what (String.concat " " commandline) exit_status) in
      (Result.fail_with_reason reason, env)
    end
  end else (result, env)

let ocaml = Actions.make
  "ocaml"
  (run_test_program_in_toplevel Ocaml_toplevels.ocaml)

let ocamlnat =
  native_action
    (Actions.make
      "ocamlnat"
      (run_test_program_in_toplevel Ocaml_toplevels.ocamlnat))

let check_ocaml_output = make_check_tool_output
  "check-ocaml-output" Ocaml_toplevels.ocaml

let check_ocamlnat_output =
  native_action
    (make_check_tool_output
      "check-ocamlnat-output" Ocaml_toplevels.ocamlnat)

let config_variables _log env = Environments.add_bindings
  [
    Ocaml_variables.c_preprocessor, Ocamltest_config.c_preprocessor;
    Ocaml_variables.ocamlc_default_flags,
      Ocamltest_config.ocamlc_default_flags;
    Ocaml_variables.ocamlopt_default_flags,
      Ocamltest_config.ocamlopt_default_flags;
    Ocaml_variables.ocamlrunparam, Sys.safe_getenv "OCAMLRUNPARAM";
    Ocaml_variables.ocamlsrcdir, Ocaml_directories.srcdir();
    Ocaml_variables.os_type, Sys.os_type;
  ] env

let flat_float_array = Actions.make
  "flat-float-array"
  (Actions_helpers.pass_or_skip Ocamltest_config.flat_float_array
    "compiler configured with -flat-float-array"
    "compiler configured with -no-flat-float-array")

let no_flat_float_array = make
  "no-flat-float-array"
  (Actions_helpers.pass_or_skip (not Ocamltest_config.flat_float_array)
    "compiler configured with -no-flat-float-array"
    "compiler configured with -flat-float-array")

let flambda = Actions.make
  "flambda"
  (Actions_helpers.pass_or_skip Ocamltest_config.flambda
    "support for flambda enabled"
    "support for flambda disabled")

let no_flambda = make
  "no-flambda"
  (Actions_helpers.pass_or_skip (not Ocamltest_config.flambda)
    "support for flambda disabled"
    "support for flambda enabled")

let spacetime = Actions.make
  "spacetime"
  (Actions_helpers.pass_or_skip Ocamltest_config.spacetime
    "support for spacetime enabled"
    "support for spacetime disabled")

let no_spacetime = make
  "no-spacetime"
  (Actions_helpers.pass_or_skip (not Ocamltest_config.spacetime)
    "support for spacetime disabled"
    "support for spacetime enabled")

let shared_libraries = Actions.make
  "shared-libraries"
  (Actions_helpers.pass_or_skip Ocamltest_config.shared_libraries
    "Shared libraries are supported."
    "Shared libraries are not supported.")

let native_compiler = Actions.make
  "native-compiler"
  (Actions_helpers.pass_or_skip (Ocamltest_config.arch <> "none")
    "native compiler available"
    "native compiler not available")

let afl_instrument = Actions.make
  "afl-instrument"
  (Actions_helpers.pass_or_skip Ocamltest_config.afl_instrument
    "AFL instrumentation enabled"
    "AFL instrumentation disabled")

let no_afl_instrument = Actions.make
  "no-afl-instrument"
  (Actions_helpers.pass_or_skip (not Ocamltest_config.afl_instrument)
    "AFL instrumentation disabled"
    "AFL instrumentation enabled")

let ocamldoc = Ocaml_tools.ocamldoc

let ocamldoc_output_file env prefix =
  let backend =
    Environments.safe_lookup Ocaml_variables.ocamldoc_backend env in
  let suffix = match backend with
    | "latex" -> ".tex"
    | "html" -> ".html"
    | "man" -> ".3o"
    | _ -> ".result" in
  prefix ^ suffix

let check_ocamldoc_output = make_check_tool_output
  "check-ocamldoc-output" ocamldoc

let ocamldoc_flags env =
  Environments.safe_lookup Ocaml_variables.ocamldoc_flags env

let compiled_doc_name input = input ^ ".odoc"

(* The compiler used for compiling both cmi file
   and plugins *)
let compiler_for_ocamldoc ocamlsrcdir =
  let compiler = Ocaml_compilers.ocamlc_byte in
  compile_modules ocamlsrcdir compiler (compiler#name ocamlsrcdir)
    compiler#output_variable

(* Within ocamldoc tests,
   modules="a.ml b.ml" is interpreted as a list of
   secondaries documentation modules that need to be
   compiled into cmi files and odoc file (serialized ocamldoc information)
   before the main documentation is generated *)
let compile_ocamldoc ocamlsrcdir (basename,filetype as module_) log env =
  let expected_exit_status =
    Ocaml_tools.expected_exit_status env (ocamldoc :> Ocaml_tools.tool) in
  let what = Printf.sprintf "Compiling documentation for module %s" basename in
  Printf.fprintf log "%s\n%!" what;
  let filename =
    Ocaml_filetypes.make_filename (basename, filetype) in
  let (r,env) = compiler_for_ocamldoc ocamlsrcdir [module_] log env in
  if not (Result.is_pass r) then (r,env) else
  let commandline =
    (* currently, we are ignoring the global ocamldoc_flags, since we
       don't have per-module flags *)
    [
    Ocaml_commands.ocamlrun_ocamldoc ocamlsrcdir;
    Ocaml_flags.stdlib ocamlsrcdir;
    "-dump " ^ compiled_doc_name basename;
     filename;
  ] in
  let exit_status =
    Actions_helpers.run_cmd
      ~environment:(Environments.to_system_env env)
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:ocamldoc#output_variable
      ~stderr_variable:ocamldoc#output_variable
      ~append:true
      log env commandline in
  if exit_status=expected_exit_status
  then (Result.pass, env)
  else begin
    let reason =
      (Actions_helpers.mkreason
        what (String.concat " " commandline) exit_status) in
    (Result.fail_with_reason reason, env)
  end

let rec ocamldoc_compile_all ocamlsrcdir log env = function
  | [] -> (Result.pass, env)
  | a :: q ->
      let (r,env) = compile_ocamldoc ocamlsrcdir a log env in
      if Result.is_pass r then
        ocamldoc_compile_all ocamlsrcdir log env q
      else
        (r,env)

let setup_ocamldoc_build_env =
  Actions.make "setup_ocamldoc_build_env" @@ fun log env ->
  let (r,env) = setup_tool_build_env ocamldoc log env in
  if not (Result.is_pass r) then (r,env) else
  let source_directory = Actions_helpers.test_source_directory env in
  let root_file = Filename.chop_extension (Actions_helpers.testfile env) in
  let reference_prefix = Filename.make_path [source_directory; root_file] in
  let output = ocamldoc_output_file env root_file in
  let reference= reference_prefix ^ ocamldoc#reference_filename_suffix env in
  let backend = Environments.safe_lookup Ocaml_variables.ocamldoc_backend env in
  let env =
    Environments.apply_modifiers env  Ocaml_modifiers.(str @ unix)
    |> Environments.add Builtin_variables.reference reference
    |> Environments.add Builtin_variables.output output in
  let env =
    if backend = "man" then Environments.add_if_undefined
        Builtin_variables.skip_header_lines "1" env
    else env in
  Result.pass, env

let ocamldoc_plugin name = name ^ ".cmo"

let ocamldoc_backend_flag env =
  let backend = Environments.safe_lookup Ocaml_variables.ocamldoc_backend env in
  if backend = "" then "" else "-" ^ backend

let ocamldoc_o_flag env =
  let output =  Environments.safe_lookup Builtin_variables.output env in
  match Environments.safe_lookup Ocaml_variables.ocamldoc_backend env with
  | "html" | "manual" -> "index"
  | _ -> output

let run_ocamldoc =
  Actions.make "ocamldoc" @@ fun log env ->
  (* modules corresponds to secondaries modules of which the
     documentation and cmi files need to be build before the main
     module documentation *)
  let modules =  List.map Ocaml_filetypes.filetype @@ modules env in
  (* plugins are used for custom documentation generators *)
  let plugins = List.map Ocaml_filetypes.filetype @@ plugins env in
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let (r,env) = compiler_for_ocamldoc ocamlsrcdir plugins log env in
  if not (Result.is_pass r) then r, env else
  let (r,env) = ocamldoc_compile_all ocamlsrcdir log env modules in
  if not (Result.is_pass r) then r, env else
  let input_file = Actions_helpers.testfile env in
  Printf.fprintf log "Generating documentation for %s\n%!" input_file;
  let load_all =
    List.map (fun name -> "-load " ^ compiled_doc_name (fst name))
    @@ (* sort module in alphabetical order *)
    List.sort Pervasives.compare modules in
  let with_plugins =
    List.map (fun name -> "-g " ^ ocamldoc_plugin (fst name)) plugins in
  let commandline =
  [
    Ocaml_commands.ocamlrun_ocamldoc ocamlsrcdir;
    ocamldoc_backend_flag env;
    Ocaml_flags.stdlib ocamlsrcdir;
    ocamldoc_flags env]
  @ load_all @ with_plugins @
   [ input_file;
     "-o"; ocamldoc_o_flag env
   ] in
  let exit_status =
    Actions_helpers.run_cmd ~environment:(Environments.to_system_env env)
      ~stdin_variable: Ocaml_variables.compiler_stdin
      ~stdout_variable:ocamldoc#output_variable
      ~stderr_variable:ocamldoc#output_variable
      ~append:true
      log env commandline in
  if exit_status=0 then
    (Result.pass, env)
  else begin
    let reason = (Actions_helpers.mkreason
      "ocamldoc" (String.concat " " commandline) exit_status) in
    (Result.fail_with_reason reason, env)
  end

let _ =
  Environments.register_initializer "find_source_modules" find_source_modules;
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
    native_compiler;
    afl_instrument;
    no_afl_instrument;
    setup_ocamldoc_build_env;
    run_ocamldoc;
    check_ocamldoc_output;
    ocamldebug;
    ocamlobjinfo
  ]
