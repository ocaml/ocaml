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

let get_backend_value_from_env env bytecode_var native_var =
  Ocaml_backends.make_backend_function
    (Environments.safe_lookup bytecode_var env)
    (Environments.safe_lookup native_var env)

let modules env =
  Actions_helpers.words_of_variable env Ocaml_variables.modules

let directories env =
  Actions_helpers.words_of_variable env Ocaml_variables.directories

let directory_flags env =
  let f dir = ("-I " ^ dir) in
  let l = List.map f (directories env) in
  String.concat " " l

let flags env = Environments.safe_lookup Ocaml_variables.flags env

let libraries backend env =
  let value = Environments.safe_lookup Ocaml_variables.libraries env in
  let libs = String.words value in
  let extension = Ocaml_backends.library_extension backend in
  let add_extension lib = Filename.make_filename lib extension in
  String.concat " " (List.map add_extension libs)

let backend_default_flags env =
  get_backend_value_from_env env
    Ocaml_variables.ocamlc_default_flags
    Ocaml_variables.ocamlopt_default_flags

let backend_flags env =
  get_backend_value_from_env env
    Ocaml_variables.ocamlc_flags
    Ocaml_variables.ocamlopt_flags

let dumb_term = [|"TERM=dumb"|]

let prepare_module (module_name, module_type) =
  let open Ocaml_filetypes in
  match module_type with
    | Implementation | Interface | C ->
      [(module_name, module_type)]
    | Binary_interface -> [(module_name, module_type)]
    | Backend_specific _ -> [(module_name, module_type)]
    | C_minus_minus -> assert false
    | Lexer -> assert false
    | Grammar -> assert false

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

let compile_program ocamlsrcdir compiler program_variable log env =
  let backend = compiler#backend in
  let program_file = Environments.safe_lookup program_variable env in
  let all_modules =
    Actions_helpers.words_of_variable env Ocaml_variables.all_modules in
  let modules =
    List.concatmap prepare_module
      (List.map Ocaml_filetypes.filetype all_modules) in
  let is_c_file (_filename, filetype) = filetype=Ocaml_filetypes.C in
  let has_c_file = List.exists is_c_file modules in
  let c_headers_flags =
    if has_c_file then Ocaml_flags.c_includes ocamlsrcdir else "" in
  let expected_exit_status =
    Ocaml_tools.expected_exit_status env (compiler :> Ocaml_tools.tool) in
  let module_names =
    String.concat " " (List.map Ocaml_filetypes.make_filename modules) in
  let what = Printf.sprintf "Compiling program %s from modules %s"
    program_file module_names in
  Printf.fprintf log "%s\n%!" what;
  let output = "-o " ^ program_file in
  let commandline =
  [
    compiler#name ocamlsrcdir;
    Ocaml_flags.runtime_flags ocamlsrcdir backend has_c_file;
    c_headers_flags;
    Ocaml_flags.stdlib ocamlsrcdir;
    directory_flags env;
    flags env;
    libraries backend env;
    backend_default_flags env backend;
    backend_flags env backend;
    output;
    module_names
  ] in
  let exit_status =
    Actions_helpers.run_cmd
      ~environment:dumb_term
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
  let backend = compiler#backend in
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
    libraries backend env;
    backend_default_flags env backend;
    backend_flags env backend;
    "-c " ^ module_;
  ] in
  let exit_status =
    Actions_helpers.run_cmd
      ~environment:dumb_term
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
      ((modules env) @ [(Actions_helpers.testfile env)]) in
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
    let default_prog_file = get_program_file compiler#backend env in
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
  mk_compiler_env_setup
    "setup-ocamlc.opt-build-env"
    Ocaml_compilers.ocamlc_opt

let setup_ocamlopt_byte_build_env =
  mk_compiler_env_setup
    "setup-ocamlopt.byte-build-env"
    Ocaml_compilers.ocamlopt_byte

let setup_ocamlopt_opt_build_env =
  mk_compiler_env_setup
    "setup-ocamlopt.opt-build-env"
    Ocaml_compilers.ocamlopt_opt

let setup_ocaml_build_env =
  mk_toplevel_env_setup
    "setup-ocaml-build-env"
    Ocaml_toplevels.ocaml

let setup_ocamlnat_build_env =
  mk_toplevel_env_setup
    "setup-ocamlnat-build-env"
    Ocaml_toplevels.ocamlnat

let compile program_variable compiler log env =
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  match Environments.lookup_nonempty Ocaml_variables.module_ env with
    | None -> compile_program ocamlsrcdir compiler program_variable log env
    | Some module_ -> compile_module ocamlsrcdir compiler module_ log env

(* Compile actions *)

let ocamlc_byte =
  Actions.make
    "ocamlc.byte"
    (compile
      Builtin_variables.program Ocaml_compilers.ocamlc_byte)

let ocamlc_opt =
  Actions.make
    "ocamlc.opt"
    (compile
      Builtin_variables.program2 Ocaml_compilers.ocamlc_opt)

let ocamlopt_byte =
  Actions.make
    "ocamlopt.byte"
    (compile
      Builtin_variables.program Ocaml_compilers.ocamlopt_byte)

let ocamlopt_opt =
  Actions.make
    "ocamlopt.opt"
    (compile
      Builtin_variables.program2 Ocaml_compilers.ocamlopt_opt)

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

let make_check_compiler_output name compiler = Actions.make
  name
  (Actions_helpers.check_output
    "compiler"
    compiler#output_variable
    compiler#reference_variable)

let check_ocamlc_byte_output = make_check_compiler_output
  "check-ocamlc.byte-output" Ocaml_compilers.ocamlc_byte

let check_ocamlc_opt_output = make_check_compiler_output
  "check-ocamlc.opt-output" Ocaml_compilers.ocamlc_opt

let check_ocamlopt_byte_output = make_check_compiler_output
  "check-ocamlopt.byte-output" Ocaml_compilers.ocamlopt_byte

let check_ocamlopt_opt_output = make_check_compiler_output
  "check-ocamlopt.opt-output" Ocaml_compilers.ocamlopt_opt

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
          Filecompare.make_cmp_tool bytes_to_ignore
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

let compare_bytecode_programs = Actions.make
  "compare-bytecode-programs"
  compare_bytecode_programs_code

let compare_native_programs = Actions.make
  "compare-native-programs"
  (compare_programs Ocaml_backends.Native native_programs_comparison_tool)

let compile_module
  ocamlsrcdir compiler compilername compileroutput log env
  (module_basename, module_filetype) =
  let backend = compiler#backend in
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

let run_test_program_in_toplevel toplevel log env =
  let testfile = Actions_helpers.testfile env in
  let expected_exit_status =
    Ocaml_tools.expected_exit_status env (toplevel :> Ocaml_tools.tool) in
  let compiler_output_variable = toplevel#output_variable in
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let compiler = match toplevel#backend with
    | Ocaml_backends.Native -> Ocaml_compilers.ocamlopt_byte
    | Ocaml_backends.Bytecode -> Ocaml_compilers.ocamlc_byte in
  let compiler_name = compiler#name ocamlsrcdir in
  let modules_with_filetypes =
    List.map Ocaml_filetypes.filetype (modules env) in
  let (modules_result, modules_env) = compile_modules
    ocamlsrcdir compiler compiler_name compiler_output_variable
    modules_with_filetypes log env in
  if Result.is_pass modules_result then begin
    let what =
      Printf.sprintf "Running %s in %s toplevel (expected exit status: %d)"
      testfile
      (Ocaml_backends.string_of_backend toplevel#backend)
      expected_exit_status in
    Printf.fprintf log "%s\n%!" what;
    let toplevel_name = toplevel#name ocamlsrcdir in
    let toplevel_default_flags = "-noinit -no-version -noprompt" in
    let commandline =
    [
      toplevel_name;
      toplevel_default_flags;
      toplevel#flags;
      Ocaml_flags.stdlib ocamlsrcdir;
      directory_flags modules_env;
      Ocaml_flags.include_toplevel_directory ocamlsrcdir;
      flags modules_env;
    ] in
    let exit_status =
      Actions_helpers.run_cmd
        ~environment:dumb_term
        ~stdin_variable:Builtin_variables.test_file
        ~stdout_variable:compiler_output_variable
        ~stderr_variable:compiler_output_variable
        log modules_env commandline in
    if exit_status=expected_exit_status
    then (Result.pass, modules_env)
    else begin
      let reason =
        (Actions_helpers.mkreason
          what (String.concat " " commandline) exit_status) in
      (Result.fail_with_reason reason, modules_env)
    end
  end else (modules_result, modules_env)

let ocaml = Actions.make
  "ocaml"
  (run_test_program_in_toplevel Ocaml_toplevels.ocaml)

let ocamlnat = Actions.make
  "ocamlnat"
  (run_test_program_in_toplevel Ocaml_toplevels.ocamlnat)

let check_ocaml_output = make_check_compiler_output
  "check-ocaml-output" Ocaml_toplevels.ocaml

let check_ocamlnat_output = make_check_compiler_output
  "check-ocamlnat-output" Ocaml_toplevels.ocamlnat

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
  ]
