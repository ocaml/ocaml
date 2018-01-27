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

let link_modules
    ocamlsrcdir compiler compilername compileroutput program_variable
    custom c_headers_flags log env modules
  =
  let backend = compiler.Ocaml_compilers.backend in
  let expected_exit_status =
    Ocaml_compilers.expected_exit_status env compiler in
  let executable_name = match Environments.lookup program_variable env with
    | None -> assert false
    | Some program -> program in
  let module_names =
    String.concat " " (List.map Filetype.make_filename modules) in
  let what = Printf.sprintf "Linking modules %s into %s"
    module_names executable_name in
  Printf.fprintf log "%s\n%!" what;
  let output = "-o " ^ executable_name in
  let customstr = if custom then "-custom" else "" in
  let commandline =
  [
    compilername;
    customstr;
    c_headers_flags;
    Ocaml_flags.use_runtime backend ocamlsrcdir;
    Ocaml_flags.runtime_variant backend ocamlsrcdir;
    Ocaml_flags.stdlib ocamlsrcdir;
    "-linkall";
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
      ~stdout_variable:compileroutput
      ~stderr_variable:compileroutput
      ~append:true
      log env commandline in
  if exit_status=expected_exit_status
  then Pass env
  else Fail (Actions_helpers.mkreason
    what (String.concat " " commandline) exit_status)

let compile_program
    ocamlsrcdir compiler compilername compileroutput program_variable
    log env modules
  =
  let is_c_file (_filename, filetype) = filetype=Filetype.C in
  let has_c_file = List.exists is_c_file modules in
  let backend = compiler.Ocaml_compilers.backend in
  let custom = (backend = Sys.Bytecode) && has_c_file in
  let c_headers_flags =
    if has_c_file then Ocaml_flags.c_includes ocamlsrcdir else "" in
  link_modules
    ocamlsrcdir compiler compilername compileroutput
    program_variable custom c_headers_flags log env modules

let module_has_interface directory module_name =
  let interface_name =
    Filetype.make_filename (module_name, Filetype.Interface) in
  let interface_fullpath = Filename.make_path [directory;interface_name] in
  Sys.file_exists interface_fullpath

let add_module_interface directory module_description =
  match module_description with
    | (filename, Filetype.Implementation) when
      module_has_interface directory filename ->
        [(filename, Filetype.Interface); module_description]
  | _ -> [module_description]

let print_module_names log description modules =
  Printf.fprintf log "%s modules: %s\n%!"
    description
    (String.concat " " (List.map Filetype.make_filename modules))

let find_source_modules log env =
  let source_directory = Actions_helpers.test_source_directory env in
  let specified_modules =
    List.map Filetype.filetype
      ((modules env) @ [(Actions_helpers.testfile env)]) in
  print_module_names log "Specified" specified_modules;
  let source_modules =
    List.concatmap
      (add_module_interface source_directory)
      specified_modules in
  print_module_names log "Source" source_modules;
  Environments.add
    Ocaml_variables.source_modules
    (String.concat " " (List.map Filetype.make_filename source_modules))
    env

let setup_compiler_build_env compiler log env =
  let source_modules =
    Actions_helpers.words_of_variable env Ocaml_variables.source_modules in
  let compiler_directory_suffix =
    Environments.safe_lookup Ocaml_variables.compiler_directory_suffix env in
  let compiler_directory_name =
    compiler.Ocaml_compilers.directory ^ compiler_directory_suffix in
  let build_dir = Filename.concat
    (Environments.safe_lookup
      Builtin_variables.test_build_directory_prefix env)
    compiler_directory_name in
  let newenv =
    Environments.add Builtin_variables.test_build_directory build_dir env in
  Actions_helpers.setup_build_env false source_modules log newenv

let mk_compiler_env_setup name compiler =
  Actions.make name (setup_compiler_build_env compiler)

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
  mk_compiler_env_setup
    "setup-ocaml-build-env"
    Ocaml_compilers.ocaml

let setup_ocamlnat_build_env =
  mk_compiler_env_setup
    "setup-ocamlnat-build-env"
    Ocaml_compilers.ocamlnat

let prepare_module (module_name, module_type) =
  match module_type with
    | Filetype.Implementation | Filetype.Interface | Filetype.C ->
      [(module_name, module_type)]
    | Filetype.C_minus_minus -> assert false
    | Filetype.Lexer -> assert false
    | Filetype.Grammar -> assert false

let compile_test_program program_variable compiler log env =
  let backend = compiler.Ocaml_compilers.backend in
  let testfile = Actions_helpers.testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let source_directory = Actions_helpers.test_source_directory env in
  let build_directory =
    Actions_helpers.test_build_directory env in
  let compilerreference_prefix =
    Filename.make_path [source_directory; testfile_basename] in
  let compilerreference_filename =
    Ocaml_compilers.reference_filename env compilerreference_prefix compiler in
  let compiler_reference_variable =
    compiler.Ocaml_compilers.reference_variable in
  let executable_filename =
    Filename.mkexe
      (Filename.make_filename
        testfile_basename (Ocaml_backends.executable_extension backend)) in
  let executable_path =
    Filename.make_path [build_directory; executable_filename] in
  let compiler_output_filename =
    Filename.make_filename compiler.Ocaml_compilers.directory "output" in
  let compiler_output =
    Filename.make_path [build_directory; compiler_output_filename] in
  let compiler_output_variable = compiler.Ocaml_compilers.output_variable in
  let newenv = Environments.add_bindings
    [
      (program_variable, executable_path);
      (compiler_reference_variable, compilerreference_filename);
      (compiler_output_variable, compiler_output);
    ] env in
  if Sys.file_exists compiler_output_filename then
    Sys.remove compiler_output_filename;
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let compilername = compiler.Ocaml_compilers.name ocamlsrcdir in
  let source_modules =
    Actions_helpers.words_of_variable env Ocaml_variables.source_modules in
  let prepared_modules =
    List.concatmap prepare_module
      (List.map Filetype.filetype source_modules) in
  compile_program
    ocamlsrcdir
    compiler
    compilername
    compiler_output_variable
    program_variable log newenv prepared_modules

(* Compile actions *)

let ocamlc_byte =
  Actions.make
    "ocamlc.byte"
    (compile_test_program
      Builtin_variables.program Ocaml_compilers.ocamlc_byte)

let ocamlc_opt =
  Actions.make
    "ocamlc.opt"
    (compile_test_program
      Builtin_variables.program2 Ocaml_compilers.ocamlc_opt)

let ocamlopt_byte =
  Actions.make
    "ocamlopt.byte"
    (compile_test_program
      Builtin_variables.program Ocaml_compilers.ocamlopt_byte)

let ocamlopt_opt =
  Actions.make
    "ocamlopt.opt"
    (compile_test_program
      Builtin_variables.program2 Ocaml_compilers.ocamlopt_opt)

let run_expect_once ocamlsrcdir input_file principal log env =
  let expect_flags = try Sys.getenv "EXPECT_FLAGS" with Not_found -> "" in
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
  if exit_status=0 then Pass env
  else Fail (Actions_helpers.mkreason
    "expect" (String.concat " " commandline) exit_status)

let run_expect_twice ocamlsrcdir input_file log env =
  let corrected filename = Filename.make_filename filename "corrected" in
  let first_run = run_expect_once ocamlsrcdir input_file false log env in
  match first_run with
    | Skip _ | Fail _ -> first_run
    | Pass env1 ->
      let intermediate_file = corrected input_file in
      let second_run =
        run_expect_once ocamlsrcdir intermediate_file true log env1 in
      (match second_run with
      | Skip _ | Fail _ -> second_run
      | Pass env2 ->
        let output_file = corrected intermediate_file in
        let output_env = Environments.add_bindings
        [
          Builtin_variables.reference, input_file;
          Builtin_variables.output, output_file
        ] env2 in
        Pass output_env
      )

let run_expect log env =
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let input_file = Actions_helpers.testfile env in
  run_expect_twice ocamlsrcdir input_file log env

let run_expect = Actions.make "run-expect" run_expect

let make_check_compiler_output name compiler = Actions.make
  name
  (Actions_helpers.check_output
    "compiler"
    compiler.Ocaml_compilers.output_variable
    compiler.Ocaml_compilers.reference_variable)

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
  if Ocamltest_config.flambda && backend = Sys.Native
  then begin
    Printf.fprintf log
      "flambda temporarily disables comparison of native programs";
    Pass env
  end else
  if backend = Sys.Native && (Sys.os_type="Win32" || Sys.os_type="Cygwin")
  then begin
    Printf.fprintf log
      "comparison of native programs temporarily disabled under Windows";
    Pass env
  end else begin
    let comparison_tool =
      if backend=Sys.Native && (Sys.os_type="Win32" || Sys.os_type="Cygwin")
        then
          let bytes_to_ignore = 512 (* comparison_start_address program *) in
          Filecompare.make_cmp_tool bytes_to_ignore
        else comparison_tool in
    match Filecompare.compare_files ~tool:comparison_tool files with
      | Filecompare.Same -> Pass env
      | Filecompare.Different ->
        let reason = Printf.sprintf "Files %s and %s are different"
          program program2 in
        Fail reason
      | Filecompare.Unexpected_output -> assert false
      | Filecompare.Error (commandline, exitcode) ->
        let reason = Actions_helpers.mkreason what commandline exitcode in
        Fail reason
  end

let compare_programs backend comparison_tool log env =
  let compare_programs =
    Environments.safe_lookup Ocaml_variables.compare_programs env in
  if compare_programs = "false" then begin
    Printf.fprintf log "Skipping program comparison (disabled)";
    Pass env
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
  compare_programs Sys.Bytecode bytecode_programs_comparison_tool log env

let compare_bytecode_programs = Actions.make
  "compare-bytecode-programs"
  compare_bytecode_programs_code

let compare_native_programs = Actions.make
  "compare-native-programs"
  (compare_programs Sys.Native native_programs_comparison_tool)

let compile_module
  ocamlsrcdir compiler compilername compileroutput log env
  (module_basename, module_filetype) =
  let backend = compiler.Ocaml_compilers.backend in
  let filename = Filetype.make_filename (module_basename, module_filetype) in
  let expected_exit_status =
    Ocaml_compilers.expected_exit_status env compiler in
  let what = Printf.sprintf "%s for file %s (expected exit status: %d)"
    (Filetype.action_of_filetype module_filetype) filename
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
    then Pass env
    else Fail (Actions_helpers.mkreason
      what (String.concat " " commandline) exit_status) in
  match module_filetype with
    | Filetype.Interface ->
      let interface_name =
        Filetype.make_filename (module_basename, Filetype.Interface) in
      let commandline = compile_commandline interface_name None "" in
      exec commandline
    | Filetype.Implementation ->
      let module_extension = Ocaml_backends.module_extension backend in
      let module_output_name =
        Filename.make_filename module_basename module_extension in
      let commandline =
        compile_commandline filename (Some module_output_name) "" in
      exec commandline
    | Filetype.C ->
      let object_extension = Config.ext_obj in
      let _object_filename = module_basename ^ object_extension in
      let commandline =
        compile_commandline filename None
          (Ocaml_flags.c_includes ocamlsrcdir) in
      exec commandline
    | _ ->
      let reason = Printf.sprintf "File %s of type %s not supported yet"
        filename (Filetype.string_of_filetype module_filetype) in
      (Fail reason)

let compile_modules
    ocamlsrcdir compiler compilername compileroutput
    modules_with_filetypes log initial_env
  =
  let compile_mod env mod_ =
    compile_module ocamlsrcdir compiler compilername compileroutput
    log env mod_ in
  let rec compile_mods env = function
    | [] -> Pass env
    | m::ms ->
      (match compile_mod env m with
        | Fail _ | Skip _ as error -> error
        | Pass newenv -> (compile_mods newenv ms)
      ) in
  compile_mods initial_env modules_with_filetypes

let run_test_program_in_toplevel toplevel log env =
  let testfile = Actions_helpers.testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let expected_exit_status =
    Ocaml_compilers.expected_exit_status env toplevel in
  let source_directory = Actions_helpers.test_source_directory env in
  let build_directory = Actions_helpers.test_build_directory env in
  let compiler_output_variable = toplevel.Ocaml_compilers.output_variable in
  let (env, compiler_output) =
    (match Environments.lookup compiler_output_variable env with
    | Some value -> (env, value)
    | None ->
      let compiler_output_filename =
        Filename.make_filename toplevel.Ocaml_compilers.directory "output" in
      let value =
        Filename.make_path [build_directory; compiler_output_filename] in
      let env' = Environments.add compiler_output_variable value env in
      (env', value)) in
  let compiler_reference_variable =
    toplevel.Ocaml_compilers.reference_variable in
  let env =
    if Environments.is_variable_defined compiler_reference_variable env then env
    else begin
      let compilerreference_prefix =
        Filename.make_path [source_directory; testfile_basename] in
      let compilerreference_filename =
        Ocaml_compilers.reference_filename
          env compilerreference_prefix toplevel in
      Environments.add
        compiler_reference_variable compilerreference_filename env
    end in
  if Sys.file_exists compiler_output then
    Sys.remove compiler_output;
  let ocamlsrcdir = Ocaml_directories.srcdir () in
  let compiler = match toplevel.Ocaml_compilers.backend with
    | Sys.Native -> Ocaml_compilers.ocamlopt_byte
    | Sys.Bytecode -> Ocaml_compilers.ocamlc_byte
    | Sys.Other _ -> assert false in
  let compiler_name = compiler.Ocaml_compilers.name ocamlsrcdir in
  let modules_with_filetypes = List.map Filetype.filetype (modules env) in
  let aux = compile_modules
    ocamlsrcdir compiler compiler_name compiler_output_variable
    modules_with_filetypes log env in
  match aux with
    | Fail _ | Skip _ -> aux
    | Pass auxenv ->
      begin
        let what =
          Printf.sprintf "Running %s in %s toplevel (expected exit status: %d)"
          testfile
          (Ocaml_backends.string_of_backend toplevel.Ocaml_compilers.backend)
          expected_exit_status in
        Printf.fprintf log "%s\n%!" what;
        let toplevel_name = toplevel.Ocaml_compilers.name ocamlsrcdir in
        let toplevel_default_flags = "-noinit -no-version -noprompt" in
        let commandline =
        [
          toplevel_name;
          toplevel_default_flags;
          toplevel.Ocaml_compilers.flags;
          Ocaml_flags.stdlib ocamlsrcdir;
          directory_flags auxenv;
          Ocaml_flags.include_toplevel_directory ocamlsrcdir;
          flags auxenv;
        ] in
        let exit_status =
          Actions_helpers.run_cmd
            ~environment:dumb_term
            ~stdin_variable:Builtin_variables.test_file
            ~stdout_variable:compiler_output_variable
            ~stderr_variable:compiler_output_variable
            log auxenv commandline in
        if exit_status=expected_exit_status
        then Pass auxenv
        else Fail (Actions_helpers.mkreason
          what (String.concat " " commandline) exit_status)
      end

let ocaml = Actions.make
  "ocaml"
  (run_test_program_in_toplevel Ocaml_compilers.ocaml)

let ocamlnat = Actions.make
  "ocamlnat"
  (run_test_program_in_toplevel Ocaml_compilers.ocamlnat)

let check_ocaml_output = make_check_compiler_output
  "check-ocaml-output" Ocaml_compilers.ocaml

let check_ocamlnat_output = make_check_compiler_output
  "check-ocamlnat-output" Ocaml_compilers.ocamlnat

let config_variables _log env = Environments.add_bindings
  [
    Ocaml_variables.c_preprocessor, Ocamltest_config.c_preprocessor;
    Ocaml_variables.ocamlc_default_flags,
      Ocamltest_config.ocamlc_default_flags;
    Ocaml_variables.ocamlopt_default_flags,
      Ocamltest_config.ocamlopt_default_flags;
    Ocaml_variables.ocamlsrcdir, Ocaml_directories.srcdir();
    Ocaml_variables.os_type, Sys.os_type;
  ] env

let flat_float_array = Actions.make
  "flat-float-array"
  (Actions_helpers.pass_or_skip Ocamltest_config.flat_float_array
    "The flat-float-array action succeeds.\n"
    "Compiler configured with -no-flat-float-array.")

let no_flat_float_array = make
  "no-flat-float-array"
  (Actions_helpers.pass_or_skip (not Ocamltest_config.flat_float_array)
    "The no-flat-float-array action succeeds.\n"
    "The compiler has been configured with -flat-float-array.")

let shared_libraries = Actions.make
  "shared-libraries"
  (Actions_helpers.pass_or_skip Ocamltest_config.shared_libraries
    "Shared libraries are supported.\n"
    "Shared libraries are not supported.")

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
    shared_libraries;
  ]
