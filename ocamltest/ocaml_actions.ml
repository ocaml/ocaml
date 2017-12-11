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

(* Compilers and flags *)

let ocamlsrcdir () =
  try Sys.getenv "OCAMLSRCDIR"
  with Not_found -> Ocamltest_config.ocamlsrcdir

type runtime_variant =
  | Normal
  | Debug
  | Instrumented

let runtime_variant() =
  let use_runtime = try Sys.getenv "USE_RUNTIME" with Not_found -> "" in
  if use_runtime="d" then Debug
  else if use_runtime="i" then Instrumented
  else Normal

let ocamlrun ocamlsrcdir =
  let runtime = match runtime_variant () with
    | Normal -> "ocamlrun"
    | Debug -> "ocamlrund"
    | Instrumented -> "ocamlruni" in
  let ocamlrunfile = Filename.mkexe runtime in
  Filename.make_path [ocamlsrcdir; "byterun"; ocamlrunfile]

let ocamlc ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocamlc"]

let ocaml ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocaml"]

let ocamlc_dot_byte ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let ocamlc = ocamlc ocamlsrcdir in
  ocamlrun ^ " " ^ ocamlc

let ocamlc_dot_opt ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocamlc.opt"]

let ocamlopt ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocamlopt"]

let ocamlopt_dot_byte ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let ocamlopt = ocamlopt ocamlsrcdir in
  ocamlrun ^ " " ^ ocamlopt

let ocamlopt_dot_opt ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocamlopt.opt"]

let ocaml_dot_byte ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let ocaml = ocaml ocamlsrcdir in
  ocamlrun ^ " " ^ ocaml

let ocaml_dot_opt ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; Filename.mkexe "ocamlnat"]

let cmpbyt ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "tools"; "cmpbyt"]

let stdlib ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "stdlib"]

let stdlib_flags ocamlsrcdir =
  let stdlib_path = stdlib ocamlsrcdir in
  "-nostdlib -I " ^ stdlib_path

let c_includes ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "byterun"]

let c_includes_flags ocamlsrcdir =
  let dir = c_includes ocamlsrcdir in
  "-ccopt -I" ^ dir

let use_runtime backend ocamlsrcdir = match backend with
  | Sys.Bytecode ->
    let ocamlrun = ocamlrun ocamlsrcdir in
    "-use-runtime " ^ ocamlrun
  | _ -> ""

let runtime_variant_flags backend ocamlsrcdir =
  let variant = runtime_variant() in
  if variant=Normal then ""
  else begin
    let variant_str = if variant=Debug then "d" else "i" in
    let backend_lib = match backend with
      | Sys.Bytecode -> "byterun"
      | Sys.Native -> "asmrun"
      | Sys.Other _ -> "stdlib" in
    let backend_lib_dir = Filename.make_path [ocamlsrcdir; backend_lib] in
    ("-runtime-variant " ^ variant_str ^" -I " ^ backend_lib_dir)
  end

(* Compiler descriptions *)

type compiler_description = {
  compiler_name : string -> string;
  compiler_flags : string;
  compiler_directory : string;
  compiler_backend : Sys.backend_type;
  compiler_exit_status_variabe : Variables.t;
  compiler_reference_variable : Variables.t;
  compiler_output_variable : Variables.t
}

(* Compilers compiling byte-code programs *)

let ocamlc_byte_compiler =
{
  compiler_name = ocamlc_dot_byte;
  compiler_flags = "";
  compiler_directory = "ocamlc.byte";
  compiler_backend = Sys.Bytecode;
  compiler_exit_status_variabe = Ocaml_variables.ocamlc_byte_exit_status;
  compiler_reference_variable = Ocaml_variables.compiler_reference;
  compiler_output_variable = Ocaml_variables.compiler_output;
}

let ocamlc_opt_compiler =
{
  compiler_name = ocamlc_dot_opt;
  compiler_flags = "";
  compiler_directory = "ocamlc.opt";
  compiler_backend = Sys.Bytecode;
  compiler_exit_status_variabe = Ocaml_variables.ocamlc_opt_exit_status;
  compiler_reference_variable = Ocaml_variables.compiler_reference2;
  compiler_output_variable = Ocaml_variables.compiler_output2;
}

(* Compilers compiling native-code programs *)

let ocamlopt_byte_compiler =
{
  compiler_name = ocamlopt_dot_byte;
  compiler_flags = "";
  compiler_directory = "ocamlopt.byte";
  compiler_backend = Sys.Native;
  compiler_exit_status_variabe = Ocaml_variables.ocamlopt_byte_exit_status;
  compiler_reference_variable = Ocaml_variables.compiler_reference;
  compiler_output_variable = Ocaml_variables.compiler_output;
}

let ocamlopt_opt_compiler =
{
  compiler_name = ocamlopt_dot_opt;
  compiler_flags = "";
  compiler_directory = "ocamlopt.opt";
  compiler_backend = Sys.Native;
  compiler_exit_status_variabe = Ocaml_variables.ocamlopt_opt_exit_status;
  compiler_reference_variable = Ocaml_variables.compiler_reference2;
  compiler_output_variable = Ocaml_variables.compiler_output2;
}

(* Top-levels *)

let ocaml_compiler = {
  compiler_name = ocaml_dot_byte;
  compiler_flags = "";
  compiler_directory = "ocaml";
  compiler_backend = Sys.Bytecode;
  compiler_exit_status_variabe = Ocaml_variables.ocaml_exit_status;
  compiler_reference_variable = Ocaml_variables.compiler_reference;
  compiler_output_variable = Ocaml_variables.compiler_output;
}

let ocamlnat_compiler = {
  compiler_name = ocaml_dot_opt;
  compiler_flags = "-S"; (* Keep intermediate assembly files *)
  compiler_directory = "ocamlnat";
  compiler_backend = Sys.Native;
  compiler_exit_status_variabe = Ocaml_variables.ocamlnat_exit_status;
  compiler_reference_variable = Ocaml_variables.compiler_reference2;
  compiler_output_variable = Ocaml_variables.compiler_output2;
}

let expected_compiler_exit_status env compiler =
  try int_of_string
    (Environments.safe_lookup compiler.compiler_exit_status_variabe env)
  with _ -> 0

let compiler_reference_filename env prefix compiler =
  let compiler_reference_suffix =
    Environments.safe_lookup Ocaml_variables.compiler_reference_suffix env in
  let suffix =
    if compiler_reference_suffix<>""
    then compiler_reference_suffix ^ ".reference"
    else ".reference" in
  let mk s = (Filename.make_filename prefix s) ^ suffix in
  let filename = mk compiler.compiler_directory in
  if Sys.file_exists filename then filename else
  let filename = mk
    (Ocaml_backends.string_of_backend compiler.compiler_backend) in
  if Sys.file_exists filename then filename else
  mk "compilers"

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

let link_modules
    ocamlsrcdir compiler compilername compileroutput program_variable
    custom c_headers_flags log env modules
  =
  let backend = compiler.compiler_backend in
  let expected_exit_status = expected_compiler_exit_status env compiler in
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
    use_runtime backend ocamlsrcdir;
    runtime_variant_flags backend ocamlsrcdir;
    stdlib_flags ocamlsrcdir;
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
  let backend = compiler.compiler_backend in
  let custom = (backend = Sys.Bytecode) && has_c_file in
  let c_headers_flags =
    if has_c_file then c_includes_flags ocamlsrcdir else "" in
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
    compiler.compiler_directory ^ compiler_directory_suffix in
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
  mk_compiler_env_setup "setup-ocamlc.byte-build-env" ocamlc_byte_compiler

let setup_ocamlc_opt_build_env =
  mk_compiler_env_setup "setup-ocamlc.opt-build-env" ocamlc_opt_compiler

let setup_ocamlopt_byte_build_env =
  mk_compiler_env_setup "setup-ocamlopt.byte-build-env" ocamlopt_byte_compiler

let setup_ocamlopt_opt_build_env =
  mk_compiler_env_setup "setup-ocamlopt.opt-build-env" ocamlopt_opt_compiler

let setup_ocaml_build_env =
  mk_compiler_env_setup "setup-ocaml-build-env" ocaml_compiler

let setup_ocamlnat_build_env =
  mk_compiler_env_setup "setup-ocamlnat-build-env" ocamlnat_compiler

let prepare_module (module_name, module_type) =
  match module_type with
    | Filetype.Implementation | Filetype.Interface | Filetype.C ->
      [(module_name, module_type)]
    | Filetype.C_minus_minus -> assert false
    | Filetype.Lexer -> assert false
    | Filetype.Grammar -> assert false

let compile_test_program program_variable compiler log env =
  let backend = compiler.compiler_backend in
  let testfile = Actions_helpers.testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let source_directory = Actions_helpers.test_source_directory env in
  let build_directory =
    Actions_helpers.test_build_directory env in
  let compilerreference_prefix =
    Filename.make_path [source_directory; testfile_basename] in
  let compilerreference_filename =
    compiler_reference_filename env compilerreference_prefix compiler in
  let compiler_reference_variable = compiler.compiler_reference_variable in
  let executable_filename =
    Filename.mkexe
      (Filename.make_filename
        testfile_basename (Ocaml_backends.executable_extension backend)) in
  let executable_path =
    Filename.make_path [build_directory; executable_filename] in
  let compiler_output_filename =
    Filename.make_filename compiler.compiler_directory "output" in
  let compiler_output =
    Filename.make_path [build_directory; compiler_output_filename] in
  let compiler_output_variable = compiler.compiler_output_variable in
  let newenv = Environments.add_bindings
    [
      (program_variable, executable_path);
      (compiler_reference_variable, compilerreference_filename);
      (compiler_output_variable, compiler_output);
    ] env in
  if Sys.file_exists compiler_output_filename then
    Sys.remove compiler_output_filename;
  let ocamlsrcdir = ocamlsrcdir () in
  let compilername = compiler.compiler_name ocamlsrcdir in
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

let ocamlc_byte = Actions.make
  "ocamlc.byte"
  (compile_test_program Builtin_variables.program ocamlc_byte_compiler)

let ocamlc_opt = Actions.make
  "ocamlc.opt"
  (compile_test_program Builtin_variables.program2 ocamlc_opt_compiler)

let ocamlopt_byte = Actions.make
  "ocamlopt.byte"
  (compile_test_program Builtin_variables.program ocamlopt_byte_compiler)

let ocamlopt_opt = Actions.make
  "ocamlopt.opt"
  (compile_test_program Builtin_variables.program2 ocamlopt_opt_compiler)

let run_expect log env =
  let newenv = Environments.apply_modifiers env Ocaml_modifiers.expect in
  Actions_helpers.run_script log newenv

let expect = Actions.make "expect" run_expect

let make_check_compiler_output name compiler = Actions.make
  name
  (Actions_helpers.check_output
    "compiler"
    compiler.compiler_output_variable
    compiler.compiler_reference_variable)

let check_ocamlc_byte_output = make_check_compiler_output
  "check-ocamlc.byte-output" ocamlc_byte_compiler

let check_ocamlc_opt_output = make_check_compiler_output
  "check-ocamlc.opt-output" ocamlc_opt_compiler

let check_ocamlopt_byte_output = make_check_compiler_output
  "check-ocamlopt.byte-output" ocamlopt_byte_compiler

let check_ocamlopt_opt_output = make_check_compiler_output
  "check-ocamlopt.opt-output" ocamlopt_opt_compiler

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
  let ocamlrun = ocamlrun ocamlsrcdir in
  let cmpbyt = cmpbyt ocamlsrcdir in
  let tool_name = ocamlrun ^ " " ^ cmpbyt in
  Filecompare.make_comparison_tool tool_name ""

let native_programs_comparison_tool = Filecompare.default_comparison_tool

let compare_bytecode_programs_code log env =
  let ocamlsrcdir = ocamlsrcdir () in
  let bytecode_programs_comparison_tool =
    make_bytecode_programs_comparison_tool ocamlsrcdir in
  compare_programs Sys.Bytecode bytecode_programs_comparison_tool log env

let compare_bytecode_programs = Actions.make
  "compare-bytecode-programs"
  compare_bytecode_programs_code

let compare_native_programs = Actions.make
  "compare-native-programs"
  (compare_programs Sys.Native native_programs_comparison_tool)

let run_test_program_in_toplevel toplevel log env =
  let testfile = Actions_helpers.testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let expected_exit_status = expected_compiler_exit_status env toplevel in
  let what =
    Printf.sprintf "Running %s in %s toplevel (expected exit status: %d)"
      testfile
      (Ocaml_backends.string_of_backend toplevel.compiler_backend)
      expected_exit_status in
  Printf.fprintf log "%s\n%!" what;
  let source_directory = Actions_helpers.test_source_directory env in
  let build_directory = Actions_helpers.test_build_directory env in
  let compilerreference_prefix =
    Filename.make_path [source_directory; testfile_basename] in
  let compilerreference_filename =
    compiler_reference_filename env compilerreference_prefix toplevel in
  let compiler_reference_variable = toplevel.compiler_reference_variable in
  let compiler_output_filename =
    Filename.make_filename toplevel.compiler_directory "output" in
  let compiler_output =
    Filename.make_path [build_directory; compiler_output_filename] in
  let compiler_output_variable = toplevel.compiler_output_variable in
  let newenv = Environments.add_bindings
    [
      (compiler_reference_variable, compilerreference_filename);
      (compiler_output_variable, compiler_output);
    ] env in
  if Sys.file_exists compiler_output_filename then
    Sys.remove compiler_output_filename;
  let ocamlsrcdir = ocamlsrcdir () in
  let toplevel_name = toplevel.compiler_name ocamlsrcdir in
  let toplevel_default_flags = "-noinit -no-version -noprompt" in
  let commandline =
  [
    toplevel_name;
    toplevel_default_flags;
    toplevel.compiler_flags;
    stdlib_flags ocamlsrcdir;
    directory_flags env;
    flags env;
  ] in
  let exit_status =
    Actions_helpers.run_cmd
      ~stdin_variable:Builtin_variables.test_file
      ~stdout_variable:compiler_output_variable
      ~stderr_variable:compiler_output_variable
      log newenv commandline in
  if exit_status=expected_exit_status
  then Pass newenv
  else Fail (Actions_helpers.mkreason
    what (String.concat " " commandline) exit_status)

let ocaml = Actions.make
  "ocaml"
  (run_test_program_in_toplevel ocaml_compiler)

let ocamlnat = Actions.make
  "ocamlnat"
  (run_test_program_in_toplevel ocamlnat_compiler)

let check_ocaml_output = make_check_compiler_output
  "check-ocaml-output" ocaml_compiler

let check_ocamlnat_output = make_check_compiler_output
  "check-ocamlnat-output" ocamlnat_compiler

let config_variables _log env = Environments.add_bindings
  [
    Ocaml_variables.c_preprocessor, Ocamltest_config.c_preprocessor;
    Ocaml_variables.ocamlc_default_flags,
      Ocamltest_config.ocamlc_default_flags;
    Ocaml_variables.ocamlopt_default_flags,
      Ocamltest_config.ocamlopt_default_flags;
    Ocaml_variables.ocamlsrcdir, ocamlsrcdir();
    Ocaml_variables.os_type, Sys.os_type;
  ] env

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
    expect;
    compare_bytecode_programs;
    compare_native_programs;
    setup_ocaml_build_env;
    ocaml;
    check_ocaml_output;
    setup_ocamlnat_build_env;
    ocamlnat;
    check_ocamlnat_output;
  ]
