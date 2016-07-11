(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of a few builtin actions *)

open Actions

(* Miscellaneous functions *)

let env_id env = env

let run_command
  ?(stdin_variable=Builtin_variables.stdin)
  ?(stdout_variable=Builtin_variables.stdout)
  ?(stderr_variable=Builtin_variables.stderr)
  ?(append=false)
  ?(timeout=0)
  log env cmd =
  let log_redirection std filename =
    if filename<>"" then
    begin
      Printf.fprintf log "  Redirecting %s to %s \n%!" std filename
    end in
  let lst = Testlib.words cmd in
  let cmd' = String.concat " " lst in
  Printf.fprintf log "Commandline: %s\n" cmd';
  let progname = List.hd lst in
  let arguments = Array.of_list lst in
  (*
  let environment =
    try [|Sys.getenv "PATH" |]
    with Not_found -> [| |] in
  *)
  let stdin_filename = Environments.safe_lookup stdin_variable env in
  let stdout_filename = Environments.safe_lookup stdout_variable env in
  let stderr_filename = Environments.safe_lookup stderr_variable env in
  log_redirection "stdin" stdin_filename;
  log_redirection "stdout" stdout_filename;
  log_redirection "stderr" stderr_filename;
  Run_command.run {
    Run_command.progname = progname;
    Run_command.argv = arguments;
    (* Run_command.envp = environment; *)
    Run_command.stdin_filename = stdin_filename;
    Run_command.stdout_filename = stdout_filename;
    Run_command.stderr_filename = stderr_filename;
    Run_command.append = append;
    Run_command.timeout = timeout;
    Run_command.log = log
  }

let mkreason what commandline exitcode =
  Printf.sprintf "%s: command\n%s\nfailed with exit code %d"
    what commandline exitcode

let make_file_name name ext = String.concat "." [name; ext]

let make_path components = List.fold_left Filename.concat "" components

let rec map_reduce_result f g init = function
  | [] -> Ok init
  | x::xs ->
    (match f x with
      | Ok fx ->
        (match map_reduce_result f g init xs with
          | Ok fxs -> Ok (g fx fxs)
          | Error _ as e -> e
        )
      | Error _ as e -> e
    )

let setup_symlinks test_source_directory build_directory files =
  let symlink filename =
    let src = Filename.concat test_source_directory filename in
    let cmd = "ln -sf " ^ src ^" " ^ build_directory in
    Testlib.run_system_command cmd in
  List.iter symlink files

(* Compilers and flags *)

let ocamlsrcdir () =
  try Sys.getenv "OCAMLSRCDIR"
  with Not_found -> failwith "The OCAMLSRCDIR environment variable is not set"

let ocamlrun ocamlsrcdir =
  make_path [ocamlsrcdir; "byterun"; "ocamlrun"]

let ocamlc ocamlsrcdir =
  make_path [ocamlsrcdir; "ocamlc"]

let ocaml ocamlsrcdir =
  make_path [ocamlsrcdir; "ocaml"]

let ocamlc_dot_byte ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let ocamlc = ocamlc ocamlsrcdir in
  ocamlrun ^ " " ^ ocamlc

let ocamlc_dot_opt ocamlsrcdir =
  make_path [ocamlsrcdir; "ocamlc.opt"]

let ocamlopt ocamlsrcdir =
  make_path [ocamlsrcdir; "ocamlopt"]

let ocamlopt_dot_byte ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let ocamlopt = ocamlopt ocamlsrcdir in
  ocamlrun ^ " " ^ ocamlopt

let ocamlopt_dot_opt ocamlsrcdir =
  make_path [ocamlsrcdir; "ocamlopt.opt"]

let ocaml_dot_byte ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let ocaml = ocaml ocamlsrcdir in
  ocamlrun ^ " " ^ ocaml

let ocaml_dot_opt ocamlsrcdir =
  make_path [ocamlsrcdir; "ocamlnat"]

let cmpbyt ocamlsrcdir =
  make_path [ocamlsrcdir; "tools"; "cmpbyt"]

let stdlib ocamlsrcdir =
  make_path [ocamlsrcdir; "stdlib"]

let stdlib_flags ocamlsrcdir =
  let stdlib_path = stdlib ocamlsrcdir in
  "-nostdlib -I " ^ stdlib_path

let testingmodule_flags ocamlsrcdir =
  let testing_module_directory =
    make_path [ocamlsrcdir; "testsuite"; "lib"] in
  "-I " ^ testing_module_directory

let use_runtime backend ocamlsrcdir = match backend with
  | Sys.Bytecode ->
    let ocamlrun = ocamlrun ocamlsrcdir in
    "-use-runtime " ^ ocamlrun
  | _ -> ""

(* Compiler descriptions *)

type compiler_info = {
  compiler_name : string -> string;
  compiler_directory : string;
  compiler_backend : Sys.backend_type;
  compiler_exit_status_variabe : Variables.t;
  compiler_reference_variable : Variables.t;
  compiler_output_variable : Variables.t
}

(* Compilers compiling byte-code programs *)

let bytecode_bytecode_compiler =
{
  compiler_name = ocamlc_dot_byte;
  compiler_directory = "ocamlc.byte";
  compiler_backend = Sys.Bytecode;
  compiler_exit_status_variabe = Builtin_variables.ocamlc_byte_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference;
  compiler_output_variable = Builtin_variables.compiler_output;
}

let bytecode_nativecode_compiler =
{
  compiler_name = ocamlc_dot_opt;
  compiler_directory = "ocamlc.opt";
  compiler_backend = Sys.Bytecode;
  compiler_exit_status_variabe = Builtin_variables.ocamlc_opt_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference2;
  compiler_output_variable = Builtin_variables.compiler_output2;
}

(* Compilers compiling native-code programs *)

let nativecode_bytecode_compiler =
{
  compiler_name = ocamlopt_dot_byte;
  compiler_directory = "ocamlopt.byte";
  compiler_backend = Sys.Native;
  compiler_exit_status_variabe = Builtin_variables.ocamlopt_byte_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference;
  compiler_output_variable = Builtin_variables.compiler_output;
}

let nativecode_nativecode_compiler =
{
  compiler_name = ocamlopt_dot_opt;
  compiler_directory = "ocamlopt.opt";
  compiler_backend = Sys.Native;
  compiler_exit_status_variabe = Builtin_variables.ocamlopt_opt_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference2;
  compiler_output_variable = Builtin_variables.compiler_output2;
}

(* Top-levels *)

let bytecode_toplevel = {
  compiler_name = ocaml_dot_byte;
  compiler_directory = "ocaml.byte";
  compiler_backend = Sys.Bytecode;
  compiler_exit_status_variabe = Builtin_variables.ocaml_byte_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference;
  compiler_output_variable = Builtin_variables.compiler_output;
}

let nativecode_toplevel = {
  compiler_name = ocaml_dot_opt;
  compiler_directory = "ocaml.opt";
  compiler_backend = Sys.Native;
  compiler_exit_status_variabe = Builtin_variables.ocaml_opt_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference2;
  compiler_output_variable = Builtin_variables.compiler_output2;
}

let expected_compiler_exit_status env compiler =
  try int_of_string
    (Environments.safe_lookup compiler.compiler_exit_status_variabe env)
  with _ -> 0 

let compiler_reference_filename env prefix compiler =
  let compiler_reference_suffix =
    Environments.safe_lookup Builtin_variables.compiler_reference_suffix env in
  let suffix =
    if compiler_reference_suffix<>""
    then compiler_reference_suffix ^ ".reference"
    else ".reference" in
  let mk s = (make_file_name prefix s) ^suffix in
  let filename = mk compiler.compiler_directory in
  if Sys.file_exists filename then filename else
  let filename = mk (Backends.string_of_backend compiler.compiler_backend) in
  if Sys.file_exists filename then filename else
  mk "compilers"

(* Extracting information from environment *)

let get_backend_value_from_env env bytecode_var native_var =
  Backends.make_backend_function
    (Environments.safe_lookup bytecode_var env)
    (Environments.safe_lookup native_var env)

let testfile env =
  match Environments.lookup Builtin_variables.test_file env with
  | None -> assert false
  | Some t -> t

let words_of_variable variable env =
  Testlib.words (Environments.safe_lookup variable env)

let modules env = words_of_variable Builtin_variables.modules env

let files env = words_of_variable Builtin_variables.files env

let use_testing_module env =
  Environments.is_variable_defined Builtin_variables.use_testing_module env

let flags env = Environments.safe_lookup Builtin_variables.flags env

let libraries backend env =
  let value = Environments.safe_lookup Builtin_variables.libraries env in
  let libs = Testlib.words value in
  let extension = Backends.library_extension backend in
  let add_extension lib = make_file_name lib extension in
  String.concat " " (List.map add_extension libs)

let backend_flags env =
  get_backend_value_from_env env
    Builtin_variables.bcflags
    Builtin_variables.ncflags

let test_source_directory env =
  Environments.safe_lookup Builtin_variables.test_source_directory env

let test_build_directory env =
  Environments.safe_lookup Builtin_variables.test_build_directory env

let action_of_filetype = function
  | Filetype.Implementation -> "Compiling implementation"
  | Filetype.Interface -> "Compiling interface"
  | Filetype.C -> "Compiling C source file"
  | Filetype.C_minor -> "Processing C Minor file"
  | Filetype.Lexer -> "Generating lexer"
  | Filetype.Grammar -> "Generating parser"

let rec compile_module
  ocamlsrcdir compiler compilername compileroutput log env
  (module_basename, module_filetype) =
  let backend = compiler.compiler_backend in
  let filename = Filetype.make_filename module_basename module_filetype in
  let expected_exit_status = expected_compiler_exit_status env compiler in
  let what = Printf.sprintf "%s for file %s (expected exit status: %d)"
    (action_of_filetype module_filetype) filename (expected_exit_status) in
  let compile_commandline input_file output_file =
    let compile = "-c " ^ input_file in
    let output = match output_file with
      | None -> ""
      | Some file -> "-o " ^ file in
    String.concat " "
    [
      compilername;
      stdlib_flags ocamlsrcdir;
      if use_testing_module env then testingmodule_flags ocamlsrcdir else "";
      flags env;
      backend_flags env backend;
      libraries backend env;
      compile;
      output;
    ] in
  let exec commandline output =
    Printf.fprintf log "%s\n%!" what;
    let exit_status = 
      run_command
        ~stdout_variable:compileroutput
        ~stderr_variable:compileroutput 
        ~append:true log env commandline in
    if exit_status=expected_exit_status
    then Ok output
    else Error (mkreason what commandline exit_status) in
  match module_filetype with
    | Filetype.Interface ->
      let interface_name =
        Filetype.make_filename module_basename Filetype.Interface in
      let commandline = compile_commandline interface_name None in
      exec commandline ""
    | Filetype.Implementation ->
      (* First see if the module has an interface. *)
      (* If it has one, compile it first *)
      let interface_result =
        let interface_name =
          Filetype.make_filename module_basename Filetype.Interface in
        if Sys.file_exists interface_name
        then compile_module
          ocamlsrcdir compiler compilername compileroutput log env
          (module_basename, Filetype.Interface)
        else (Ok ("Module " ^ module_basename ^ " has no interface")) in
      begin match interface_result with
        | Error _ -> interface_result
        | Ok _ ->
          let module_extension = Backends.module_extension backend in
          let module_output_name = make_file_name module_basename module_extension in
          let commandline =
            compile_commandline filename (Some module_output_name) in
          exec commandline module_output_name
      end
    | Filetype.C ->
      let object_filename = make_file_name module_basename "o" in
      let commandline = compile_commandline filename None in
      exec commandline object_filename
    | _ ->
      let reason = Printf.sprintf "File %s of type %s not supported yet"
        filename (Filetype.string_of_filetype module_filetype) in
      (Error reason)

let compile_modules ocamlsrcdir compiler compilername compileroutput log env modules_with_filetypes =
  let cons x xs = x::xs in
  map_reduce_result
    (compile_module ocamlsrcdir compiler compilername compileroutput log env)
    cons
    []
    modules_with_filetypes

let link_modules ocamlsrcdir compiler compilername compileroutput program_variable custom log env modules =
  let backend = compiler.compiler_backend in
  let expected_exit_status = expected_compiler_exit_status env compiler in
  let modules =
    if use_testing_module env then
      let testing_module =
        make_file_name "testing" (Backends.module_extension backend) in
      testing_module::modules
    else modules in
  let executable_name = match Environments.lookup program_variable env with
    | None -> assert false
    | Some program -> program in
  let module_names = String.concat " " modules in
  let what = Printf.sprintf "Linking modules %s into %s"
    module_names executable_name in
  Printf.fprintf log "%s\n%!" what;
  let output = "-o " ^ executable_name in
  let customstr = if custom then "-custom" else "" in
  let commandline = String.concat " "
  [
    compilername;
    customstr;
    use_runtime backend ocamlsrcdir;
    stdlib_flags ocamlsrcdir;
    if use_testing_module env then testingmodule_flags ocamlsrcdir else "";
    flags env;
    libraries backend env;
    backend_flags env backend;
    module_names;
    output
  ] in
  let exit_status =
    run_command
      ~stdout_variable:compileroutput
      ~stderr_variable:compileroutput
      ~append:true
      log env commandline in
  if exit_status=expected_exit_status
  then Ok ()
  else Error (mkreason what commandline exit_status)

let compile_program ocamlsrcdir compiler compilername compileroutput program_variable log env modules_with_filetypes =
  match
    compile_modules ocamlsrcdir compiler compilername compileroutput log env modules_with_filetypes
  with
    | Ok module_objects ->
      let is_c_file (_filename, filetype) = filetype=Filetype.C in
      let has_c_file = List.exists is_c_file modules_with_filetypes in
      let backend = compiler.compiler_backend in
      let custom = (backend = Sys.Bytecode) && has_c_file in
      (match link_modules ocamlsrcdir compiler compilername compileroutput program_variable custom log env module_objects with
        | Ok _ -> Pass env
        | Error reason -> Fail reason
      )
    | Error reason -> Fail reason

let module_has_interface directory module_name =
  match Filetype.filetype module_name with
  | (basename, Filetype.Implementation) ->
    let interface_name = Filetype.make_filename basename Filetype.Interface in
    let interface_fullpath = make_path [directory;interface_name] in
    if Sys.file_exists interface_fullpath then Some interface_name else None
  | _ -> None

let rec find_module_interfaces directory = function
  | [] -> []
  | module_name :: module_names ->
    begin
      let interfaces =
        find_module_interfaces directory module_names in
      match module_has_interface directory module_name with
      | None -> interfaces
      | Some interface -> interface::interfaces
    end

let compile_test_program program_variable compiler log env =
  let backend = compiler.compiler_backend in
  let testfile = testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let executable_filename =
    make_file_name testfile_basename (Backends.executable_extension backend) in
  let modules = (modules env) @ [testfile] in
  let modules_with_filetypes = List.map Filetype.filetype modules in
  let test_source_directory = test_source_directory env in
  let module_interfaces =
    find_module_interfaces test_source_directory modules in
  let compilerreference_prefix =
    make_path [test_source_directory; testfile_basename] in
  let compilerreference_filename =
    compiler_reference_filename env compilerreference_prefix compiler in
  let compiler_directory_suffix =
    Environments.safe_lookup Builtin_variables.compiler_directory_suffix env in
  let compiler_directory_name =
    compiler.compiler_directory ^ compiler_directory_suffix in
  let compiler_directory =
    make_path [test_build_directory env; compiler_directory_name] in
  let executable_path = make_path [compiler_directory; executable_filename] in
  let compiler_output_filename =
    make_file_name compiler.compiler_directory "output" in
  let compiler_output =
    make_path [compiler_directory; compiler_output_filename] in
  let compiler_output_variable = compiler.compiler_output_variable in
  let compiler_reference_variable = compiler.compiler_reference_variable in
  let newenv = Environments.add_bindings
    [
      (program_variable, executable_path);
      (compiler_reference_variable, compilerreference_filename);
      (compiler_output_variable, compiler_output);
    ] env in
  Testlib.make_directory compiler_directory;
  setup_symlinks test_source_directory compiler_directory modules;
  setup_symlinks test_source_directory compiler_directory module_interfaces;
  setup_symlinks test_source_directory compiler_directory (files env);
  Sys.chdir compiler_directory;
  if Sys.file_exists compiler_output_filename then
    Sys.remove compiler_output_filename;
  let ocamlsrcdir = ocamlsrcdir () in
  let compilername = compiler.compiler_name ocamlsrcdir in
  compile_program
    ocamlsrcdir
    compiler
    compilername
    compiler_output_variable
    program_variable log newenv modules_with_filetypes

(* Compile actions *)

let compile_bytecode_with_bytecode_compiler = {
  action_name = "compile-bytecode-with-bytecode-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program
      Builtin_variables.program bytecode_bytecode_compiler
}

let compile_bytecode_with_nativecode_compiler = {
  action_name = "compile-bytecode-with-nativecode-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program
      Builtin_variables.program2 bytecode_nativecode_compiler
}

let compile_nativecode_with_bytecode_compiler = {
  action_name = "compile-nativecode-with-bytecode-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program
      Builtin_variables.program nativecode_bytecode_compiler
}

let compile_nativecode_with_nativecode_compiler = {
  action_name = "compile-nativecode-with-nativecode-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program
      Builtin_variables.program2 nativecode_nativecode_compiler
}

let execute_program log env =
  match Environments.lookup Builtin_variables.program env with
  | None -> Fail "In execute action: No \"program\" variable in environment"
  | Some program ->
    let arguments = Environments.safe_lookup Builtin_variables.arguments env in
    let commandline = program ^ " " ^ arguments in
    let what = "Executing program " ^ program ^ " " ^
    begin if arguments="" then "without any argument"
    else "with arguments " ^ arguments
    end in
    let output = program ^ ".output" in
    let bindings =
    [
      Builtin_variables.stdout, output;
      Builtin_variables.stderr, output
    ] in
    let env' = Environments.add_bindings bindings env in
    match run_command log env' commandline with
      | 0 -> Pass (Environments.add Builtin_variables.output output env)
      | _ as exitcode -> Fail (mkreason what commandline exitcode)

let execute = {
  action_name = "execute";
  action_environment = env_id;
  action_body = execute_program
}

let check_output kind_of_output output_variable reference_variable log env =
  let reference_filename = Environments.safe_lookup reference_variable env in
  let output_filename = Environments.safe_lookup output_variable env in
  Printf.fprintf log "Comparing %s output %s to reference %s\n%!"
    kind_of_output output_filename reference_filename;
  let files =
  {
    Filecompare.reference_filename = reference_filename;
    Filecompare.output_filename = output_filename
  } in
  match Filecompare.check_file files with
    | Filecompare.Same -> Pass env
    | Filecompare.Different ->
      let reason = Printf.sprintf "%s output %s differs from reference %s"
        kind_of_output output_filename reference_filename in
      (Actions.Fail reason)
    | Filecompare.Unexpected_output ->
      let reason = Printf.sprintf "The file %s was expected to be empty because there is no reference file %s but it is not"
        output_filename reference_filename in
      (Actions.Fail reason)
    | Filecompare.Error (commandline, exitcode) ->
      let reason = Printf.sprintf "The command %s failed with status %d"
        commandline exitcode in
      (Actions.Fail reason)

let make_check_compiler_output name compiler = {
  action_name = name;
  action_environment = env_id;
  action_body =
    check_output
      "compiler"
      compiler.compiler_output_variable
      compiler.compiler_reference_variable
}

let check_ocamlc_dot_byte_output = make_check_compiler_output
  "check-ocamlc-byte-output" bytecode_bytecode_compiler

let check_ocamlc_dot_opt_output = make_check_compiler_output
  "check-ocamlc-opt-output" bytecode_nativecode_compiler

let check_ocamlopt_dot_byte_output = make_check_compiler_output
  "check-ocamlopt-byte-output" nativecode_bytecode_compiler

let check_ocamlopt_dot_opt_output = make_check_compiler_output
  "check-ocamlopt-opt-output" nativecode_nativecode_compiler

let check_program_output = {
  action_name = "check-program-output";
  action_environment = env_id;
  action_body = check_output "program"
    Builtin_variables.output
    Builtin_variables.reference
}

let compare_programs backend comparison_tool log env =
  let program = Environments.safe_lookup Builtin_variables.program env in
  let program2 = Environments.safe_lookup Builtin_variables.program2 env in
  let what = Printf.sprintf "Comparing %s programs %s and %s"
    (Backends.string_of_backend backend) program program2 in
  Printf.fprintf log "%s\n%!" what;
  let files = {
    Filecompare.reference_filename = program;
    Filecompare.output_filename = program2
  } in
  match Filecompare.compare_files ~tool:comparison_tool files with
    | Filecompare.Same -> Pass env

    | Filecompare.Different ->
      let reason = Printf.sprintf "Files %s and %s are different"
        program program2 in
      Fail reason
    | Filecompare.Unexpected_output -> assert false
    | Filecompare.Error (commandline, exitcode) ->
      let reason = mkreason what commandline exitcode in
      Fail reason

let make_bytecode_programs_comparison_tool ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let cmpbyt = cmpbyt ocamlsrcdir in
  let tool_name = ocamlrun ^ " " ^ cmpbyt in
  {
    Filecompare.tool_name = tool_name;
    Filecompare.tool_flags = "";
    Filecompare.result_of_exitcode = Filecompare.cmp_result_of_exitcode
  }

let nativecode_programs_comparison_tool = Filecompare.default_comparison_tool

let compare_bytecode_programs_body log env =
  let ocamlsrcdir = ocamlsrcdir () in
  let bytecode_programs_comparison_tool =
    make_bytecode_programs_comparison_tool ocamlsrcdir in
  compare_programs Sys.Bytecode bytecode_programs_comparison_tool log env

let compare_bytecode_programs = {
  action_name = "compare-bytecode-programs";
  action_environment = env_id;
  action_body = compare_bytecode_programs_body
}

let compare_nativecode_programs = {
  action_name = "compare-nativecode-programs";
  action_environment = env_id;
  action_body = compare_programs Sys.Native nativecode_programs_comparison_tool
}

let run_test_program_in_toplevel toplevel log env =
  let testfile = testfile env in
  let expected_exit_status = expected_compiler_exit_status env toplevel in
  let what = Printf.sprintf "Running %s in %s toplevel (expected exit status: %d)"
    testfile (Backends.string_of_backend toplevel.compiler_backend) expected_exit_status in
  Printf.fprintf log "%s\n%!" what;
  let testfile_basename = Filename.chop_extension testfile in
  let test_source_directory = test_source_directory env in
  let compilerreference_prefix =
    make_path [test_source_directory; testfile_basename] in
  let compilerreference_filename =
    compiler_reference_filename env compilerreference_prefix toplevel in
  let compiler_directory_suffix =
    Environments.safe_lookup Builtin_variables.compiler_directory_suffix env in
  let compiler_directory_name =
    toplevel.compiler_directory ^ compiler_directory_suffix in
  let compiler_directory =
    make_path [test_build_directory env; compiler_directory_name] in
  let compiler_output_filename =
    make_file_name toplevel.compiler_directory "output" in
  let compiler_output =
    make_path [compiler_directory; compiler_output_filename] in
  let compiler_output_variable = toplevel.compiler_output_variable in
  let compiler_reference_variable = toplevel.compiler_reference_variable in
  let newenv = Environments.add_bindings
    [
      (compiler_reference_variable, compilerreference_filename);
      (compiler_output_variable, compiler_output);
    ] env in
  Testlib.make_directory compiler_directory;
  setup_symlinks test_source_directory compiler_directory [testfile];
  setup_symlinks test_source_directory compiler_directory (files env);
  Sys.chdir compiler_directory;
  if Sys.file_exists compiler_output_filename then
    Sys.remove compiler_output_filename;
  let ocamlsrcdir = ocamlsrcdir () in
  let toplevel_name = toplevel.compiler_name ocamlsrcdir in
  let toplevel_default_flags = "-noinit -no-version -noprompt" in
  let commandline = String.concat " "
  [
    toplevel_name;
    toplevel_default_flags;
    stdlib_flags ocamlsrcdir;
    flags env;
  ] in
  let exit_status =
    run_command
      ~stdin_variable:Builtin_variables.test_file
      ~stdout_variable:compiler_output_variable
      ~stderr_variable:compiler_output_variable
      log newenv commandline in
  if exit_status=expected_exit_status
  then Pass newenv
  else Fail (mkreason what commandline exit_status)

let run_in_bytecode_toplevel =
{
  action_name = "run-in-bytecode-toplevel";
  action_environment = env_id;
  action_body = run_test_program_in_toplevel bytecode_toplevel;
}

let run_in_nativecode_toplevel =
{
  action_name = "run-in-nativecode-toplevel";
  action_environment = env_id;
  action_body = run_test_program_in_toplevel nativecode_toplevel;
}

let check_bytecode_toplevel_output = make_check_compiler_output
  "check-bytecode-toplevel-output" bytecode_toplevel

let check_nativecode_toplevel_output = make_check_compiler_output
  "check-nativecode-toplevel-output" nativecode_toplevel

let _ =
  List.iter register
  [
    compile_bytecode_with_bytecode_compiler;
    compile_bytecode_with_nativecode_compiler;
    compile_nativecode_with_bytecode_compiler;
    compile_nativecode_with_nativecode_compiler;
    execute;
    check_program_output;
    compare_bytecode_programs;
    compare_nativecode_programs;
    check_ocamlc_dot_byte_output;
    check_ocamlc_dot_opt_output;
    check_ocamlopt_dot_byte_output;
    check_ocamlopt_dot_opt_output;
    run_in_bytecode_toplevel;
    run_in_nativecode_toplevel;
    check_bytecode_toplevel_output;
    check_nativecode_toplevel_output;
  ]
