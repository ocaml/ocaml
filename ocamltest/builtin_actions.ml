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
  ?(stdout_variable="stdout")
  ?(stderr_variable="")
  ?(append=false)
  ?(timeout=0)
  log env cmd =
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
  let stdout_filename = Environments.safe_lookup stdout_variable env in
  let stderr_filename = Environments.safe_lookup stderr_variable env in
  Run.run {
    Run.progname = progname;
    Run.argv = arguments;
    (* Run.envp = environment; *)
    Run.stdout_filename = stdout_filename;
    Run.stderr_filename = stderr_filename;
    Run.append = append;
    Run.timeout = timeout;
    Run.log = log
  }

let mkreason what commandline exitcode =
  Printf.sprintf "%s: command\n%s\nfailed with exit code %d"
    what commandline exitcode

let make_file_name name ext = name ^ "." ^ ext

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

let cmpbyt ocamlsrcdir =
  make_path [ocamlsrcdir; "tools"; "cmpbyt"]

let stdlib ocamlsrcdir =
  make_path [ocamlsrcdir; "stdlib"]

let stdlib_flags ocamlsrcdir =
  let stdlib_path = stdlib ocamlsrcdir in
  "-nostdlib -I " ^ stdlib_path

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
  compilerreference_variable : string;
  compileroutput_variable : string
}

(* Compilers compiling byte-code programs *)

let bytecode_bytecode_compiler =
{
  compiler_name = ocamlc_dot_byte;
  compiler_directory = "ocamlc.byte";
  compiler_backend = Sys.Bytecode;
  compilerreference_variable = "compilerreference";
  compileroutput_variable = "compileroutput"
}

let bytecode_nativecode_compiler =
{
  compiler_name = ocamlc_dot_opt;
  compiler_directory = "ocamlc.opt";
  compiler_backend = Sys.Bytecode;
  compilerreference_variable = "compilerreference2";
  compileroutput_variable = "compileroutput2"
}

(* Compilers compiling native-code programs *)

let nativecode_bytecode_compiler =
{
  compiler_name = ocamlopt_dot_byte;
  compiler_directory = "ocamlopt.byte";
  compiler_backend = Sys.Native;
  compilerreference_variable = "compilerreference";
  compileroutput_variable = "compileroutput"
}

let nativecode_nativecode_compiler =
{
  compiler_name = ocamlopt_dot_opt;
  compiler_directory = "ocamlopt.opt";
  compiler_backend = Sys.Native;
  compilerreference_variable = "compilerreference2";
  compileroutput_variable = "compileroutput2"
}

let compiler_reference_filename prefix compiler =
  let suffix = "reference" in
  let mk s = make_file_name (make_file_name prefix s) suffix in
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

let testfile env = match Environments.lookup "testfile" env with
  | None -> assert false
  | Some t -> t

let modules env = Testlib.words (Environments.safe_lookup "modules" env)

let files env = Testlib.words (Environments.safe_lookup "files" env)

let flags env = Environments.safe_lookup "flags" env

let libraries env = Environments.safe_lookup "libraries" env

let backend_flags env =
  get_backend_value_from_env env "bcflags" "ncflags"

let test_source_directory env = Environments.safe_lookup "testsrcdir" env

let test_build_directory env = Environments.safe_lookup "testbuilddir" env

let compile_module ocamlsrcdir compilername compileroutput backend log env module_name =
  let what = Printf.sprintf "Compiling %s module %s"
    (Backends.string_of_backend backend) module_name in
  Printf.fprintf log "%s\n%!" what;
  let module_base_name = Filename.chop_extension module_name in
  let module_extension = Backends.module_extension backend in
  let module_output_name = make_file_name module_base_name module_extension in
  let output = "-o " ^ module_output_name in
  let compile_commandline = String.concat " "
  [
    compilername;
    stdlib_flags ocamlsrcdir;
    flags env;
    backend_flags env backend;
    libraries env;
    "-c";
  ] in
  let interface_result = match Filetype.filetype module_name with
    | (basename, Filetype.Implementation) ->
      let interface_name =
        Filetype.make_filename basename Filetype.Interface in
      if Sys.file_exists interface_name then
      begin
        let what = "Compiling interface " ^ interface_name in
        let commandline = compile_commandline ^ " " ^ interface_name in
        match
          run_command
            ~stdout_variable:compileroutput
            ~stderr_variable:compileroutput
            ~append:true
            log env commandline
        with
          | 0 -> Ok interface_name
          | _ as exitcode -> Error (mkreason what commandline exitcode)
      end else (Ok ("Module " ^ " has no interface"))
    | _ -> (Error ("ocamltest does not know how to compile " ^ module_name)) in
  match interface_result with
    | Error _ -> interface_result
    | Ok _ ->
      begin
        let commandline = String.concat " "
        [
          compile_commandline;
          module_name;
          output
        ] in
        match
          run_command
            ~stdout_variable:compileroutput
            ~stderr_variable:compileroutput
            ~append:true
            log env commandline
        with
          | 0 -> Ok module_output_name
          | _ as exitcode -> Error (mkreason what commandline exitcode)
      end

let compile_modules ocamlsrcdir compilername compileroutput backend log env module_names =
  let cons x xs = x::xs in
  map_reduce_result
    (compile_module ocamlsrcdir compilername compileroutput backend log env)
    cons
    []
    module_names

let link_modules ocamlsrcdir compilername compileroutput program_variable backend log env modules =
  let executable_name = match Environments.lookup program_variable env with
    | None -> assert false
    | Some program -> program in
  let module_names = String.concat " " modules in
  let what = Printf.sprintf "Linking modules %s into %s"
    module_names executable_name in
  Printf.fprintf log "%s\n%!" what;
  let output = "-o " ^ executable_name in
  let commandline = String.concat " "
  [
    compilername;
    use_runtime backend ocamlsrcdir;
    stdlib_flags ocamlsrcdir;
    flags env;
    libraries env;
    backend_flags env backend;
    module_names;
    output
  ] in
  match
    run_command
      ~stdout_variable:compileroutput
      ~stderr_variable:compileroutput
      ~append:true
      log env commandline
  with
    | 0 -> Ok ()
    | _ as exitcode -> Error (mkreason what commandline exitcode)

let compile_program ocamlsrcdir compilername compileroutput program_variable backend log env modules =
  match
    compile_modules ocamlsrcdir compilername compileroutput log backend env modules
  with
    | Ok module_binaries ->
      (match link_modules ocamlsrcdir compilername compileroutput program_variable log backend env module_binaries with
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

let compile_test_program program_variable compiler backend log env =
  let testfile = testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let executable_filename =
    make_file_name testfile_basename (Backends.executable_extension backend) in
  let modules = (modules env) @ [testfile] in
  let test_source_directory = test_source_directory env in
  let module_interfaces =
    find_module_interfaces test_source_directory modules in
  let compilerreference_prefix =
    make_path [test_source_directory; testfile_basename] in
  let compilerreference_filename =
    compiler_reference_filename compilerreference_prefix compiler in
  let build_directory =
    make_path [test_build_directory env; compiler.compiler_directory] in
  let executable_path = make_path [build_directory; executable_filename] in
  let compiler_output_filename =
    make_file_name compiler.compiler_directory "output" in
  let compiler_output =
    make_path [build_directory; compiler_output_filename] in
  let compileroutput_variable = compiler.compileroutput_variable in
  let compilerreference_variable = compiler.compilerreference_variable in
  let newenv = Environments.add_variables
    [
      (program_variable, executable_path);
      (compilerreference_variable, compilerreference_filename);
      (compileroutput_variable, compiler_output);
    ] env in
  Testlib.make_directory build_directory;
  setup_symlinks test_source_directory build_directory modules;
  setup_symlinks test_source_directory build_directory module_interfaces;
  setup_symlinks test_source_directory build_directory (files env);
  Sys.chdir build_directory;
  if Sys.file_exists compiler_output_filename then
    Sys.remove compiler_output_filename;
  let ocamlsrcdir = ocamlsrcdir () in
  let compilername = compiler.compiler_name ocamlsrcdir in
  compile_program
    ocamlsrcdir
    compilername
    compileroutput_variable
    program_variable log backend newenv modules

(* Compile actions *)

let compile_bytecode_with_bytecode_compiler = {
  action_name = "compile-bytecode-with-bytecode-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program "program" bytecode_bytecode_compiler Sys.Bytecode
}

let compile_bytecode_with_nativecode_compiler = {
  action_name = "compile-bytecode-with-nativecode-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program "program2" bytecode_nativecode_compiler Sys.Bytecode
}

let compile_nativecode_with_bytecode_compiler = {
  action_name = "compile-nativecode-with-bytecode-compiler";
  action_environment = env_id;
  action_body =
  compile_test_program "program" nativecode_bytecode_compiler Sys.Native
}

let compile_nativecode_with_nativecode_compiler = {
  action_name = "compile-nativecode-with-nativecode-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program "program2" nativecode_nativecode_compiler Sys.Native
}

let execute_program log env =
  match Environments.lookup "program" env with
  | None -> failwith "No program to execute"
  | Some program ->
    let arguments = Environments.safe_lookup "arguments" env in
    let commandline = program ^ " " ^ arguments in
    let what = "Executing program " ^ program ^ " " ^
    begin if arguments="" then "without any argument"
    else "with arguments " ^ arguments
    end in
    let output = program ^ ".output" in
    let bindings =
    [
      "stdout", output;
      "stderr", output
    ] in
    let env' = Environments.add_variables bindings env in
    match run_command log env' commandline with
      | 0 -> Pass (Environments.add "output" output env)
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
      compiler.compileroutput_variable
      compiler.compilerreference_variable
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
  action_body = check_output "program" "output" "reference"
}

let compare_programs backend comparison_tool log env =
  let program = Environments.safe_lookup "program" env in
  let program2 = Environments.safe_lookup "program2" env in
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
    check_ocamlopt_dot_opt_output
  ]
