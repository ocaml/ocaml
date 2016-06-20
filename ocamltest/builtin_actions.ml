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
  Run.run {
    Run.progname = progname;
    Run.argv = arguments;
    (* Run.envp = environment; *)
    Run.stdout_filename = Environments.safe_lookup stdout_variable env;
    Run.stderr_filename = Environments.safe_lookup stderr_variable env;
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
  compiler_directory : string
}

(* Compilers compiling byte-code programs *)

let bytecode_bytecode_compiler =
{
  compiler_name = ocamlc_dot_byte;
  compiler_directory = "ocamlc.byte"
}

let bytecode_nativecode_compiler =
{
  compiler_name = ocamlc_dot_opt;
  compiler_directory = "ocamlc.opt"
}

(* Compilers compiling native-code programs *)

let nativecode_bytecode_compiler =
{
  compiler_name = ocamlopt_dot_byte;
  compiler_directory = "ocamlopt.byte"
}

let nativecode_nativecode_compiler =
{
  compiler_name = ocamlopt_dot_opt;
  compiler_directory = "ocamlopt.opt"
}

(* Extracting information from environment *)

let get_backend_value_from_env env bytecode_var native_var =
  Backends.make_backend_function
    (Environments.safe_lookup bytecode_var env)
    (Environments.safe_lookup native_var env)

let testfile env = match Environments.lookup "testfile" env with
  | None -> assert false
  | Some t -> t

let modules env = Testlib.words (Environments.safe_lookup "modules" env)

let flags env = Environments.safe_lookup "flags" env

let libraries env = Environments.safe_lookup "libraries" env

let backend_flags env =
  get_backend_value_from_env env "bcflags" "ncflags"

let test_source_directory env = Environments.safe_lookup "testsrcdir" env

let test_build_directory env = Environments.safe_lookup "testbuilddir" env

let compile_module ocamlsrcdir compilername backend log env module_name =
  let what = Printf.sprintf "Compiling %s module %s"
    (Backends.string_of_backend backend) module_name in
  Printf.fprintf log "%s\n%!" what;
  let compile = "-c " ^ module_name in
  let module_base_name = Filename.chop_extension module_name in
  let module_extension = Backends.module_extension backend in
  let module_output_name = make_file_name module_base_name module_extension in
  let output = "-o " ^ module_output_name in
  let commandline = String.concat " "
  [
    compilername;
    stdlib_flags ocamlsrcdir;
    flags env;
    backend_flags env backend;
    libraries env;
    compile;
    output
  ] in
  match run_command log env commandline with
    | 0 -> Ok module_output_name
    | _ as exitcode -> Error (mkreason what commandline exitcode)

let compile_modules ocamlsrcdir compilername backend log env module_names =
  let cons x xs = x::xs in
  map_reduce_result
    (compile_module ocamlsrcdir compilername backend log env)
    cons
    []
    module_names

let link_modules ocamlsrcdir compilername program_variable backend log env modules =
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
  match run_command log env commandline with
    | 0 -> Ok ()
    | _ as exitcode -> Error (mkreason what commandline exitcode)

let compile_program ocamlsrcdir compilername program_variable backend log env modules =
  match compile_modules ocamlsrcdir compilername log backend env modules with
    | Ok module_binaries ->
      (match link_modules ocamlsrcdir compilername program_variable log backend env module_binaries with
        | Ok _ -> Pass env
        | Error reason -> Fail reason
      )
    | Error reason -> Fail reason

let compile_test_program program_variable compiler backend log env =
  let testfile = testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let executable_filename =
    make_file_name testfile_basename (Backends.executable_extension backend) in
  let modules = (modules env) @ [testfile] in
  let test_source_directory = test_source_directory env in
  let build_directory =
    make_path [test_build_directory env; compiler.compiler_directory] in
  let executable_path = make_path [build_directory; executable_filename] in
  let newenv = Environments.add program_variable executable_path env in
  Testlib.make_directory build_directory;
  setup_symlinks test_source_directory build_directory modules;
  Sys.chdir build_directory;
  let ocamlsrcdir = ocamlsrcdir () in
  let compilername = compiler.compiler_name ocamlsrcdir in
  compile_program ocamlsrcdir compilername program_variable log backend newenv modules

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

let check_prog_output log env =
  let reference_filename = Environments.safe_lookup "reference" env in
  let output_filename = Environments.safe_lookup "output" env in
  Printf.fprintf log "Comparing program output %s to reference %s\n%!"
    output_filename reference_filename;
  let files =
  {
    Filecompare.reference_filename = reference_filename;
    Filecompare.output_filename = output_filename
  } in
  match Filecompare.check_file files with
    | Filecompare.Same -> Pass env
    | Filecompare.Different ->
      let reason = Printf.sprintf "Program output %s differs from reference %s"
        output_filename reference_filename in
      (Actions.Fail reason)
    | Filecompare.Unexpected_output ->
      let reason = Printf.sprintf "The file %s was expected to be empty because there is no reference file %s but it is not"
        output_filename reference_filename in
      (Actions.Fail reason)
    | Filecompare.Error (commandline, exitcode) ->
      let reason = Printf.sprintf "The command %s failed with status %d"
        commandline exitcode in
      (Actions.Fail reason)

let check_program_output = {
  action_name = "check-program-output";
  action_environment = env_id;
  action_body = check_prog_output
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
    compare_nativecode_programs
  ]
