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

let filename_concat components = List.fold_left Filename.concat "" components

let ocamlsrcdir () =
  try Sys.getenv "OCAMLSRCDIR"
  with Not_found -> failwith "The OCAMLSRCDIR environment variable is not set"

let ocamlrun ocamlsrcdir =
  filename_concat [ocamlsrcdir; "byterun"; "ocamlrun"]

let ocamlc ocamlsrcdir =
  filename_concat [ocamlsrcdir; "ocamlc"]

let ocamlopt ocamlsrcdir =
  filename_concat [ocamlsrcdir; "ocamlopt"]

let stdlib ocamlsrcdir =
  filename_concat [ocamlsrcdir; "stdlib"]

let add_stdlib_flags ocamlsrcdir env = match Environments.lookup "stdlibflags" env with
  | Some _ -> env
  | None ->
    let stdlib_path = stdlib ocamlsrcdir in
    let stdlibflags = "-nostdlib -I " ^ stdlib_path in
    Environments.add "stdlibflags" stdlibflags env

let add_ocamlrun ocamlrun env = match Environments.lookup "ocamlrun" env with
  | Some _ -> env
  | None ->
    Environments.add "ocamlrun" ocamlrun env

let bytecode_environment env =
  let dir = ocamlsrcdir() in
  let ocamlrun = ocamlrun dir in
  let ocamlc = ocamlc dir in
  let envwithstdlibflags = add_stdlib_flags dir env in
  let env_with_ocamlrun = add_ocamlrun ocamlrun envwithstdlibflags in
  Environments.add "ocamlc" (ocamlrun ^ " " ^ ocamlc) env_with_ocamlrun

let nativecode_environment env =
  let dir = ocamlsrcdir() in
  let ocamlrun = ocamlrun dir in
  let ocamlopt = ocamlopt dir in
  let envwithstdlibflags = add_stdlib_flags dir env in
  let env_with_ocamlrun = add_ocamlrun ocamlrun envwithstdlibflags in
  Environments.add "ocamlopt" (ocamlrun ^ " " ^ ocamlopt) env_with_ocamlrun

let use_runtime env = function
  | Sys.Native -> ""
  | Sys.Bytecode ->
    begin match Environments.lookup "ocamlrun" env with
      | None -> ""
      | Some runtime ->"-use-runtime " ^ runtime
    end
  | Sys.Other _ -> assert false

(*
let file_extension filename =
  let l = String.length filename in
  let pos_dot = ref (l-1) in
  while !pos_dot >= 0 && (filename.[!pos_dot] <> '.'); do decr pos_dot; done;
  if !pos_dot < 0 then ""
  else String.sub filename (!pos_dot+1) (l - !pos_dot -1)
*)

(*
type file_type =
  | Implementation
  | Interface
  | C
  | C_minor
  | Lexer
  | Grammar
*)

(* exception Unknown_file_extension of string *)

(*
let file_type filename =
  match (file_extension filename) with
  | "ml" -> Implementation
  | "mli" -> Interface
  | "c" -> C
  | "cmm" -> C_minor
  | "mll" -> Lexer
  | "mly" -> Grammar
  | _ as ext -> raise (Unknown_file_extension ext)
*)

let get_modules env =
  let modules = Testlib.words (Environments.safe_lookup "modules" env) in
  modules

let mkfilename name ext = name ^ "." ^ ext

let testfile env = match Environments.lookup "testfile" env with
  | None -> assert false
  | Some t -> t

let common_flags env = Environments.safe_lookup "flags" env

let get_backend_value_from_env env bytecode_var native_var =
  Backends.make_backend_function
    (Environments.safe_lookup bytecode_var env)
    (Environments.safe_lookup native_var env)

let compiler env = get_backend_value_from_env env "ocamlc" "ocamlopt"

let linker env = get_backend_value_from_env env "ocamlc" "ocamlopt"

let backend_flags env =
  get_backend_value_from_env env "bcflags" "ncflags"

let libraries env = Environments.safe_lookup "libraries" env

let stdlib_flags env = Environments.safe_lookup "stdlibflags" env

let mkreason what commandline exitcode =
  Printf.sprintf "%s: command\n%s\nfailed with exit code %d"
    what commandline exitcode

let compile_module backend log env module_name =
  let what = Printf.sprintf "Compiling %s module %s"
    (Backends.string_of_backend backend) module_name in
  Printf.fprintf log "%s\n%!" what;
  let compile = "-c " ^ module_name in
  let module_base_name = Filename.chop_extension module_name in
  let module_extension = Backends.module_extension backend in
  let module_output_name = mkfilename module_base_name module_extension in
  let output = "-o " ^ module_output_name in
  let commandline = String.concat " "
  [
    compiler env backend;
    stdlib_flags env;
    common_flags env;
    backend_flags env backend;
    compile;
    output
  ] in
  match run_command log env commandline with
    | 0 -> Ok module_output_name
    | _ as exitcode -> Error (mkreason what commandline exitcode)

let rec fold_left_result f g init = function
  | [] -> Ok init
  | x::xs ->
    (match f x with
      | Ok fx ->
        (match fold_left_result f g init xs with
          | Ok fxs -> Ok (g fx fxs)
          | Error _ as e -> e
        )
      | Error _ as e -> e
    )

let compile_modules backend log env module_names =
  let cons x xs = x::xs in
  fold_left_result
    (compile_module backend log env)
    cons
    []
    module_names

let link_modules backend log env modules =
  let executable_name = match Environments.lookup "program" env with
    | None -> assert false
    | Some program -> program in
  let module_names = String.concat " " modules in
  let what = Printf.sprintf "Linking modules %s into %s"
    module_names executable_name in
  Printf.fprintf log "%s\n%!" what;
  let output = "-o " ^ executable_name in
  let commandline = String.concat " "
  [
    linker env backend;
    use_runtime env backend;
    stdlib_flags env;
    common_flags env;
    libraries env;
    backend_flags env backend;
    module_names;
    output
  ] in
  match run_command log env commandline with
    | 0 -> Ok ()
    | _ as exitcode -> Error (mkreason what commandline exitcode)

let compile_program backend log env modules =
  match compile_modules log backend env modules with
    | Ok module_binaries ->
      (match link_modules log backend env module_binaries with
        | Ok _ -> Pass env
        | Error reason -> Fail reason
      )
    | Error reason -> Fail reason

let compile_test_program backend log env =
  let testfile = testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let executable_filename =
    mkfilename testfile_basename (Backends.executable_extension backend) in
  let newenv = Environments.add "program" executable_filename env in
  let modules = (get_modules env) @ [testfile] in
  compile_program log backend newenv modules

let bytecode_compile = {
  action_name = "bytecode-compile";
  action_environment = bytecode_environment;
  action_body = compile_test_program Sys.Bytecode
}

let nativecode_compile = {
  action_name = "nativecode-compile";
  action_environment = nativecode_environment;
  action_body = compile_test_program Sys.Native
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

let env_id env = env

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

let _ =
  List.iter register
  [
    bytecode_compile;
    nativecode_compile;
    execute;
    check_program_output
  ]
