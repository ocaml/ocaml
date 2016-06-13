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
  ppf env cmd =
  let lst = Testlib.words cmd in
  let cmd' = String.concat " " lst in
  Format.fprintf ppf "Commandline: %s\n" cmd';
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
    Run.timeout = timeout
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
  | Other _ -> assert false

let file_extension filename =
  let l = String.length filename in
  let pos_dot = ref (l-1) in
  while !pos_dot >= 0 && (filename.[!pos_dot] <> '.'); do decr pos_dot; done;
  if !pos_dot < 0 then ""
  else String.sub filename (!pos_dot+1) (l - !pos_dot -1)

type file_type =
  | Implementation
  | Interface
  | C
  | C_minor
  | Lexer
  | Grammar

exception Unknown_file_extension of string

let file_type filename =
  match (file_extension filename) with
  | "ml" -> Implementation
  | "mli" -> Interface
  | "c" -> C
  | "cmm" -> C_minor
  | "mll" -> Lexer
  | "mly" -> Grammar
  | _ as ext -> raise (Unknown_file_extension ext)

let noop ppf env = Pass env

let get_modules env =
  let modules = Testlib.words (Environments.safe_lookup "modules" env) in
  modules

let mkfilename name ext = name ^ "." ^ ext

let rec files_generated_when_compiling module_base_name module_type backend =
  let mkmodname ext = mkfilename module_base_name ext in
  match module_type with
    | Implementation ->
      let extension = Backends.module_extension backend in
      [mkmodname extension]
    | Interface -> [mkmodname "cmi"]
    | C | C_minor -> assert false
    | Lexer ->
      let l = files_generated_when_compiling module_base_name Implementation backend in
      (mkmodname "ml") :: l
    | Grammar ->
      let l1 = files_generated_when_compiling module_base_name Interface backend in
      let l2 = files_generated_when_compiling module_base_name Implementation backend in
      (mkmodname "ml") :: (mkmodname "mli") :: (l1 @ l2)

let testfile env = match Environments.lookup "testfile" env with
  | None -> assert false
  | Some t -> t

let generated_files backend env =
  let testfile = testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let executable_filename =
    mkfilename testfile_basename (Backends.executable_extension backend) in
  let modules = (get_modules env) @ [testfile] in
  let f modul files =
    let mod_name = Filename.chop_extension modul in
    let mod_type = file_type modul in
    (files_generated_when_compiling mod_name mod_type backend) @ files in
  let l = List.fold_right f modules [] in
  l @ [executable_filename]

let bytecode_compile_generated_files = generated_files Sys.Bytecode

let nativecode_compile_generated_files = generated_files Sys.Native

let common_flags env = Environments.safe_lookup "flags" env

let get_backend_value_from_env env bytecode_var native_var =
  Backends.make_backend_function
    (Environments.safe_lookup bytecode_var env)
    (Environments.safe_lookup native_var env)

let compiler env = get_backend_value_from_env env "ocamlc" "ocamlopt"

let linker env = get_backend_value_from_env env "ocamlc" "ocamlopt"

let backend_flags env =
  get_backend_value_from_env env "bcflags" "ncflags"

let libraries env = get_backend_value_from_env env "bclibs" "nclibs"

let stdlib_flags env = Environments.safe_lookup "stdlibflags" env

let mkreason what commandline exitcode =
  Format.sprintf "%s: command\n%s\nfailed with exit code %d"
    what commandline exitcode

let compile_module backend ppf env module_name =
  let what = Format.sprintf "Compiling %s module %s"
    (Backends.string_of_backend backend) module_name in
  Format.fprintf ppf "%s\n%!" what;
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
  match run_command ppf env commandline with
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

let compile_modules backend ppf env module_names =
  let cons x xs = x::xs in
  fold_left_result
    (compile_module backend ppf env)
    cons
    []
    module_names

let link_modules backend ppf env modules =
  let executable_name = match Environments.lookup "program" env with
    | None -> assert false
    | Some program -> program in
  let module_names = String.concat " " modules in
  let what = Format.sprintf "Linking modules %s into %s"
    module_names executable_name in
  Format.fprintf ppf "%s\n%!" what;
  let output = "-o " ^ executable_name in
  let commandline = String.concat " "
  [
    linker env backend;
    use_runtime env backend;
    stdlib_flags env;
    common_flags env;
    backend_flags env backend;
    module_names;
    output
  ] in
  match run_command ppf env commandline with
    | 0 -> Ok ()
    | _ as exitcode -> Error (mkreason what commandline exitcode)

let compile_program backend ppf env modules =
  match compile_modules ppf backend env modules with
    | Ok module_binaries ->
      (match link_modules ppf backend env module_binaries with
        | Ok _ -> Pass env
        | Error reason -> Fail reason
      )
    | Error reason -> Fail reason

let compile_test_program backend ppf env =
  let testfile = testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let executable_filename =
    mkfilename testfile_basename (Backends.executable_extension backend) in
  let newenv = Environments.add "program" executable_filename env in
  let modules = (get_modules env) @ [testfile] in
  compile_program ppf backend newenv modules

let bytecode_compile = {
  action_name = "bytecode-compile";
  action_generated_files = bytecode_compile_generated_files;
  action_environment = bytecode_environment;
  action_body = compile_test_program Sys.Bytecode
}

let nativecode_compile = {
  action_name = "nativecode-compile";
  action_generated_files = nativecode_compile_generated_files;
  action_environment = nativecode_environment;
  action_body = compile_test_program Sys.Native
}

let execute_program ppf env =
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
    let env = Environments.add_variables bindings env in
    match run_command ppf env commandline with
      | 0 -> Pass env
      | _ as exitcode -> Fail (mkreason what commandline exitcode)

let env_id env = env

let execute = {
  action_name = "execute";
  action_generated_files = no_generated_files;
  action_environment = env_id;
  action_body = execute_program
}

let _ =
  register bytecode_compile;
  register nativecode_compile;
  register execute
