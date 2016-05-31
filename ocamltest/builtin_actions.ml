
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

type backend = Bytecode | Native

let backend_extension = function
  | Bytecode -> "cmo"
  | Native -> "cmx"

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

let noop env = Pass env

let get_modules env =
  let modules = Testlib.words (Environments.safe_lookup "modules" env) in
  modules

let mkfilename name ext = name ^"." ^ext

let rec files_generated_when_compiling module_base_name module_type backend =
  let mkmodname ext = mkfilename module_base_name ext in
  match module_type with
    | Implementation ->
      let extension = backend_extension backend in
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

let executable_extension = function
  | Bytecode -> "byte"
  | Native -> "opt"

let generated_files backend env =
  let testfile = match Environments.lookup "testfile" env with
    | None -> assert false
    | Some t -> t in
  let testfile_basename = Filename.chop_extension testfile in
  let executable_filename = mkfilename testfile_basename (executable_extension backend) in
  let modules = (get_modules env) @ [testfile] in
  let f modul files =
    let mod_name = Filename.chop_extension modul in
    let mod_type = file_type modul in
    (files_generated_when_compiling mod_name mod_type backend) @ files in
  let l = List.fold_right f modules [] in
  l @ [executable_filename]

let bytecode_compile_generated_files = generated_files Bytecode

let nativecode_compile_generated_files = generated_files Native

let bytecode_compile = {
  action_name = "bytecode-compile";
  action_generated_files = bytecode_compile_generated_files;
  action_body = noop
}

let nativecode_compile = {
  action_name = "nativecode-compile";
  action_generated_files = nativecode_compile_generated_files;
  action_body = noop
}

let execute = {
  action_name = "execute";
  action_generated_files = no_generated_files;
  action_body = noop
}

let reg act = register act.action_name act.action_generated_files act.action_body

let _ =
  reg bytecode_compile;
  reg nativecode_compile;
  reg execute
