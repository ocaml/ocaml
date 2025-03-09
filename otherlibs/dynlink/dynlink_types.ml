(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*             Mark Shinwell and Leo White, Jane Street Europe            *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Types shared amongst the various parts of the dynlink code. *)

type implem_state =
  | Loaded
  | Not_initialized
  | Check_inited of int

type filename = string

type linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error =
  | Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | Cannot_open_dynamic_library of exn
  | Library's_module_initializers_failed of exn
  | Inconsistent_implementation of string
  | Module_already_loaded of string
  | Private_library_cannot_implement_interface of string

exception Error of error

let error_message = function
  | Not_a_bytecode_file name ->
    name ^ " is not an object file"
  | Inconsistent_import name ->
    "interface mismatch on " ^ name
  | Unavailable_unit name ->
    "no implementation available for " ^ name
  | Unsafe_file ->
    "this object file uses unsafe features"
  | Linking_error (name, Undefined_global s) ->
    "error while linking " ^ name ^ ".\n" ^
      "Reference to undefined global `" ^ s ^ "'"
  | Linking_error (name, Unavailable_primitive s) ->
    "error while linking " ^ name ^ ".\n" ^
      "The external function `" ^ s ^ "' is not available"
  | Linking_error (name, Uninitialized_global s) ->
    "error while linking " ^ name ^ ".\n" ^
      "The module `" ^ s ^ "' is not yet initialized"
  | Corrupted_interface name ->
    "corrupted interface file " ^ name
  | Cannot_open_dynamic_library exn ->
    "error loading shared library: " ^ (Printexc.to_string exn)
  | Inconsistent_implementation name ->
    "implementation mismatch on " ^ name
  | Library's_module_initializers_failed exn ->
    "execution of module initializers in the shared library failed: "
      ^ (Printexc.to_string exn)
  | Module_already_loaded name ->
    "The module `" ^ name ^ "' is already loaded \
      (either by the main program or a previously-dynlinked library)"
  | Private_library_cannot_implement_interface name ->
    "The interface `" ^ name ^ "' cannot be implemented by a \
      library loaded privately"

let () =
  Printexc.register_printer (function
    | Error err ->
      let msg = match err with
      | Not_a_bytecode_file s -> Printf.sprintf "Not_a_bytecode_file %S" s
      | Inconsistent_import s -> Printf.sprintf "Inconsistent_import %S" s
      | Unavailable_unit s -> Printf.sprintf "Unavailable_unit %S" s
      | Unsafe_file -> "Unsafe_file"
      | Linking_error (s, Undefined_global s') ->
        Printf.sprintf "Linking_error (%S, Dynlink.Undefined_global %S)"
          s s'
      | Linking_error (s, Unavailable_primitive s') ->
        Printf.sprintf "Linking_error (%S, Dynlink.Unavailable_primitive %S)"
          s s'
      | Linking_error (s, Uninitialized_global s') ->
        Printf.sprintf "Linking_error (%S, Dynlink.Uninitialized_global %S)"
          s s'
      | Corrupted_interface s ->
        Printf.sprintf "Corrupted_interface %S" s
      | Cannot_open_dynamic_library exn ->
        Printf.sprintf "Cannot_open_dll %S" (Printexc.to_string exn)
      | Inconsistent_implementation s ->
        Printf.sprintf "Inconsistent_implementation %S" s
      | Library's_module_initializers_failed exn ->
        Printf.sprintf "Library's_module_initializers_failed %S"
          (Printexc.to_string exn)
      | Module_already_loaded name ->
        Printf.sprintf "Module_already_loaded %S" name
      | Private_library_cannot_implement_interface name ->
        Printf.sprintf "Private_library_cannot_implement_interface %S" name
      in
      Some (Printf.sprintf "Dynlink.Error (Dynlink.%s)" msg)
    | _ -> None)
