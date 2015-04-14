(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Description of primitive functions *)

open Misc

type native_float =
  [ `No           (* No unboxed floats in the inputs and the output *)
  | `Yes          (* Unboxed float inputs and the output *)
  | `Only_input ] (* Unboxed float inputs but not the output *)

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_float: native_float }
                               (* Does the above operate on unboxed floats? *)

let parse_native_float = function
  "float" -> `Yes
| "float-input" -> `Only_input
| _ -> assert false

let parse_declaration arity decl =
  match decl with
  | name :: "noalloc" :: name2 :: ("float" | "float-input" as f) :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = name2; prim_native_float = parse_native_float f}
  | name :: "noalloc" :: name2 :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = name2; prim_native_float = `No}
  | name :: name2 :: ("float" | "float-input" as f) :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = name2; prim_native_float = parse_native_float f}
  | name :: "noalloc" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = ""; prim_native_float = `No}
  | name :: name2 :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = name2; prim_native_float = `No}
  | name :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = ""; prim_native_float = `No}
  | [] ->
      fatal_error "Primitive.parse_declaration"

let description_list p =
  let list = [p.prim_name] in
  let list = if not p.prim_alloc then "noalloc" :: list else list in
  let list =
    if p.prim_native_name <> "" then p.prim_native_name :: list else list
  in
  let list =
    match p.prim_native_float with
      `No -> list
    | `Yes -> "float" :: list
    | `Only_input -> "float-input" :: list
  in
  List.rev list

let native_name p =
  if p.prim_native_name <> ""
  then p.prim_native_name
  else p.prim_name

let byte_name p =
  p.prim_name
