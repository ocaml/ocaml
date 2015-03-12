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

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_float: bool;   (* Does the above operate on unboxed floats? *)
    prim_asm: Inline_asm.inline_asm option (* Is it an inline asm block *) }

let parse_declaration arity decl =
  match decl with
  | name :: "noalloc" :: name2 :: "float" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = name2; prim_native_float = true; prim_asm = None}
  | name :: "noalloc" :: name2 :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = name2; prim_native_float = false; prim_asm = None}
  | name :: name2 :: "float" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = name2; prim_native_float = true; prim_asm = None}
  | name :: "noalloc" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = ""; prim_native_float = false; prim_asm = None}
  | name :: name2 :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = name2; prim_native_float = false; prim_asm = None}
  | name :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = ""; prim_native_float = false; prim_asm = None}
  | [] ->
      fatal_error "Primitive.parse_declaration"

let description_list p =
  let list = [p.prim_name] in
  let list = if not p.prim_alloc then "noalloc" :: list else list in
  let list =
    if p.prim_native_name <> "" then p.prim_native_name :: list else list
  in
  let list = if p.prim_native_float then "float" :: list else list in
  let list = List.rev list in
  match p.prim_asm with
  | None -> list
  | Some inline_asm -> list @ (Inline_asm.description inline_asm)

let native_name p =
  if p.prim_native_name <> ""
  then p.prim_native_name
  else p.prim_name

let byte_name p =
  p.prim_name
