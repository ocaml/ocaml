(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Description of primitive functions *)

open Format

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_float: bool }  (* Does the above operate on unboxed floats? *)

let parse_declaration arity decl =
  match decl with
    name :: "noalloc" :: name2 :: "float" :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = false;
           prim_native_name = name2; prim_native_float = true}
  | name :: "noalloc" :: name2 :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = false;
           prim_native_name = name2; prim_native_float = false}
  | name :: name2 :: "float" :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = true;
           prim_native_name = name2; prim_native_float = true}
  | name :: "noalloc" :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = false;
           prim_native_name = ""; prim_native_float = false}
  | name :: name2 :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = true;
           prim_native_name = name2; prim_native_float = false}
  | name :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = true;
           prim_native_name = ""; prim_native_float = false}
  | [] ->
      None

let print_quoted s = print_char '"'; print_string s; print_char '"'

let print_description p =
  print_quoted p.prim_name;
  if not p.prim_alloc then
    (print_space(); print_quoted "noalloc");
  if p.prim_native_name <> "" then
    (print_space(); print_quoted p.prim_native_name);
  if p.prim_native_float then
    (print_space(); print_quoted "float")
