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
open Parsetree

type boxed_integer = Pnativeint | Pint32 | Pint64

type native_repr =
  | Same_as_ocaml_repr
  | Unboxed_float
  | Unboxed_integer of boxed_integer
  | Untagged_int

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_repr_args: native_repr list;
    prim_native_repr_res: native_repr }

type error =
  | Float_with_native_repr_attribute

exception Error of Location.t * error

let is_ocaml_repor = function
  | Same_as_ocaml_repr -> true
  | Unboxed_float
  | Unboxed_integer _
  | Untagged_int -> false

let rec make_native_repr_args arity x =
  if arity = 0 then
    []
  else
    x :: make_native_repr_args (arity - 1) x

let simple ~name ~arity ~alloc =
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = alloc;
   prim_native_name = "";
   prim_native_repr_args = make_native_repr_args arity Same_as_ocaml_repr;
   prim_native_repr_res = Same_as_ocaml_repr}

let parse_declaration valdecl ~native_repr_args ~native_repr_res =
  let arity = List.length native_repr_args in
  let name, native_name, noalloc, float =
    match valdecl.pval_prim with
    | name :: "noalloc" :: name2 :: "float" :: _ -> (name, name2, true, true)
    | name :: "noalloc" :: name2 :: _ -> (name, name2, true, false)
    | name :: name2 :: "float" :: _ -> (name, name2, false, true)
    | name :: "noalloc" :: _ -> (name, "", true, false)
    | name :: name2 :: _ -> (name, name2, false, false)
    | name :: _ -> (name, "", false, false)
    | [] ->
        fatal_error "Primitive.parse_declaration"
  in
  (* The compiler used to assume "noalloc" with "float", we just make this
     explicit now (GPR#167): *)
  let noalloc = noalloc || float in
  if float &&
     not (List.for_all is_ocaml_repor native_repr_args &&
          is_ocaml_repor native_repr_res) then
    raise (Error (valdecl.pval_loc, Float_with_native_repr_attribute));
  let native_repr_args, native_repr_res =
    if float then
      (make_native_repr_args arity Unboxed_float, Unboxed_float)
    else
      (native_repr_args, native_repr_res)
  in
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = not noalloc;
   prim_native_name = native_name;
   prim_native_repr_args = native_repr_args;
   prim_native_repr_res = native_repr_res}

let description_list_and_attributes p =
  let list = [p.prim_name] in
  let list = if not p.prim_alloc then "noalloc" :: list else list in
  let list =
    if p.prim_native_name <> "" then p.prim_native_name :: list else list
  in
  let has_float =
    let is_unboxed_float x = x = Unboxed_float in
    List.for_all is_unboxed_float p.prim_native_repr_args &&
    is_unboxed_float p.prim_native_repr_res
  in
  let list =
    if has_float then
      "float" :: list
    else
      list
  in
  let list = List.rev list in
  let attr_of_native_repr = function
    | Same_as_ocaml_repr -> None
    | Unboxed_float -> if has_float then None else Some "unboxed"
    | Unboxed_integer _ -> Some "unboxed"
    | Untagged_int -> Some "untagged"
  in
  let attrs =
    List.map attr_of_native_repr p.prim_native_repr_args @
    [attr_of_native_repr p.prim_native_repr_res]
  in
  (list, attrs)

let native_name p =
  if p.prim_native_name <> ""
  then p.prim_native_name
  else p.prim_name

let byte_name p =
  p.prim_name

let report_error ppf err =
  let open Format in
  match err with
  | Float_with_native_repr_attribute ->
      fprintf ppf "Cannot use \"float\" in conjunction with [@unboxed ]/[@untagged ]"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
