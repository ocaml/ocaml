(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

open Misc
open Asttypes
open Primitive
open Path
open Types
open Typedtree
open Lambda

let has_base_type exp base_ty =
  let exp_ty =
    Ctype.expand_head exp.exp_env (Ctype.correct_levels exp.exp_type) in
  match (Ctype.repr exp_ty, Ctype.repr base_ty) with
    {desc = Tconstr(p1, _, _)}, {desc = Tconstr(p2, _, _)} -> Path.same p1 p2
  | (_, _) -> false

let maybe_pointer exp =
  let exp_ty =
    Ctype.expand_head exp.exp_env (Ctype.correct_levels exp.exp_type) in
  match (Ctype.repr exp_ty).desc with
    Tconstr(p, args, abbrev) ->
      not (Path.same p Predef.path_int) &&
      not (Path.same p Predef.path_char) &&
      begin try
        match Env.find_type p exp.exp_env with
          {type_kind = Type_variant cstrs} ->
            List.exists (fun (name, args) -> args <> []) cstrs
        | _ -> true
      with Not_found -> true
        (* This can happen due to e.g. missing -I options,
           causing some .cmi files to be unavailable.
           Maybe we should emit a warning. *)
      end
  | _ -> true

let array_element_kind env ty =
  let ty = Ctype.repr (Ctype.expand_head env ty) in
  match ty.desc with
    Tvar ->
      Pgenarray
  | Tconstr(p, args, abbrev) ->
      if Path.same p Predef.path_int || Path.same p Predef.path_char then
        Pintarray
      else if Path.same p Predef.path_float then
        Pfloatarray
      else if Path.same p Predef.path_string
           || Path.same p Predef.path_array then
        Paddrarray
      else begin
        try
          match Env.find_type p env with
            {type_kind = Type_abstract} ->
              Pgenarray
          | {type_kind = Type_variant cstrs}
            when List.for_all (fun (name, args) -> args = []) cstrs ->
              Pintarray
          | {type_kind = _} ->
              Paddrarray
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Pgenarray
      end
  | _ ->
      Paddrarray

let array_kind_gen ty env =
  let array_ty = Ctype.expand_head env (Ctype.correct_levels ty) in
  match (Ctype.repr array_ty).desc with
    Tconstr(p, [elt_ty], _) when Path.same p Predef.path_array ->
      array_element_kind env elt_ty
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_kind_gen exp.exp_type exp.exp_env

let array_pattern_kind pat = array_kind_gen pat.pat_type pat.pat_env
