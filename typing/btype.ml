(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Basic operations on core types *)

open Types

(**** Type level management ****)

let generic_level = 100000000

(* Used to mark a type during a traversal. *)
let lowest_level = 0
let pivot_level = 2 * lowest_level - 1
    (* pivot_level - lowest_level < lowest_level *)

(**** Some type creators ****)

let new_id = ref (-1)

let newty2 level desc  =
  incr new_id; { desc = desc; level = level; id = !new_id }
let newgenty desc      = newty2 generic_level desc
let newgenvar ()       = newgenty Tvar
let newmarkedvar level =
  incr new_id; { desc = Tvar; level = pivot_level - level; id = !new_id }
let newmarkedgenvar () =
  incr new_id;
  { desc = Tvar; level = pivot_level - generic_level; id = !new_id }

(**** Representative of a type ****)

let rec field_kind_repr =
  function
    Fvar {contents = Some kind} -> field_kind_repr kind
  | kind                        -> kind

let rec repr =
  function
    {desc = Tlink t'} ->
      (* 
         We do no path compression. Path compression does not seem to
         improve notably efficiency, and it prevents from changing a
         [Tlink] into another type (for instance, for undoing a
         unification).
      *)
      repr t'
  | {desc = Tfield (_, k, _, t')} when field_kind_repr k = Fabsent ->
      repr t'
  | t -> t


                  (**********************************)
                  (*  Utilities for type traversal  *)
                  (**********************************)


let iter_type_expr f ty =
  match ty.desc with
    Tvar               -> ()
  | Tarrow (ty1, ty2) -> f ty1; f ty2
  | Ttuple l           -> List.iter f l
  | Tconstr (_, l, _)          -> List.iter f l
  | Tobject(ty, {contents = Some (_, p)})
                         -> f ty; List.iter f p
  | Tobject (ty, _)    -> f ty
  | Tfield (_, _, ty1, ty2) -> f ty1; f ty2
  | Tnil               -> ()
  | Tlink ty           -> f ty

let saved_desc = ref []
  (* Saved association of generic nodes with their description. *)

let save_desc ty desc = 
  saved_desc := (ty, desc)::!saved_desc

(* Restored type descriptions. *)
let cleanup_types () =
  List.iter (fun (ty, desc) -> ty.desc <- desc) !saved_desc;
  saved_desc := []

(* Mark a type. *)
let rec mark_type ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr mark_type ty
  end

(* Remove marks from a type. *)
let rec unmark_type ty =
  let ty = repr ty in
  if ty.level < lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr unmark_type ty
  end

let unmark_type_decl decl =
  List.iter unmark_type decl.type_params;
  begin match decl.type_kind with
    Type_abstract -> ()
  | Type_variant cstrs ->
      List.iter (fun (c, tl) -> List.iter unmark_type tl) cstrs
  | Type_record lbls ->
      List.iter (fun (c, mut, t) -> unmark_type t) lbls
  end;
  begin match decl.type_manifest with
    None    -> ()
  | Some ty -> unmark_type ty
  end

let unmark_class_signature sign =
  unmark_type sign.cty_self;
  Vars.iter (fun l (m, t) -> unmark_type t) sign.cty_vars

let rec unmark_class_type =
  function
    Tcty_constr (p, tyl, cty) ->
      List.iter unmark_type tyl; unmark_class_type cty
  | Tcty_signature sign ->
      unmark_class_signature sign
  | Tcty_fun (ty, cty) ->
      unmark_type ty; unmark_class_type cty



                  (*******************************************)
                  (*  Memorization of abbreviation expansion *)
                  (*******************************************)

let memo = ref []
        (* Contains the list of saved abbreviation expansions. *)

let cleanup_abbrev () =
        (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := Mnil) !memo;
  memo := []

let memorize_abbrev mem path v v' =
        (* Memorize the expansion of an abbreviation. *)
  mem := Mcons (path, v, v', !mem);
  memo := mem :: !memo

let rec forget_abbrev_rec mem path =
  match mem with
    Mnil ->
      assert false
  | Mcons (path', _, _, rem) when Path.same path path' ->
      rem 
  | Mcons (path, v, v', rem) ->
      Mcons (path, v, v', forget_abbrev_rec rem path)
  | Mlink mem' ->
      mem' := forget_abbrev_rec !mem' path;
      raise Exit

let forget_abbrev mem path =
  try mem := forget_abbrev_rec !mem path with Exit -> ()
