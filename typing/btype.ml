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

let newgenty desc      = { desc = desc; level = generic_level }
let newgenvar ()       = newgenty Tvar
let newmarkedgenvar () = { desc = Tvar; level = pivot_level - generic_level }

(**** Representative of a type ****)

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
  | t -> t

let rec field_kind_repr =
  function
    Fvar {contents = Some kind} -> field_kind_repr kind
  | kind                        -> kind

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

(* Restored type descriptions *)
let cleanup_types () =
  List.iter (fun (ty, desc) -> ty.desc <- desc) !saved_desc;
  saved_desc := []

(* Remove marks from a type. *)
let rec unmark_type ty =
  let ty = repr ty in
  if ty.level < lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr unmark_type ty
  end


                  (*******************************************)
                  (*  Memorization of abbreviation expansion *)
                  (*******************************************)

let memo = ref []
        (* Contains the list of saved abbreviation expansions. *)

let cleanup_abbrev () =
        (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := Mnil) !memo;
  memo := []

let memorize_abbrev mem path v =
        (* Memorize the expansion of an abbreviation. *)
  mem := Mcons (path, v, !mem);
  memo := mem :: !memo
