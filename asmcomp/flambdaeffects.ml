(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Lambda
open Abstract_identifiers
open Flambda

let no_effects_prim = function
  | Pccall { prim_name =
               ( "caml_format_float" | "caml_format_int" |
                 "caml_int32_format" | "caml_nativeint_format" |
                 "caml_int64_format" ) } ->
      true
  | Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _
  | Pccall _ | Praise _ | Poffsetref _ | Pstringsetu | Pstringsets
  | Parraysetu _ | Parraysets _ | Pbigarrayset _

  | Psetglobalfield _

  | Pstringrefs | Parrayrefs _ | Pbigarrayref (false,_,_,_)

  | Pstring_load_16 false | Pstring_load_32 false | Pstring_load_64 false

  | Pbigstring_load_16 false | Pbigstring_load_32 false
  | Pbigstring_load_64 false

  | Pstring_set_16 _ | Pstring_set_32 _ | Pstring_set_64 _
  | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_64 _
    -> false
  | _ -> true

let rec no_effects = function
  | Fvar _
  | Fsymbol _
  | Fconst _
    -> true
  | Flet (_,_,def,body,_) ->
      no_effects def && no_effects body
  | Fletrec (defs,body,_) ->
      no_effects body &&
      List.for_all (fun (_,def) -> no_effects def) defs
  | Fprim(p, args, _, _) ->
      no_effects_prim p &&
      List.for_all no_effects args
  | Fset_of_closures ({ cl_free_var }, _) ->
      Variable.Map.for_all (fun id def -> no_effects def) cl_free_var
  | Fclosure({ fu_closure = lam }, _) ->
      no_effects lam
  | Fvariable_in_closure({ vc_closure }, _) ->
      no_effects vc_closure

  | Fifthenelse (cond, ifso, ifnot, _) ->
      no_effects cond &&
      no_effects ifso &&
      no_effects ifnot

  | Fswitch(lam,sw,_) ->
      let aux (_,lam) = no_effects lam in
      no_effects lam &&
      List.for_all aux sw.fs_blocks &&
      List.for_all aux sw.fs_consts &&
      Misc.may_default no_effects sw.fs_failaction true

  | Fstringswitch(lam,sw,def,_) ->
      no_effects lam &&
      List.for_all (fun (_,lam) -> no_effects lam) sw &&
      Misc.may_default no_effects def true

  | Fstaticcatch (_,_,body,_,_)
  | Ftrywith (body, _, _, _) ->
      (* the raise is effectful, no need to test the handler *)
      no_effects body

  | Fsequence (l1,l2,_) ->
      no_effects l1 && no_effects l2

  | Fwhile _
  | Ffor _
  | Fapply _
  | Fsend _
  | Fassign _
  | Fstaticraise _
    -> false

  | Funreachable _ -> true

let sequence l1 l2 annot =
  if no_effects l1 then l2 else Fsequence (l1, l2, annot)
