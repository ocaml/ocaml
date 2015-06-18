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

(* Transform let-bound references into variables *)

open Lambda
open Abstract_identifiers
open Flambda

let rename_var var =
  Variable.rename var
    ~current_compilation_unit:(Symbol.Compilation_unit.get_current_exn ())

let directly_used_variables tree =
  let set = ref Variable.Set.empty in
  let rec loop = function
    | Fvar (v, _) ->
        set := Variable.Set.add v !set
    | Fprim(Pfield _, [Fvar _], _, _)
    | Fprim(Poffsetref _, [Fvar _], _, _) ->
        ()
    | Fprim(Psetfield(_, _), [Fvar _; e], _, _) ->
        loop e
    | Fset_of_closures _ | Flet _ | Fassign _
    | Fsymbol _ | Fconst _ | Fapply _ | Fselect_closure _
    | Fvar_within_closure _ | Fletrec _
    | Fprim _ | Fswitch _ | Fstringswitch _ | Fstaticraise _
    | Fstaticcatch _ | Ftrywith _ | Fifthenelse _ | Fsequence _
    | Fwhile _ | Ffor _ | Fsend _ | Funreachable _ as exp ->
        Flambdaiter.apply_on_subexpressions loop exp in
  loop tree;
  !set

let variables_containing_ref lam =
  let map = ref Variable.Map.empty in
  let aux = function
    | Flet(Immutable, v,
           Fprim(Pmakeblock(0, Asttypes.Mutable), l, _, _), _, _) ->
        map := Variable.Map.add v (List.length l) !map
    | _ -> ()
  in
  Flambdaiter.iter aux lam;
  !map

let eliminate_ref lam =
  let directly_used_variables = directly_used_variables lam in
  let convertible_variables =
    Variable.Map.filter
      (fun v _ -> not (Variable.Set.mem v directly_used_variables))
      (variables_containing_ref lam)
  in
  let convertible_variables =
    Variable.Map.mapi (fun v size -> Array.init size (fun _ -> rename_var v))
      convertible_variables in
  let convertible_variable v = Variable.Map.mem v convertible_variables in

  let get_variable v field =
    let arr = try Variable.Map.find v convertible_variables
      with Not_found -> assert false in
    if Array.length arr <= field
    then None (* This case could apply when inlining code containing GADTS *)
    else Some (arr.(field), Array.length arr)
  in

  let aux = function
    | Flet(Immutable, v,
           Fprim(Pmakeblock(0, Asttypes.Mutable), inits, _, _), body, _)
      when convertible_variable v ->
        let _, expr =
          List.fold_left (fun (field,body) init ->
              match get_variable v field with
              | None -> assert false
              | Some (var, _) ->
                  field+1,
                  Flet(Mutable, var, init, body, Expr_id.create ()))
            (0,body) inits in
        expr
    | Fprim(Pfield field, [Fvar (v,d)], _, _)
      when convertible_variable v ->
        (match get_variable v field with
        | None -> Funreachable d
        | Some (var,_) -> Fvar (var,d))
    | Fprim(Poffsetref delta, [Fvar (v,d1)], dbg, d2)
      when convertible_variable v ->
        (match get_variable v 0 with
        | None -> Funreachable d1
        | Some (var,size) ->
            if size = 1
            then
              Fassign(var, Fprim(Poffsetint delta, [Fvar (var,d1)], dbg, d2),
                      Expr_id.create ())
            else Funreachable d1)
    | Fprim(Psetfield(field, _), [Fvar (v,d1); e], _, d2)
      when convertible_variable v ->
        (match get_variable v field with
         | None -> Funreachable d1
         | Some (var,_) -> Fassign(var, e, d2))
    | Fset_of_closures _ | Flet _
    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Fselect_closure _
    | Fvar_within_closure _ | Fletrec _
    | Fprim _ | Fswitch _ | Fstringswitch _
    | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fifthenelse _ | Fsequence _
    | Fwhile _ | Ffor _ | Fsend _ | Funreachable _ as exp ->
        exp
  in
  Flambdaiter.map aux lam
