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

open Abstract_identifiers
open Flambda

let lift_lets tree =
  let rec aux = function
    | Flet(str1,v1,Flet(str2,v2,def2,body2,d2),body1,d1) ->
        Flet(str2,v2,def2,
             aux (Flet(str1,v1,body2,body1,d1)),d2)
    | e -> e in
  Flambdaiter.map aux tree

(** An variable in a closure can either be used by the closure itself
    or by an inlined version of the function. *)
let remove_unused_closure_variables tree =
  let used_variable_within_closure =
    let used = ref Var_within_closure.Set.empty in
    let aux expr = match expr with
      | Fvariable_in_closure({ vc_var },_) ->
         used := Var_within_closure.Set.add vc_var !used
      | e -> ()
    in
    Flambdaiter.iter aux tree;
    !used
  in
  let aux = function
    | Fset_of_closures ({ cl_fun; cl_free_var } as closure, eid) ->
       let all_free_var =
         Variable.Map.fold
           (fun _ { free_variables } acc ->
             Variable.Set.union free_variables acc)
           cl_fun.funs
           Variable.Set.empty in
       let cl_free_var =
         Variable.Map.filter (fun id _ ->
           Variable.Set.mem id all_free_var
             || Var_within_closure.Set.mem (Var_within_closure.wrap id)
                                       used_variable_within_closure)
           cl_free_var in
       Fset_of_closures ({ closure with cl_free_var }, eid)
    | e -> e in
  Flambdaiter.map aux tree
