(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell for pchambart: I don't understand the
   directly_used_variables function well enough to adapt it to the new
   types.  This function needs a comment, and it may need an exhaustive
   match on primitives.
*)
(*

let rename_var var =
  Variable.rename var
    ~current_compilation_unit:(Compilation_unit.get_current_exn ())

let directly_used_variables tree =
  let set = ref Variable.Set.empty in
  let rec loop (flam : Flambda.t) =
    match flam with
    | Var (v, _) -> set := Variable.Set.add v !set
    | Prim(Pfield _, [Var _], _, _)
    | Prim(Poffsetref _, [Var _], _, _) -> ()
    | Prim(Psetfield(_, _), [Var _; e], _, _) -> loop e
    | Set_of_closures _ | Let _ | Assign _
    | Symbol _ | Const _ | Apply _ | Project_closure _
    | Project_var _ | Let_rec _
    | Prim _ | Switch _ | String_switch _ | Static_raise _
    | Static_catch _ | Try_with _ | If_then_else _ | Fsequence _
    | While _ | For _ | Send _ | Proved_unreachable as exp ->
      Flambda_iterators.apply_on_subexpressions loop exp
  in
  loop tree;
  !set

let variables_containing_ref lam =
  let map = ref Variable.Map.empty in
  let aux (flam : Flambda.t) =
    match flam with
    | Let(Immutable, v,
           Prim(Pmakeblock(0, Asttypes.Mutable), l, _, _), _, _) ->
        map := Variable.Map.add v (List.length l) !map
    | _ -> ()
  in
  Flambda_iterators.iter aux lam;
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

  let aux (flam : Flambda.t) : Flambda.t =
    match flam with
    | Let(Immutable, v,
           Prim(Pmakeblock(0, Asttypes.Mutable), inits, _, _), body, _)
      when convertible_variable v ->
        let _, expr =
          List.fold_left (fun (field,body) init ->
              match get_variable v field with
              | None -> assert false
              | Some (var, _) ->
                field+1,
                  ((Let(Mutable, var, init, body))
                    : Flambda.t))
            (0,body) inits in
        expr
    | Prim(Pfield field, [Var (v,d)], _, _)
      when convertible_variable v ->
        (match get_variable v field with
        | None -> Proved_unreachable d
        | Some (var,_) -> Var (var,d))
    | Prim(Poffsetref delta, [Var (v,d1)], dbg, d2)
      when convertible_variable v ->
        (match get_variable v 0 with
        | None -> Proved_unreachable d1
        | Some (var,size) ->
            if size = 1
            then
              Assign(var, Prim(Poffsetint delta, [Flambda.Var (var,d1)], dbg, d2),
                      Expr_id.create ())
            else Proved_unreachable d1)
    | Prim(Psetfield(field, _), [Var (v,d1); e], _, d2)
      when convertible_variable v ->
        (match get_variable v field with
         | None -> Proved_unreachable d1
         | Some (var,_) -> Assign(var, e, d2))
    | Set_of_closures _ | Let _
    | Assign _ | Var _
    | Symbol _ | Const _ | Apply _ | Project_closure _
    | Project_var _ | Let_rec _
    | Prim _ | Switch _ | String_switch _
    | Static_raise _ | Static_catch _
    | Try_with _ | If_then_else _ | Fsequence _
    | While _ | For _ | Send _ | Proved_unreachable as exp ->
        exp
  in
  Flambda_iterators.map aux lam

*)

let eliminate_ref flam = flam
