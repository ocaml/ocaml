(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* CR pchambart to pchambart: in fact partial application doesn't work because
   there are no 'known' partial application left: they are converted to
   applications new partial function declaration.
   That can be improved (and many other cases) by keeping track of aliases in
   closure of functions. *)

(* A parameter [x] of the function [f] is considered as unchanging if during
   an 'external' (call from outside the set of closures) call of [f], every
   recursive call of [f] all the instances of [x] are aliased to the original
   one.

   This function computes an underapproximation of that set by computing the
   flow of parameters between the different function of the set of closures.
   We will write (f, x) <- (g, y) to denote that the parameter [x] of the
   function [f] can be an alias of the parameter [y] of the function [g].
   (f, x) <- Anything denote that unknown values can flow to [x].
   The '<-' relation is transitive.

   [x] is not unchanging if either
      (f, x) <- Anything
   or (f, x) <- (f, y) with x != y

   Notice that having (f, x) <- (g, a) and (f, x) <- (g, b) does not make
   x not unchanging. This is because (g, a) and (g, b) represent necessarily
   different values only if g is the externaly called function. If some
   value where created during the execution of the function that could
   flow to (g, a), then (g, a) <- Anything, so (f, x) <- Anything.

 *)

(* This is computed in two steps:
   * accumulate the atomic <- relations
   * compute the transitive closure

   We record [(f, x) <- Argument (g, y)] when the function g calls f and
   the y parameter of g is used as argument for the x parameter of f. For
   instance in

     let rec f x = ...
     and g y = f x

   We record [(f, x) <- Anything] when some unknown values can flow to a the
   [y] parameter.

     let rec f x = f 1

   We record also [(f, x) <- Anything] if [f] could escape. This is over
   approximated by considering that a function escape when its variable is used
   for something else than an application:

     let rec f x = (f,f)


  The <- relation is represented by the type

     t Pair_var_var.Map.t

  if [Pair_var_var.Set.mem (g, y) s] and
  [Argument s = Pair_var_var.Map.find (f, x) relation]
  then (f, x) <- (g, y) is in the relation.

*)

(*
module Pair_var_var =
  Ext_types.Identifiable.Make(Ext_types.Pair(Variable.M)(Variable.M))

type t =
  | Anything
  | Arguments of Pair_var_var.Set.t

let transitive_closure state =
  let union s1 s2 =
    match s1, s2 with
    | Anything, _ | _, Anything -> Anything
    | Arguments s1, Arguments s2 -> Arguments (Pair_var_var.Set.union s1 s2)
  in
  let equal s1 s2 =
    match s1, s2 with
    | Anything, Arguments _ | Arguments _, Anything -> false
    | Anything, Anything -> true
    | Arguments s1, Arguments s2 -> Pair_var_var.Set.equal s1 s2
  in
  let update arg state =
    let original_set =
      try Pair_var_var.Map.find arg state with
      | Not_found -> Arguments Pair_var_var.Set.empty
    in
    match original_set with
    | Anything -> state
    | Arguments arguments ->
        let set =
          Pair_var_var.Set.fold
            (fun orig acc->
               let set =
                 try Pair_var_var.Map.find orig state with
                 | Not_found -> Arguments Pair_var_var.Set.empty in
               union set acc)
            arguments original_set
        in
        Pair_var_var.Map.add arg set state
  in
  let once state =
    Pair_var_var.Map.fold (fun arg _ state -> update arg state) state state
  in
  let rec fp state =
    let state' = once state in
    if Pair_var_var.Map.equal equal state state'
    then state
    else fp state'
  in
  fp state

let unchanging_params_in_recursion (decls : Flambda.function_declarations) =
  let escaping_functions = ref Variable.Set.empty in
  let relation = ref Pair_var_var.Map.empty in
  let variables_at_position =
    Variable.Map.map (fun (decl : Flambda.function_declaration) ->
        Array.of_list decl.params)
      decls.funs
  in
  let link
      ~callee ~callee_arg
      ~caller ~caller_arg =
    let kind =
      try Pair_var_var.Map.find (callee,callee_arg) !relation with
      | Not_found -> Arguments Pair_var_var.Set.empty in
    match kind with
    | Anything -> ()
    | Arguments set ->
        relation :=
          Pair_var_var.Map.add (callee,callee_arg)
            (Arguments (Pair_var_var.Set.add (caller,caller_arg) set)) !relation
  in
  let mark ~callee ~callee_arg =
    relation := Pair_var_var.Map.add (callee,callee_arg) Anything !relation
  in
  let find_callee_arg ~callee ~callee_pos =
    match Variable.Map.find callee variables_at_position with
    | exception Not_found -> None (* not a recursive call *)
    | arr ->
        if callee_pos < Array.length arr then
          (* ignore overapplied parameters: they are applied to another function *)
          Some arr.(callee_pos)
        else None
  in
  (* If the called closure is in the current set of closures, record the
     relation (callee, callee_arg) <- (caller, caller_arg) *)
  let check_argument ~caller ~callee ~callee_pos caller_arg =
    match find_callee_arg ~callee ~callee_pos with
    | None -> () (* not a recursive call *)
    | Some callee_arg ->
      match Variable.Map.find caller decls.funs with
      | exception Not_found ->
        assert false
      | { params } ->
        if List.mem caller_arg params then
          link ~caller ~caller_arg ~callee ~callee_arg
        else
          mark ~callee ~callee_arg
  in
  let test_escape var =
    if Variable.Map.mem var decls.funs
    then escaping_functions := Variable.Set.add var !escaping_functions
  in
  let arity ~callee =
    match Variable.Map.find callee decls.funs with
    | exception Not_found -> 0
    | func -> Flambda_utils.function_arity func
  in
  let rec loop ~caller (expr : Flambda.t) =
    match expr with
    | Var var -> test_escape var
    | Apply { func = callee; args } ->
      let num_args = List.length args in
      for callee_pos = num_args to (arity ~callee) - 1 do
        match find_callee_arg ~callee ~callee_pos with
        | None -> ()
        | Some callee_arg -> mark ~callee ~callee_arg
        (* if a function is partially aplied, consider all missing
           arguments as not kept*)
      done;
      List.iteri (fun callee_pos arg ->
          check_argument ~caller ~callee ~callee_pos arg)
        args
    | e ->
      Flambda_iterators.apply_on_subexpressions (loop ~caller)
        (fun (_ : Flambda.named) -> ()) e
  in
  Variable.Map.iter (fun caller (decl : Flambda.function_declaration) ->
      loop ~caller decl.body)
    decls.funs;
  let relation =
    Variable.Map.fold (fun func_var
          ({ params } : Flambda.function_declaration) relation ->
        if Variable.Set.mem func_var !escaping_functions
        then
          List.fold_left (fun relation param ->
              Pair_var_var.Map.add (func_var,param) Anything relation)
            relation params
        else relation)
      decls.funs !relation
  in
  let result = transitive_closure relation in
  let not_unchanging =
    Pair_var_var.Map.fold (fun (func,var) set not_unchanging ->
        match set with
        | Anything -> Variable.Set.add var not_unchanging
        | Arguments set ->
            if Pair_var_var.Set.exists (fun (func', var') ->
                Variable.equal func func' && not (Variable.equal var var'))
                set
            then Variable.Set.add var not_unchanging
            else not_unchanging)
      result Variable.Set.empty
  in
  let variables = Variable.Map.fold (fun _
        ({ params } : Flambda.function_declaration) set ->
      Variable.Set.union (Variable.Set.of_list params) set)
    decls.funs Variable.Set.empty
  in
  Variable.Set.diff variables not_unchanging

type argument =
  | Used
  | Argument of Variable.t

let unused_arguments (decls : Flambda.function_declarations) : Variable.Set.t =
  let used_variables = ref Variable.Set.empty in
  let used_variable var =
    used_variables := Variable.Set.add var !used_variables
  in
  let variables_at_position =
    Variable.Map.fold (fun var (decl : Flambda.function_declaration) map ->
        let cid = Closure_id.wrap var in
        Closure_id.Map.add cid (Array.of_list decl.params) map)
      decls.funs Closure_id.Map.empty
  in
  let find_callee_arg ~callee ~callee_pos =
    match Closure_id.Map.find callee variables_at_position with
    | exception Not_found -> Used (* not a recursive call *)
    | arr ->
        assert(callee_pos < Array.length arr);
        (* Direct calls don't have overapplication *)
        Argument arr.(callee_pos)
  in
  let rec loop (expr : Flambda.t) =
    match expr with
    | Var var -> used_variable var
    | Apply { func = _; args; kind = Direct callee } ->
      List.iteri (fun callee_pos arg ->
          match find_callee_arg ~callee ~callee_pos with
          | Used -> used_variable arg
          | Argument param ->
            if not (Variable.equal arg param) then used_variable arg)
        args
    | e ->
      Flambda_iterators.apply_on_subexpressions loop
        (fun (_ : Flambda.named) -> ()) e
  in
  Variable.Map.iter (fun _caller (decl : Flambda.function_declaration) ->
      loop decl.body)
    decls.funs;
  let arguments = Variable.Map.fold (fun _ decl acc ->
      Variable.Set.union acc (Variable.Set.of_list decl.Flambda.params))
      decls.funs Variable.Set.empty
  in
  Variable.Set.diff arguments !used_variables
*)

(* CR mshinwell: re-enable this once everything else is working *)
let unchanging_params_in_recursion _ = Variable.Set.empty
let unused_arguments _ = Variable.Set.empty
