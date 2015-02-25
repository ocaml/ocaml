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

open Misc
open Abstract_identifiers

(* access functions *)

let find_declaration cf ({ funs } : _ Flambda.function_declarations) =
  Variable.Map.find (Closure_id.unwrap cf) funs

let find_declaration_variable cf ({ funs } : _ Flambda.function_declarations) =
  let var = Closure_id.unwrap cf in
  if not (Variable.Map.mem var funs)
  then raise Not_found
  else var

let find_free_variable cv ({ cl_free_var } : _ Flambda.fset_of_closures) =
  Variable.Map.find (Var_within_closure.unwrap cv) cl_free_var

(* utility functions *)

let function_arity (f : _ Flambda.function_declaration) = List.length f.params

let variables_bound_by_the_closure cf
      (decls : _ Flambda.function_declarations) =
  let func = find_declaration cf decls in
  let params = Variable.Set.of_list func.params in
  let functions = Variable.Map.keys decls.funs in
  Variable.Set.diff
    (Variable.Set.diff func.free_variables params)
    functions

let data_at_toplevel_node (expr : _ Flambda.t) =
  match expr with
  | Fsymbol (_,data)
  | Fvar (_,data)
  | Fconst (_,data)
  | Flet(_,_,_,_,data)
  | Fletrec(_,_,data)
  | Fset_of_closures(_,data)
  | Fclosure(_,data)
  | Fvariable_in_closure(_,data)
  | Fapply(_,data)
  | Fswitch(_,_,data)
  | Fstringswitch(_,_,_,data)
  | Fsend(_,_,_,_,_,data)
  | Fprim(_,_,_,data)
  | Fstaticraise (_,_,data)
  | Fstaticcatch (_,_,_,_,data)
  | Ftrywith(_,_,_,data)
  | Fifthenelse(_,_,_,data)
  | Fsequence(_,_,data)
  | Fwhile(_,_,data)
  | Ffor(_,_,_,_,_,data)
  | Fassign(_,_,data)
  | Fevent(_,_,data)
  | Funreachable data -> data

let description_of_toplevel_node (expr : _ Flambda.t) =
  match expr with
  | Fsymbol (sym,_) ->
      Format.asprintf "%%%a" Symbol.print sym
  | Fvar (id,data) ->
      Format.asprintf "var %a" Variable.print id
  | Fconst (cst,data) -> "const"
  | Flet(str, id, lam, body,data) ->
      Format.asprintf "let %a" Variable.print id
  | Fletrec(defs, body,data) -> "letrec"
  | Fset_of_closures(_,data) -> "set_of_closures"
  | Fclosure(_,data) -> "closure"
  | Fvariable_in_closure(_,data) -> "variable_in_closure"
  | Fapply(_,data) -> "apply"
  | Fswitch(arg, sw,data) -> "switch"
  | Fstringswitch(arg, cases, default, data) -> "stringswitch"
  | Fsend(kind, met, obj, args, _,data) -> "send"
  | Fprim(_, args, _,data) -> "prim"
  | Fstaticraise (i, args,data) -> "staticraise"
  | Fstaticcatch (i, vars, body, handler,data) -> "catch"
  | Ftrywith(body, id, handler,data) -> "trywith"
  | Fifthenelse(arg, ifso, ifnot,data) -> "if"
  | Fsequence(lam1, lam2,data) -> "seq"
  | Fwhile(cond, body,data) -> "while"
  | Ffor(id, lo, hi, dir, body,data) -> "for"
  | Fassign(id, lam,data) -> "assign"
  | Fevent(lam, ev, data) -> "event"
  | Funreachable _ -> "unreachable"

let recursive_functions ({ funs } : _ Flambda.function_declarations) =
  let function_variables = Variable.Map.keys funs in
  let directed_graph =
    Variable.Map.map (fun (ffun : _ Flambda.function_declaration) ->
        Variable.Set.inter ffun.free_variables function_variables)
      funs in
  let connected_components =
    Variable_connected_components.connected_components_sorted_from_roots_to_leaf
      directed_graph in
  Array.fold_left (fun rec_fun -> function
      | Variable_connected_components.No_loop _ ->
          rec_fun
      | Variable_connected_components.Has_loop elts ->
          List.fold_right Variable.Set.add elts rec_fun)
    Variable.Set.empty connected_components

let rec same (l1 : 'a Flambda.t) (l2 : 'a Flambda.t) =
  l1 == l2 || (* it is ok for string case: if they are physicaly the same,
                 it is the same original branch *)
  match (l1, l2) with
  | Fsymbol(s1, _), Fsymbol(s2, _) -> Symbol.equal s1 s2
  | Fsymbol _, _ | _, Fsymbol _ -> false
  | Fvar(v1, _), Fvar(v2, _) -> Variable.equal v1 v2
  | Fvar _, _ | _, Fvar _ -> false
  | Fconst(c1, _), Fconst(c2, _) -> begin
      let open Asttypes in
      match c1, c2 with
      | Fconst_base (Const_string (s1,_)), Fconst_base (Const_string (s2,_)) ->
          s1 == s2 (* string constants can't be merged: they are mutable,
                      but if they are physicaly the same, it comes from a safe case *)
      | Fconst_base (Const_string _), _ -> false
      | Fconst_base (Const_int _ | Const_char _ | Const_float _ |
                     Const_int32 _ | Const_int64 _ | Const_nativeint _), _
      | Fconst_pointer _, _
      | Fconst_float _, _
      | Fconst_float_array _, _
      | Fconst_immstring _, _ -> c1 = c2
    end
  | Fconst _, _ | _, Fconst _ -> false
  | Fapply(a1, _), Fapply(a2, _) ->
      a1.ap_kind = a2.ap_kind &&
      same a1.ap_function a2.ap_function &&
      samelist same a1.ap_arg a2.ap_arg
  | Fapply _, _ | _, Fapply _ -> false
  | Fset_of_closures (c1, _), Fset_of_closures (c2, _) ->
      Variable.Map.equal sameclosure c1.cl_fun.funs c2.cl_fun.funs &&
      Variable.Map.equal same c1.cl_free_var c2.cl_free_var &&
      Variable.Map.equal Variable.equal c1.cl_specialised_arg c2.cl_specialised_arg
  | Fset_of_closures _, _ | _, Fset_of_closures _ -> false
  | Fclosure (f1, _), Fclosure (f2, _) ->
      same f1.fu_closure f2.fu_closure &&
      Closure_id.equal f1.fu_fun f1.fu_fun &&
      sameoption Closure_id.equal f1.fu_relative_to f1.fu_relative_to
  | Fclosure _, _ | _, Fclosure _ -> false
  | Fvariable_in_closure (v1, _), Fvariable_in_closure (v2, _) ->
      same v1.vc_closure v2.vc_closure &&
      Closure_id.equal v1.vc_fun v2.vc_fun &&
      Var_within_closure.equal v1.vc_var v2.vc_var
  | Fvariable_in_closure _, _ | _, Fvariable_in_closure _ -> false
  | Flet (k1, v1, a1, b1, _), Flet (k2, v2, a2, b2, _) ->
      k1 = k2 && Variable.equal v1 v2 && same a1 a2 && same b1 b2
  | Flet _, _ | _, Flet _ -> false
  | Fletrec (bl1, a1, _), Fletrec (bl2, a2, _) ->
      samelist samebinding bl1 bl2 && same a1 a2
  | Fletrec _, _ | _, Fletrec _ -> false
  | Fprim (p1, al1, _, _), Fprim (p2, al2, _, _) ->
      p1 = p2 && samelist same al1 al2
  | Fprim _, _ | _, Fprim _ -> false
  | Fswitch (a1, s1, _), Fswitch (a2, s2, _) ->
      same a1 a2 && sameswitch s1 s2
  | Fswitch _, _ | _, Fswitch _ -> false
  | Fstringswitch (a1, s1, d1, _), Fstringswitch (a2, s2, d2, _) ->
      same a1 a2 &&
      samelist (fun (s1, e1) (s2, e2) -> s1 = s2 && same e1 e2) s1 s2 &&
      sameoption same d1 d2
  | Fstringswitch _, _ | _, Fstringswitch _ -> false
  | Fstaticraise (e1, a1, _), Fstaticraise (e2, a2, _) ->
      Static_exception.equal e1 e2 && samelist same a1 a2
  | Fstaticraise _, _ | _, Fstaticraise _ -> false
  | Fstaticcatch (s1, v1, a1, b1, _), Fstaticcatch (s2, v2, a2, b2, _) ->
      Static_exception.equal s1 s2 && samelist Variable.equal v1 v2 &&
      same a1 a2 && same b1 b2
  | Fstaticcatch _, _ | _, Fstaticcatch _ -> false
  | Ftrywith (a1, v1, b1, _), Ftrywith (a2, v2, b2, _) ->
      same a1 a2 && Variable.equal v1 v2 && same b1 b2
  | Ftrywith _, _ | _, Ftrywith _ -> false
  | Fifthenelse (a1, b1, c1, _), Fifthenelse (a2, b2, c2, _) ->
      same a1 a2 && same b1 b2 && same c1 c2
  | Fifthenelse _, _ | _, Fifthenelse _ -> false
  | Fsequence (a1, b1, _), Fsequence (a2, b2, _) ->
      same a1 a2 && same b1 b2
  | Fsequence _, _ | _, Fsequence _ -> false
  | Fwhile (a1, b1, _), Fwhile (a2, b2, _) ->
      same a1 a2 && same b1 b2
  | Fwhile _, _ | _, Fwhile _ -> false
  | Ffor(v1, a1, b1, df1, c1, _), Ffor(v2, a2, b2, df2, c2, _) ->
      Variable.equal v1 v2 &&  same a1 a2 &&
      same b1 b2 && df1 = df2 && same c1 c2
  | Ffor _, _ | _, Ffor _ -> false
  | Fassign(v1, a1, _), Fassign(v2, a2, _) ->
      Variable.equal v1 v2 && same a1 a2
  | Fassign _, _ | _, Fassign _ -> false
  | Fsend(k1, a1, b1, cl1, _, _), Fsend(k2, a2, b2, cl2, _, _) ->
      k1 = k2 && same a1 a2 && same b1 b2 && samelist same cl1 cl2
  | Fsend _, _ | _, Fsend _ -> false
  | Funreachable _, Funreachable _ -> true
  | Funreachable _, _ | _, Funreachable _ -> false
  | Fevent _, Fevent _ -> false

and sameclosure c1 c2 =
  samelist Variable.equal c1.params c2.params &&
  same c1.body c2.body

and samebinding (v1, c1) (v2, c2) =
  Variable.equal v1 v2 && same c1 c2

and sameswitch fs1 fs2 =
  let samecase (n1, a1) (n2, a2) = n1 = n2 && same a1 a2 in
  fs1.fs_numconsts = fs2.fs_numconsts &&
  fs1.fs_numblocks = fs2.fs_numblocks &&
  samelist samecase fs1.fs_consts fs2.fs_consts &&
  samelist samecase fs1.fs_blocks fs2.fs_blocks &&
  sameoption same fs1.fs_failaction fs2.fs_failaction

let can_be_merged = same

(* Sharing key TODO
   Not implemented yet: this avoids sharing anything *)

type sharing_key = unit
let make_key _ = None

let fold_over_exprs_for_variables_bound_by_closure ~fun_id ~clos_id ~clos
      ~init ~f =
  Variable.Set.fold (fun var acc ->
      let expr : _ Flambda.t =
        Fvariable_in_closure
          ({ vc_closure = Fvar (clos_id, Expr_id.create ());
             vc_fun = fun_id;
             vc_var = Var_within_closure.wrap var;
           },
           Expr_id.create ())
      in
      f ~acc ~var ~expr)
    (variables_bound_by_the_closure fun_id clos) init

let make_closure_declaration ~id ~body ~params : _ Flambda.t =
  let free_variables = Flambdaiter.free_variables body in
  let param_set = Variable.Set.of_list params in
  if not (Variable.Set.subset param_set free_variables) then begin
    Misc.fatal_error "Flambdautils.make_closure_declaration"
  end;
  let sb =
    Variable.Set.fold
      (fun id sb -> Variable.Map.add id (Flambdasubst.freshen_var id) sb)
      free_variables Variable.Map.empty in
  let body = Flambdasubst.toplevel_substitution sb body in
  let subst id = Variable.Map.find id sb in
  let function_declaration : _ Flambda.function_declaration =
    { stub = false;
      params = List.map subst params;
      free_variables = Variable.Set.map subst free_variables;
      body;
      dbg = Debuginfo.none;
    }
  in
  let fv' =
    Variable.Map.fold (fun id id' fv' ->
        Variable.Map.add id' (Flambda.Fvar(id,Expr_id.create ())) fv')
      (Variable.Map.filter (fun id _ -> not (Variable.Set.mem id param_set)) sb)
      Variable.Map.empty in
  let current_unit = Symbol.Compilation_unit.get_current_exn () in
  Fclosure
    ({ fu_closure =
         Fset_of_closures
           ({ cl_fun =
                { ident = Set_of_closures_id.create current_unit;
                  funs = Variable.Map.singleton id function_declaration;
                  compilation_unit = current_unit };
              cl_free_var = fv';
              cl_specialised_arg = Variable.Map.empty },
            Expr_id.create ());
       fu_fun = Closure_id.wrap id;
       fu_relative_to = None},
     Expr_id.create ())

module Pair_var_var =
  Ext_types.Identifiable.Make(Ext_types.Pair(Variable.M)(Variable.M))

type t =
  | Anything
  | Arguments of Pair_var_var.Set.t

let fixpoint state =
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
    let (func, var) = arg in
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
        let set =
          match set with
          | Arguments args ->
              if Pair_var_var.Set.exists (fun (func', var') ->
                  Variable.equal func func' && not (Variable.equal var var'))
                  args
              then Anything
              else set
          | Anything -> Anything in
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

let unchanging_params_in_recursion (decls : _ Flambda.function_declarations) =
  let escaping_functions = ref Variable.Set.empty in
  let state = ref Pair_var_var.Map.empty in
  let variables_at_position =
    Variable.Map.map (fun (decl : _ Flambda.function_declaration) ->
        Array.of_list decl.params)
      decls.funs
  in
  let link
      ~callee ~callee_arg
      ~caller ~caller_arg =
    let kind =
      try Pair_var_var.Map.find (callee,callee_arg) !state with
      | Not_found -> Arguments Pair_var_var.Set.empty in
    match kind with
    | Anything -> ()
    | Arguments set ->
        state :=
          Pair_var_var.Map.add (callee,callee_arg)
            (Arguments (Pair_var_var.Set.add (caller,caller_arg) set)) !state
  in
  let mark ~callee ~callee_arg =
    state := Pair_var_var.Map.add (callee,callee_arg) Anything !state
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
  let check_argument ~caller ~callee ~callee_pos arg =
    match find_callee_arg ~callee ~callee_pos with
    | None -> () (* not a recursive call *)
    | Some callee_arg ->
        match (arg : _ Flambda.t) with
        | Fvar(caller_arg,_) ->
            begin
              match Variable.Map.find caller decls.funs with
              | exception Not_found ->
                  mark ~callee ~callee_arg
              | { params } ->
                  if List.mem caller_arg params then
                    link ~caller ~caller_arg ~callee ~callee_arg
                  else
                    mark ~callee ~callee_arg
            end
        | _ -> mark ~callee ~callee_arg
  in
  let test_escape var =
    if Variable.Map.mem var decls.funs
    then escaping_functions := Variable.Set.add var !escaping_functions
  in
  let arity ~callee =
    match Variable.Map.find callee decls.funs with
    | exception Not_found -> 0
    | func -> function_arity func
  in
  let rec loop ~caller (expr : _ Flambda.t) =
    match expr with
    | Fvar (var,_) -> test_escape var
    | Fapply ({ ap_function = Fvar(callee,_); ap_arg }, _) ->
        let num_args = List.length ap_arg in
        for callee_pos = num_args to (arity callee) - 1 do
          match find_callee_arg ~callee ~callee_pos with
          | None -> ()
          | Some callee_arg -> mark ~callee ~callee_arg
          (* if a function is partially aplied, consider all missing
             arguments as not kept*)
        done;
        List.iteri (fun callee_pos arg ->
            check_argument ~caller ~callee ~callee_pos arg)
          ap_arg;
        List.iter (loop ~caller) ap_arg
    | e ->
        Flambdaiter.apply_on_subexpressions (loop ~caller) e
  in
  Variable.Map.iter (fun caller (decl : _ Flambda.function_declaration) ->
      loop caller decl.body)
    decls.funs;
  let state =
    Variable.Map.fold (fun func_var
          ({ params } : _ Flambda.function_declaration) state ->
        if Variable.Set.mem func_var !escaping_functions
        then
          List.fold_left (fun state param ->
              Pair_var_var.Map.add (func_var,param) Anything state)
            state params
        else state)
      decls.funs !state
  in
  let result = fixpoint state in
  let not_kept =
    Pair_var_var.Map.fold (fun (_,var) set acc ->
        match set with
        | Anything -> Variable.Set.add var acc
        | Arguments _ -> acc)
      result Variable.Set.empty
  in
  let variables = Variable.Map.fold (fun _
        ({ params } : _ Flambda.function_declaration) set ->
      Variable.Set.union (Variable.Set.of_list params) set)
    decls.funs Variable.Set.empty
  in
  Variable.Set.diff variables not_kept
