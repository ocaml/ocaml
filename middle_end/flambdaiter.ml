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

let apply_on_subexpressions f f_named (flam : _ Flambda.t) =
  match flam with
  | Var _ -> ()
  | Apply ({func;_},_) ->
    f func
  | Project_var _ -> ()
  | Let (_, _, defining_expr, body) ->
    f_named defining_expr;
    f body
  | Let_rec (defs, body,_) ->
    List.iter (fun (_,l) -> f_named l) defs;
    f body
  | Fseq_prim (_, args, _, _) ->
    List.iter f args
  | Switch (arg,sw,_) ->
    f arg;
    List.iter (fun (_,l) -> f l) sw.consts;
    List.iter (fun (_,l) -> f l) sw.blocks;
    Misc.may f sw.failaction
  | String_switch (arg,sw,def,_) ->
    f arg;
    List.iter (fun (_,l) -> f l) sw;
    Misc.may f def
  | Static_raise (_,l,_) ->
    List.iter f l
  | Static_catch (_,_,f1,f2,_) ->
    f f1; f f2;
  | Try_with (f1,_,f2,_) ->
    f f1; f f2
  | If_then_else (f1,f2,f3,_) ->
    f f1;f f2;f f3
  | Fsequence (f1,f2,_)
  | While (f1,f2,_) ->
    f f1; f f2
  | For (_,f1,f2,_,f3,_) ->
    f f1;f f2;f f3
  | Assign (_,f1,_) ->
    f f1
  | Unreachable _ -> ()


  | Symbol _
  | Const _
  | Unreachable _
  | Project_closure _
  | Move_within_set_of_closures _
  | Prim _ -> ()






  | Set_of_closures ({function_decls;free_vars = _; specialised_args = _},_) ->
    Variable.Map.iter (fun _ (ffun : _ Flambda.function_declaration) ->
        f ffun.body)
      function_decls.funs
  | Send (_,f1,f2,fl,_,_) ->
    List.iter f (f1::f2::fl)

let iter_general ~toplevel f t =
  let rec aux (t : _ Flambda.t) =
    f t;
    match t with
    | Symbol _
    | Var _
    | Const _
    | Project_closure _
    | Move_within_set_of_closures _
    | Project_var _
    | Prim _ -> ()

    | Assign (_,f1,_) ->
      aux f1

    | Let ( _, _, f1, f2,_)
    | Try_with (f1,_,f2,_)
    | Fsequence (f1,f2,_)
    | While (f1,f2,_)
    | Static_catch (_,_,f1,f2,_) ->
      aux f1; aux f2;

    | For (_,f1,f2,_,f3,_)
    | If_then_else (f1,f2,f3,_) ->
      aux f1;aux f2;aux f3

    | Static_raise (_,l,_) ->
      iter_list l

    | Apply ({func = f1; _},_) ->
      aux f1
    | Fseq_prim (_, args, _, _) ->
      iter_list args

    | Set_of_closures ({function_decls = funcs; free_vars = _; specialised_args = _},_) ->
      if not toplevel then begin
        Variable.Map.iter (fun _ (ffun : _ Flambda.function_declaration) ->
            aux ffun.body)
          funcs.funs
      end

    | Let_rec (defs, body,_) ->
      List.iter (fun (_,l) -> aux l) defs;
      aux body
    | Switch (arg,sw,_) ->
      aux arg;
      List.iter (fun (_,l) -> aux l) sw.consts;
      List.iter (fun (_,l) -> aux l) sw.blocks;
      Misc.may aux sw.failaction
    | String_switch (arg,sw,def,_) ->
      aux arg;
      List.iter (fun (_,l) -> aux l) sw;
      Misc.may aux def

    | Send (_,f1,f2,fl,_,_) ->
      iter_list (f1::f2::fl)
    | Unreachable _ -> ()

  and iter_list l = List.iter aux l in
  aux t

let iter f t = iter_general ~toplevel:false f t
let iter_toplevel f t = iter_general ~toplevel:true f t

let iter_on_sets_of_closures f t =
  let aux (flam : _ Flambda.t) =
    match flam with
    | Set_of_closures (clos,data) -> f clos data
    | Assign _ | Var _
    | Symbol _ | Const _ | Apply _ | Project_closure _
    | Move_within_set_of_closures _
    | Project_var _ | Let _ | Let_rec _
    | Prim _ | Fseq_prim _ | Switch _ | Static_raise _ | Static_catch _
    | Try_with _ | If_then_else _ | Fsequence _ | String_switch _
    | While _ | For _ | Send _ | Unreachable _
      -> ()
  in
  iter aux t

let map_general ~toplevel f f_named tree =
  let rec aux (tree : _ Flambda.t) =
    let exp : _ Flambda.t =
      match tree with
      | Var _ -> tree
      | Apply ({ func; args; kind; dbg }, annot) ->
        Apply ({ func = aux func; args; kind; dbg }, annot)
      | Project_var _ -> tree
      | Let (str, id, lam, body, annot) ->
        let lam = aux_named lam in
        let body = aux body in
        Let (str, id, lam, body, annot)
      | Let_rec (defs, body, annot) ->
        let defs = List.map (fun (id, lam) -> id, aux_named lam) defs in
        let body = aux body in
        Let_rec (defs, body, annot)
      | Fseq_prim (prim, args, dbg, annot) ->
        Fseq_prim (prim, List.map aux args, dbg, annot)
      | Switch(arg, sw, annot) ->
        let arg = aux arg in
        let sw =
          { sw with
            failaction = Misc.may_map aux sw.failaction;
            consts = List.map (fun (i,v) -> i, aux v) sw.consts;
            blocks = List.map (fun (i,v) -> i, aux v) sw.blocks;
          }
        in
        Switch (arg, sw, annot)
      | String_switch(arg, sw, def, annot) ->
        let arg = aux arg in
        let sw = List.map (fun (i,v) -> i, aux v) sw in
        let def = Misc.may_map aux def in
        String_switch(arg, sw, def, annot)
      | Static_raise(i, args, annot) ->
        let args = List.map aux args in
        Static_raise (i, args, annot)
      | Static_catch (i, vars, body, handler, annot) ->
        let body = aux body in
        let handler = aux handler in
        Static_catch (i, vars, body, handler, annot)
      | Try_with(body, id, handler, annot) ->
        let body = aux body in
        let handler = aux handler in
        Try_with(body, id, handler, annot)
      | If_then_else(arg, ifso, ifnot, annot) ->
        let arg = aux arg in
        let ifso = aux ifso in
        let ifnot = aux ifnot in
        If_then_else(arg, ifso, ifnot, annot)
      | While(cond, body, annot) ->
        let cond = aux cond in
        let body = aux body in
        While(cond, body, annot)
      | Send(kind, met, obj, args, dbg, annot) ->
        let met = aux met in
        let obj = aux obj in
        let args = List.map aux args in
        Send(kind, met, obj, args, dbg, annot)
      | For(id, lo, hi, dir, body, annot) ->
        let lo = aux lo in
        let hi = aux hi in
        let body = aux body in
        For(id, lo, hi, dir, body, annot)
      | Assign(id, lam, annot) ->
        let lam = aux lam in
        Assign(id, lam, annot)
      | Unreachable _ -> tree
    in
    f exp
  and aux_named (named : _ Flambda.named) =
    let named : _ Flambda.named =
      match named with
      | Symbol _
      | Const _
      | Project_closure _
      | Move_within_set_of_closures _
      | Prim _ -> named
      | Set_of_closures ({ function_decls; free_vars;
            specialised_args }, annot) ->
        let function_decls : _ Flambda.function_declarations =
          if toplevel then function_decls
          else
            { function_decls with
              funs = Variable.Map.map
                  (fun (ffun : _ Flambda.function_declaration) ->
                    { ffun with body = aux ffun.body })
                function_decls.funs;
            }
        in
        Set_of_closures ({ function_decls; free_vars; specialised_args },
          annot)
      | Expr expr -> aux expr
    in
    f_named named
  in
  aux tree

let map f f_named tree = map_general ~toplevel:false f f_named tree
let map_toplevel f f_named tree = map_general ~toplevel:true f f_named tree

(*
let map_data (type t1) (type t2) (f:t1 -> t2)
      (tree:t1 Flambda.t) : t2 Flambda.t =
  let rec mapper : t1 Flambda.t -> t2 Flambda.t = function
    | Symbol (sym, v) -> Symbol (sym, f v)
    | Var (id, v) -> Var (id, f v)
    | Const (cst, v) -> Const (cst, f v)
    | Let(str, id, lam, body, v) ->
        Let(str, id, mapper lam, mapper body, f v)
    | Let_rec(defs, body, v) ->
        let defs = List.map (fun (id,def) -> (id, mapper def)) defs in
        Let_rec( defs, mapper body, f v)
    | Apply ({ func; args; kind; dbg }, v) ->
        Apply ({ func = mapper func;
                  args;
                  kind; dbg }, f v)
    | Set_of_closures ({ function_decls; free_vars;
                  specialised_args }, v) ->
        let function_decls =
          { function_decls with
            funs = Variable.Map.map
                (fun (ffun : _ Flambda.function_declaration) ->
                  { ffun with body = mapper ffun.body })
                function_decls.funs } in
        Set_of_closures ({ function_decls;
                    free_vars;
                    specialised_args }, f v)
    | Project_closure (project_closure, v) ->
      Project_closure (project_closure, f v)
    | Move_within_set_of_closures (move_within_set_of_closures, v) ->
      Move_within_set_of_closures (move_within_set_of_closures, f v)
    | Project_var (project_var, v) ->
      Project_var (project_var, f v)
    | Switch(arg, sw, v) ->
        let aux l = List.map (fun (i,v) -> i, mapper v) l in
        let sw = { sw with
                   consts = aux sw.consts;
                   blocks = aux sw.blocks;
                   failaction = Misc.may_map mapper sw.failaction } in
        Switch(mapper arg, sw, f v)
    | String_switch (arg, sw, def, v) ->
        let sw = List.map (fun (i,v) -> i, mapper v) sw in
        String_switch (mapper arg, sw, Misc.may_map mapper def, f v)
    | Send(kind, met, obj, args, dbg, v) ->
        Send(kind, mapper met, mapper obj, list_mapper args, dbg, f v)
    | Prim(prim, args, dbg, v) ->
        Prim(prim, args, dbg, f v)
    | Fseq_prim(prim, args, dbg, v) ->
        Fseq_prim(prim, list_mapper args, dbg, f v)
    | Static_raise (i, args, v) ->
        Static_raise (i, list_mapper args, f v)
    | Static_catch (i, vars, body, handler, v) ->
        Static_catch (i, vars, mapper body, mapper handler, f v)
    | Try_with(body, id, handler, v) ->
        Try_with(mapper body, id, mapper handler, f v)
    | If_then_else(arg, ifso, ifnot, v) ->
        If_then_else(mapper arg, mapper ifso, mapper ifnot, f v)
    | Fsequence(lam1, lam2, v) ->
        Fsequence(mapper lam1, mapper lam2, f v)
    | While(cond, body, v) ->
        While(mapper cond, mapper body, f v)
    | For(id, lo, hi, dir, body, v) ->
        For(id, mapper lo, mapper hi, dir, mapper body, f v)
    | Assign(id, lam, v) ->
        Assign(id, mapper lam, f v)
    | Unreachable v -> Unreachable (f v)
  and list_mapper l = List.map mapper l in
  mapper tree
*)
