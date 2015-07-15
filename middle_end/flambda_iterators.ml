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

let apply_on_subexpressions f f_named (flam : Flambda.t) =
  match flam with
  | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable -> ()
  | Let (_, _, defining_expr, body) ->
    f_named defining_expr;
    f body
  | Let_rec (defs, body) ->
    List.iter (fun (_,l) -> f_named l) defs;
    f body
  | Switch (_, sw) ->
    List.iter (fun (_,l) -> f l) sw.consts;
    List.iter (fun (_,l) -> f l) sw.blocks;
    Misc.may f sw.failaction
  | String_switch (_, sw, def) ->
    List.iter (fun (_,l) -> f l) sw;
    Misc.may f def
  | Static_raise (_,l) ->
    List.iter f l
  | Static_catch (_,_,f1,f2) ->
    f f1; f f2;
  | Try_with (f1,_,f2) ->
    f f1; f f2
  | If_then_else (_,f1, f2) ->
    f f1;f f2
  | While (f1,f2) ->
    f f1; f f2
  | For { body; _ } -> f body

type maybe_named =
  | Expr of Flambda.t
  | Named of Flambda.named

let iter_general ~toplevel f f_named maybe_named =
  let rec aux (t : Flambda.t) =
    f t;
    match t with
    | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable -> ()
    | Let ( _, _, f1, f2) ->
      aux_named f1;
      aux f2
    | Let_rec (defs, body) ->
      List.iter (fun (_,l) -> aux_named l) defs;
      aux body
    | Try_with (f1,_,f2)
    | While (f1,f2)
    | Static_catch (_,_,f1,f2) ->
      aux f1; aux f2
    | For { body; _ } -> aux body
    | If_then_else (_, f1, f2) ->
      aux f1; aux f2
    | Static_raise (_,l) ->
      iter_list l
    | Switch (_, sw) ->
      List.iter (fun (_,l) -> aux l) sw.consts;
      List.iter (fun (_,l) -> aux l) sw.blocks;
      Misc.may aux sw.failaction
    | String_switch (_, sw, def) ->
      List.iter (fun (_,l) -> aux l) sw;
      Misc.may aux def
  and aux_named (named : Flambda.named) =
    f_named named;
    match named with
    | Symbol _ | Const _ | Project_closure _ | Project_var _
    | Move_within_set_of_closures _ | Prim _ -> ()
    | Set_of_closures ({ function_decls = funcs; free_vars = _;
          specialised_args = _}) ->
      if not toplevel then begin
        Variable.Map.iter (fun _ (decl : Flambda.function_declaration) ->
            aux decl.body)
          funcs.funs
      end
    | Expr flam -> aux flam
  and iter_list l = List.iter aux l in
  match maybe_named with
  | Expr expr -> aux expr
  | Named named -> aux_named named

let iter f f_named t = iter_general ~toplevel:false f f_named (Expr t)
let iter_named f_named t = iter (fun (_ : Flambda.t) -> ()) f_named t
let iter_toplevel f f_named t = iter_general ~toplevel:true f f_named (Expr t)
let iter_named_toplevel f f_named named =
  iter_general ~toplevel:true f f_named (Named named)

let iter_on_sets_of_closures f t =
  iter_named (function
      | Set_of_closures clos -> f clos
      | Symbol _ | Const _ | Project_closure _ | Move_within_set_of_closures _
      | Project_var _ | Prim _ | Expr _ -> ())
    t

let map_general ~toplevel f f_named tree =
  let rec aux (tree : Flambda.t) =
    let exp : Flambda.t =
      match tree with
      | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable -> tree
      | Let (str, id, lam, body) ->
        let lam = aux_named lam in
        let body = aux body in
        Let (str, id, lam, body)
      | Let_rec (defs, body) ->
        let defs = List.map (fun (id, lam) -> id, aux_named lam) defs in
        let body = aux body in
        Let_rec (defs, body)
      | Switch (arg, sw) ->
        let sw =
          { sw with
            failaction = Misc.may_map aux sw.failaction;
            consts = List.map (fun (i,v) -> i, aux v) sw.consts;
            blocks = List.map (fun (i,v) -> i, aux v) sw.blocks;
          }
        in
        Switch (arg, sw)
      | String_switch (arg, sw, def) ->
        let sw = List.map (fun (i,v) -> i, aux v) sw in
        let def = Misc.may_map aux def in
        String_switch(arg, sw, def)
      | Static_raise(i, args) ->
        let args = List.map aux args in
        Static_raise (i, args)
      | Static_catch (i, vars, body, handler) ->
        let body = aux body in
        let handler = aux handler in
        Static_catch (i, vars, body, handler)
      | Try_with(body, id, handler) ->
        let body = aux body in
        let handler = aux handler in
        Try_with(body, id, handler)
      | If_then_else(arg, ifso, ifnot) ->
        let ifso = aux ifso in
        let ifnot = aux ifnot in
        If_then_else(arg, ifso, ifnot)
      | While(cond, body) ->
        let cond = aux cond in
        let body = aux body in
        While(cond, body)
      | For { bound_var; from_value; to_value; direction; body; } ->
        let body = aux body in
        For { bound_var; from_value; to_value; direction; body; }
    in
    f exp
  and aux_named (named : Flambda.named) =
    let named : Flambda.named =
      match named with
      | Symbol _ | Const _ | Project_closure _ | Move_within_set_of_closures _
      | Project_var _ | Prim _ -> named
      | Set_of_closures ({ function_decls; free_vars;
            specialised_args }) ->
        let function_decls : Flambda.function_declarations =
          if toplevel then function_decls
          else
            { function_decls with
              funs = Variable.Map.map
                  (fun (ffun : Flambda.function_declaration) ->
                    { ffun with body = aux ffun.body })
                function_decls.funs;
            }
        in
        Set_of_closures { function_decls; free_vars; specialised_args }
      | Expr expr -> Expr (aux expr)
    in
    f_named named
  in
  aux tree

let map f f_named tree = map_general ~toplevel:false f f_named tree
let map_named f_named tree = map (fun expr -> expr) f_named tree
(* CR mshinwell: rename "toplevel" *)
let map_toplevel f f_named tree = map_general ~toplevel:true f f_named tree

let map_symbols tree ~f =
  map_named (function
      | Symbol sym -> Symbol (f sym)
      | (Const _ | Set_of_closures _ | Project_closure _
      | Move_within_set_of_closures _ | Project_var _ | Prim _
      | Expr _) as named -> named)
    tree
