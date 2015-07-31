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
    | Symbol _ | Const _ | Allocated_const _ | Predefined_exn _
    | Project_closure _ | Project_var _ | Move_within_set_of_closures _
    | Prim _ -> ()
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
let iter_expr f t = iter f (fun _ -> ()) t
let iter_named f_named t = iter (fun (_ : Flambda.t) -> ()) f_named t
let iter_named_on_named f_named named =
  iter_general ~toplevel:false (fun (_ : Flambda.t) -> ()) f_named (Named named)

let iter_toplevel f f_named t = iter_general ~toplevel:true f f_named (Expr t)
let iter_named_toplevel f f_named named =
  iter_general ~toplevel:true f f_named (Named named)

let iter_all_let_and_let_rec_bindings t ~f =
  iter_expr (function
      | Let (_, var, named, _) -> f var named
      | Let_rec (defs, _) -> List.iter (fun (var, named) -> f var named) defs
      | _ -> ())
    t

let iter_on_sets_of_closures f t =
  iter_named (function
      | Set_of_closures clos -> f clos
      | Symbol _ | Const _ | Allocated_const _ | Predefined_exn _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _ -> ())
    t

let rec iter_exprs_at_toplevel_of_program (program : Flambda.program) ~f =
  match program with
  | Let_symbol (_, Set_of_closures set_of_closures, program) ->
    Variable.Map.iter (fun _ (function_decl : Flambda.function_declaration) ->
        f function_decl.body)
      set_of_closures.function_decls.funs;
    iter_exprs_at_toplevel_of_program program ~f
  | Let_symbol (_, _, program)
  | Import_symbol (_, program) ->
    iter_exprs_at_toplevel_of_program program ~f
  | Initialize_symbol (_, expr, program) ->
    f expr;
    iter_exprs_at_toplevel_of_program program ~f
  | End -> ()

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
      | Symbol _ | Const _ | Allocated_const _ | Predefined_exn _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ -> named
      | Set_of_closures ({ function_decls; free_vars; specialised_args }) ->
        if toplevel then named
        else
          let function_decls : Flambda.function_declarations =
            { function_decls with
              funs = Variable.Map.map
                  (fun (ffun : Flambda.function_declaration) ->
                    Flambda.create_function_declaration
                      ~params:ffun.params
                      ~body:(aux ffun.body)
                      ~stub:ffun.stub
                      ~dbg:ffun.dbg)
                function_decls.funs;
            }
          in
          let set_of_closures =
            Flambda.create_set_of_closures ~function_decls ~free_vars
              ~specialised_args
          in
          Set_of_closures set_of_closures
      | Expr expr -> Expr (aux expr)
    in
    f_named named
  in
  aux tree

let map f f_named tree = map_general ~toplevel:false f f_named tree
let map_expr f tree = map f (fun named -> named) tree
let map_named f_named tree = map (fun expr -> expr) f_named tree
(* CR mshinwell: rename "toplevel" *)
let map_toplevel f f_named tree = map_general ~toplevel:true f f_named tree
let map_toplevel_named f_named tree =
  map_toplevel (fun tree -> tree) f_named tree

let map_symbols tree ~f =
  map_named (function
      | Symbol sym -> Symbol (f sym)
      | (Const _ | Allocated_const _ | Predefined_exn _ | Set_of_closures _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _) as named -> named)
    tree

let map_toplevel_sets_of_closures tree ~f =
  map_toplevel_named (function
      | Set_of_closures set_of_closures -> Set_of_closures (f set_of_closures)
      | (Symbol _ | Const _ | Allocated_const _ | Predefined_exn _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _) as named -> named)
    tree

let map_apply tree ~f =
  map (function
      | Apply apply -> Apply (f apply)
      | expr -> expr)
    (fun named -> named)
    tree

let map_sets_of_closures tree ~f =
  map_named (function
      | Set_of_closures set_of_closures -> Set_of_closures (f set_of_closures)
      | (Symbol _ | Const _ | Allocated_const _ | Project_closure _
      | Predefined_exn _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _) as named -> named)
    tree

let map_project_var_to_expr_opt tree ~f =
  map_named (function
      | Project_var project_var ->
        begin match f project_var with
        | None -> Project_var project_var
        | Some expr -> Expr expr
        end
      | (Symbol _ | Const _ | Allocated_const _ | Predefined_exn _
      | Set_of_closures _ | Project_closure _ | Move_within_set_of_closures _
      | Prim _ | Expr _)
          as named -> named)
    tree

let map_function_bodies (set_of_closures : Flambda.set_of_closures) ~f =
  let funs =
    Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
        Flambda.create_function_declaration ~body:(f function_decl.body)
          ~params:function_decl.params
          ~stub:function_decl.stub
          ~dbg:function_decl.dbg)
      set_of_closures.function_decls.funs
  in
  Flambda.create_set_of_closures
    ~function_decls:{ set_of_closures.function_decls with funs; }
    ~free_vars:set_of_closures.free_vars
    ~specialised_args:set_of_closures.specialised_args

let rec map_exprs_at_toplevel_of_program (program : Flambda.program)
      ~(f : Flambda.t -> Flambda.t) : Flambda.program =
  match program with
  | Let_symbol (symbol, Set_of_closures set_of_closures, program) ->
    let funs =
      Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
          let body = f function_decl.body in
          Flambda.create_function_declaration ~body
            ~params:function_decl.params
            ~stub:function_decl.stub
            ~dbg:function_decl.dbg)
        set_of_closures.function_decls.funs
    in
    let function_decls = { set_of_closures.function_decls with funs; } in
    let set_of_closures =
      Flambda.create_set_of_closures ~function_decls
        ~free_vars:set_of_closures.free_vars
        ~specialised_args:set_of_closures.specialised_args
    in
    Let_symbol (symbol, Set_of_closures set_of_closures,
      map_exprs_at_toplevel_of_program program ~f)
  | Let_symbol (symbol, const, program) ->
    Let_symbol (symbol, const, map_exprs_at_toplevel_of_program program ~f)
  | Import_symbol (symbol, program) ->
    Import_symbol (symbol, map_exprs_at_toplevel_of_program program ~f)
  | Initialize_symbol (symbol, expr, program) ->
    Initialize_symbol (symbol, f expr,
      map_exprs_at_toplevel_of_program program ~f)
  | End -> End
