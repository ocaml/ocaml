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
  | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable
  | Static_raise _ -> ()
  | Let { defining_expr; body; _ } ->
    f_named defining_expr;
    f body
  | Let_mutable (_mut_var, _var, body) ->
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
  | Static_catch (_,_,f1,f2) ->
    f f1; f f2;
  | Try_with (f1,_,f2) ->
    f f1; f f2
  | If_then_else (_,f1, f2) ->
    f f1;f f2
  | While (f1,f2) ->
    f f1; f f2
  | For { body; _ } -> f body

let map_subexpressions f f_named (tree:Flambda.t) : Flambda.t =
  match tree with
  | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable
  | Static_raise _ -> tree
  | Let { var; defining_expr; body; _ } ->
    Flambda.create_let var (f_named var defining_expr) (f body)
  | Let_rec (defs, body) ->
    let defs = List.map (fun (id, lam) -> id, f_named id lam) defs in
    Let_rec (defs, f body)
  | Let_mutable (mut_var, var, body) ->
    Let_mutable (mut_var, var, f body)
  | Switch (arg, sw) ->
    let sw =
      { sw with
        failaction = Misc.may_map f sw.failaction;
        consts = List.map (fun (i,v) -> i, f v) sw.consts;
        blocks = List.map (fun (i,v) -> i, f v) sw.blocks;
      }
    in
    Switch (arg, sw)
  | String_switch (arg, sw, def) ->
    let sw = List.map (fun (i,v) -> i, f v) sw in
    let def = Misc.may_map f def in
    String_switch(arg, sw, def)
  | Static_catch (i, vars, body, handler) ->
    let body = f body in
    let handler = f handler in
    Static_catch (i, vars, body, handler)
  | Try_with(body, id, handler) ->
    let body = f body in
    let handler = f handler in
    Try_with(body, id, handler)
  | If_then_else(arg, ifso, ifnot) ->
    let ifso = f ifso in
    let ifnot = f ifnot in
    If_then_else(arg, ifso, ifnot)
  | While(cond, body) ->
    let cond = f cond in
    let body = f body in
    While(cond, body)
  | For { bound_var; from_value; to_value; direction; body; } ->
    let body = f body in
    For { bound_var; from_value; to_value; direction; body; }

let iter_general = Flambda.iter_general

let iter f f_named t = iter_general ~toplevel:false f f_named (Is_expr t)
let iter_expr f t = iter f (fun _ -> ()) t
let iter_on_named f f_named t =
  iter_general ~toplevel:false f f_named (Is_named t)
let iter_named f_named t = iter (fun (_ : Flambda.t) -> ()) f_named t
let iter_named_on_named f_named named =
  iter_general ~toplevel:false (fun (_ : Flambda.t) -> ()) f_named
    (Is_named named)

let iter_toplevel f f_named t = iter_general ~toplevel:true f f_named (Is_expr t)
let iter_named_toplevel f f_named named =
  iter_general ~toplevel:true f f_named (Is_named named)

let iter_all_immutable_let_and_let_rec_bindings t ~f =
  iter_expr (function
      | Let { var; defining_expr; _ } -> f var defining_expr
      | Let_rec (defs, _) -> List.iter (fun (var, named) -> f var named) defs
      | _ -> ())
    t

let iter_all_toplevel_immutable_let_and_let_rec_bindings t ~f =
  iter_general ~toplevel:true
    (function
      | Let { var; defining_expr; _ } -> f var defining_expr
      | Let_rec (defs, _) -> List.iter (fun (var, named) -> f var named) defs
      | _ -> ())
    (fun _ -> ())
    (Is_expr t)

let iter_on_sets_of_closures f t =
  iter_named (function
      | Set_of_closures clos -> f clos
      | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Read_symbol_field _
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
  | Let_rec_symbol (defs, program) ->
    List.iter (function
        | (_, Flambda.Set_of_closures set_of_closures) ->
          Variable.Map.iter (fun _ (function_decl : Flambda.function_declaration) ->
              f function_decl.body)
            set_of_closures.function_decls.funs
        | _ -> ()) defs;
    iter_exprs_at_toplevel_of_program program ~f
  | Let_symbol (_, _, program)
  | Import_symbol (_, program) ->
    iter_exprs_at_toplevel_of_program program ~f
  | Initialize_symbol (_, _, fields, program) ->
    List.iter f fields;
    iter_exprs_at_toplevel_of_program program ~f
  | Effect (expr, program) ->
    f expr;
    iter_exprs_at_toplevel_of_program program ~f
  | End _ -> ()

let iter_named_of_program program ~f =
  iter_exprs_at_toplevel_of_program program ~f:(iter_named f)

let iter_on_set_of_closures_of_program (program : Flambda.program) ~f =
  let rec loop (program : Flambda.program) =
    match program with
    | Let_symbol (_, Set_of_closures set_of_closures, program) ->
      f ~constant:true set_of_closures;
      Variable.Map.iter (fun _ (function_decl : Flambda.function_declaration) ->
          iter_on_sets_of_closures (f ~constant:false) function_decl.body)
        set_of_closures.function_decls.funs;
      loop program
    | Let_rec_symbol (defs, program) ->
      List.iter (function
          | (_, Flambda.Set_of_closures set_of_closures) ->
            f ~constant:true set_of_closures;
            Variable.Map.iter (fun _ (function_decl : Flambda.function_declaration) ->
                iter_on_sets_of_closures (f ~constant:false) function_decl.body)
              set_of_closures.function_decls.funs
          | _ -> ()) defs;
      loop program
    | Let_symbol (_, _, program)
    | Import_symbol (_, program) ->
      loop program
    | Initialize_symbol (_, _, fields, program) ->
      List.iter (iter_on_sets_of_closures (f ~constant:false)) fields;
      loop program
    | Effect (expr, program) ->
      iter_on_sets_of_closures (f ~constant:false) expr;
      loop program
    | End _ -> ()
  in
  loop program

let rec iter_constant_defining_values_on_program
      (program : Flambda.program) ~f =
  match program with
  | Let_symbol (_, const, program) ->
    f const;
    iter_constant_defining_values_on_program program ~f
  | Let_rec_symbol (defs, program) ->
    List.iter (fun (_, const) -> f const) defs;
    iter_constant_defining_values_on_program program ~f
  | Import_symbol (_, program) ->
    iter_constant_defining_values_on_program program ~f
  | Initialize_symbol (_, _, _, program) ->
    iter_constant_defining_values_on_program program ~f
  | Effect (_, program) ->
    iter_constant_defining_values_on_program program ~f
  | End _ -> ()

let map_general ~toplevel f f_named tree =
  let rec aux (tree : Flambda.t) =
    match tree with
    | Let _ ->
      Flambda.map_lets tree ~for_defining_expr:aux_named ~for_last_body:aux
        ~after_rebuild:f
    | _ ->
      let exp : Flambda.t =
        match tree with
        | Var _ | Apply _ | Assign _ | Send _ | Proved_unreachable
        | Static_raise _ -> tree
        | Let _ -> assert false
        | Let_mutable (mut_var, var, body) ->
          let body = aux body in
          Let_mutable (mut_var, var, body)
        | Let_rec (defs, body) ->
          let defs = List.map (fun (id, lam) -> id, aux_named id lam) defs in
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
  and aux_named (id : Variable.t) (named : Flambda.named) =
    let named : Flambda.named =
      match named with
      | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Read_symbol_field _ -> named
      | Set_of_closures ({ function_decls; free_vars; specialised_args }) ->
        if toplevel then named
        else
          let function_decls =
            let funs =
              Variable.Map.map (fun (func_decl : Flambda.function_declaration) ->
                  Flambda.create_function_declaration
                    ~params:func_decl.params
                    ~body:(aux func_decl.body)
                    ~stub:func_decl.stub
                    ~dbg:func_decl.dbg
                    ~inline:func_decl.inline
                    ~is_a_functor:func_decl.is_a_functor)
                function_decls.funs
            in
            Flambda.update_function_declarations function_decls ~funs
          in
          let set_of_closures =
            Flambda.create_set_of_closures ~function_decls ~free_vars
              ~specialised_args
          in
          Set_of_closures set_of_closures
      | Expr expr -> Expr (aux expr)
    in
    f_named id named
  in
  aux tree

let iter_apply_on_program program ~f =
  iter_exprs_at_toplevel_of_program program ~f:(fun expr ->
    iter (function
        | Apply apply -> f apply
        | _ -> ())
      (fun _ -> ())
      expr)

let map f f_named tree = map_general ~toplevel:false f (fun _ n -> f_named n) tree
let map_expr f tree = map f (fun named -> named) tree
let map_named f_named tree = map (fun expr -> expr) f_named tree
let map_named_with_id f_named tree =
  map_general ~toplevel:false (fun expr -> expr) f_named tree
let map_toplevel f f_named tree =
  map_general ~toplevel:true f (fun _ n -> f_named n) tree
let map_toplevel_expr f_expr tree =
  map_toplevel f_expr (fun named -> named) tree
let map_toplevel_named f_named tree =
  map_toplevel (fun tree -> tree) f_named tree

let map_symbols tree ~f =
  map_named (function
      | Symbol sym -> Symbol (f sym)
      | Read_symbol_field (sym, field) -> Read_symbol_field (f sym, field)
      | (Const _ | Allocated_const _ | Set_of_closures _ | Read_mutable _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _) as named -> named)
    tree

let map_symbols_on_set_of_closures
    { Flambda.function_decls; free_vars; specialised_args }
    ~f =
  let function_decls =
    let funs =
      Variable.Map.map (fun (func_decl : Flambda.function_declaration) ->
          let body = map_symbols func_decl.body ~f in
          Flambda.create_function_declaration
            ~params:func_decl.params
             ~body
             ~stub:func_decl.stub
             ~dbg:func_decl.dbg
             ~inline:func_decl.inline
             ~is_a_functor:func_decl.is_a_functor)
        function_decls.funs
    in
    Flambda.update_function_declarations function_decls ~funs
  in
  Flambda.create_set_of_closures ~function_decls ~free_vars
    ~specialised_args

let map_toplevel_sets_of_closures tree ~f =
  map_toplevel_named (function
      | Set_of_closures set_of_closures -> Set_of_closures (f set_of_closures)
      | (Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Read_symbol_field _
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
      | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Expr _ | Read_mutable _
      | Read_symbol_field _) as named -> named)
    tree

let map_project_var_to_expr_opt tree ~f =
  map_named (function
      | Project_var project_var ->
        begin match f project_var with
        | None -> Project_var project_var
        | Some expr -> Expr expr
        end
      | (Symbol _ | Const _ | Allocated_const _
      | Set_of_closures _ | Project_closure _ | Move_within_set_of_closures _
      | Prim _ | Expr _ | Read_mutable _ | Read_symbol_field _)
          as named -> named)
    tree

let map_toplevel_project_var_to_expr_opt tree ~f =
  map_toplevel_named (function
      | Project_var project_var ->
        begin match f project_var with
        | None -> Project_var project_var
        | Some expr -> Expr expr
        end
      | (Symbol _ | Const _ | Allocated_const _
      | Set_of_closures _ | Project_closure _ | Move_within_set_of_closures _
      | Prim _ | Expr _ | Read_mutable _ | Read_symbol_field _)
          as named -> named)
    tree

let map_project_var_to_named_opt tree ~f =
  map_named (function
      | Project_var project_var ->
        begin match f project_var with
        | None -> Project_var project_var
        | Some named -> named
        end
      | (Symbol _ | Const _ | Allocated_const _
      | Set_of_closures _ | Project_closure _ | Move_within_set_of_closures _
      | Prim _ | Expr _ | Read_mutable _ | Read_symbol_field _)
          as named -> named)
    tree

let map_function_bodies (set_of_closures : Flambda.set_of_closures) ~f =
  let funs =
    Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
        Flambda.create_function_declaration ~body:(f function_decl.body)
          ~params:function_decl.params
          ~stub:function_decl.stub
          ~dbg:function_decl.dbg
          ~inline:function_decl.inline
          ~is_a_functor:function_decl.is_a_functor)
      set_of_closures.function_decls.funs
  in
  let function_decls =
    Flambda.update_function_declarations set_of_closures.function_decls ~funs
  in
  Flambda.create_set_of_closures
    ~function_decls
    ~free_vars:set_of_closures.free_vars
    ~specialised_args:set_of_closures.specialised_args

let rec map_sets_of_closures_of_program (program : Flambda.program)
    ~(f : Flambda.set_of_closures -> Flambda.set_of_closures) : Flambda.program =
  let map_constant_set_of_closures (set_of_closures:Flambda.set_of_closures) =
    let funs =
      Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
          let body = map_sets_of_closures ~f function_decl.body in
          Flambda.create_function_declaration ~body
            ~params:function_decl.params
            ~stub:function_decl.stub
            ~dbg:function_decl.dbg
            ~inline:function_decl.inline
            ~is_a_functor:function_decl.is_a_functor)
        set_of_closures.function_decls.funs
    in
    let function_decls =
      Flambda.update_function_declarations set_of_closures.function_decls ~funs
    in
    let set_of_closures = f set_of_closures in
    Flambda.create_set_of_closures ~function_decls
      ~free_vars:set_of_closures.free_vars
      ~specialised_args:set_of_closures.specialised_args
  in
  match program with
  | Let_symbol (symbol, Set_of_closures set_of_closures, program) ->
    Let_symbol
      (symbol,
       Set_of_closures (map_constant_set_of_closures set_of_closures),
       map_sets_of_closures_of_program program ~f)
  | Let_symbol (symbol, const, program) ->
    Let_symbol (symbol, const, map_sets_of_closures_of_program program ~f)
  | Let_rec_symbol (defs, program) ->
    let defs =
      List.map (function
          | (var, Flambda.Set_of_closures set_of_closures) ->
            var, Flambda.Set_of_closures (map_constant_set_of_closures set_of_closures)
          | def -> def)
        defs
    in
    Let_rec_symbol (defs, map_sets_of_closures_of_program program ~f)
  | Import_symbol (symbol, program) ->
    Import_symbol (symbol, map_sets_of_closures_of_program program ~f)
  | Initialize_symbol (symbol, tag, fields, program) ->
    let fields = List.map (map_sets_of_closures ~f) fields in
    Initialize_symbol (symbol, tag, fields,
      map_sets_of_closures_of_program program ~f)
  | Effect (expr, program) ->
    Effect (map_sets_of_closures ~f expr,
      map_sets_of_closures_of_program program ~f)
  | End s -> End s

let rec map_exprs_at_toplevel_of_program (program : Flambda.program)
    ~(f : Flambda.t -> Flambda.t) : Flambda.program =
  let map_constant_set_of_closures (set_of_closures:Flambda.set_of_closures) =
    let funs =
      Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
          let body = f function_decl.body in
          Flambda.create_function_declaration ~body
            ~params:function_decl.params
            ~stub:function_decl.stub
            ~dbg:function_decl.dbg
            ~inline:function_decl.inline
            ~is_a_functor:function_decl.is_a_functor)
        set_of_closures.function_decls.funs
    in
    let function_decls =
      Flambda.update_function_declarations set_of_closures.function_decls ~funs
    in
    Flambda.create_set_of_closures ~function_decls
      ~free_vars:set_of_closures.free_vars
      ~specialised_args:set_of_closures.specialised_args
  in
  match program with
  | Let_symbol (symbol, Set_of_closures set_of_closures, program) ->
    Let_symbol
      (symbol,
       Set_of_closures (map_constant_set_of_closures set_of_closures),
       map_exprs_at_toplevel_of_program program ~f)
  | Let_symbol (symbol, const, program) ->
    Let_symbol (symbol, const, map_exprs_at_toplevel_of_program program ~f)
  | Let_rec_symbol (defs, program) ->
    let defs =
      List.map (function
          | (var, Flambda.Set_of_closures set_of_closures) ->
            var, Flambda.Set_of_closures (map_constant_set_of_closures set_of_closures)
          | def -> def)
        defs
    in
    Let_rec_symbol (defs, map_exprs_at_toplevel_of_program program ~f)
  | Import_symbol (symbol, program) ->
    Import_symbol (symbol, map_exprs_at_toplevel_of_program program ~f)
  | Initialize_symbol (symbol, tag, fields, program) ->
    let fields = List.map f fields in
    Initialize_symbol (symbol, tag, fields,
      map_exprs_at_toplevel_of_program program ~f)
  | Effect (expr, program) ->
    Effect (f expr,
      map_exprs_at_toplevel_of_program program ~f)
  | End s -> End s

let map_named_of_program (program : Flambda.program)
      ~(f : Variable.t -> Flambda.named -> Flambda.named) : Flambda.program =
  map_exprs_at_toplevel_of_program program
      ~f:(fun expr -> map_named_with_id f expr)

let map_all_immutable_let_and_let_rec_bindings (expr : Flambda.t)
      ~(f : Variable.t -> Flambda.named -> Flambda.named) : Flambda.t =
  map_named_with_id f expr
