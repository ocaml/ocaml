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

(* This cannot be done in a single recursive pass due to expressions like:

  let ... =
    let v = ... in

    let rec f1 x =
      let f2 y =
        f1 rec_list
      in
      f2 v
    and rec_list = f1 :: rec_list

    ...

  f1, f2 and rec_list are constants iff v is a constant.

  To handle this we implement it as 2 loops populating a 'not constant'
  set NC:

   - the first one collects informations on the expressions to add dependencies
     between variables and mark values directly known as not constant:

      f1 in NC => rec_list in NC
      f2 in NC => f1 in NC
      rec_list in NC => f2 in NC
      v in NC => f1 in NC

     and if for instance if v is:
      let v = if ... then 1 else 2 in
     it adds

      v in NC

   - the second propagates the implications
*)

module Int = Ext_types.Int

type result = {
  id : Variable.Set.t;
  closure : Set_of_closures_id.Set.t;
}

module type Param = sig
  val expr : Flambda.t
  val for_clambda : bool
  val compilation_unit : Compilation_unit.t
  val toplevel : bool
end

module NotConstants(P:Param) = struct
  let for_clambda = P.for_clambda
  let compilation_unit = P.compilation_unit

  type dep =
    | Closure of Set_of_closures_id.t
    | Var of Variable.t
    | Global of int (* position of the global *)

  (* Sets representing NC *)
  let variables = ref Variable.Set.empty
  let closures = ref Set_of_closures_id.Set.empty
  let globals = ref Int.Set.empty

  (* if the table associates [v1;v2;...;vn] to v, it represents
     v in NC => v1 in NC /\ v2 in NC ... /\ vn in NC *)
  let id_dep_table : dep list Variable.Tbl.t = Variable.Tbl.create 100
  let fun_dep_table : dep list Set_of_closures_id.Tbl.t = Set_of_closures_id.Tbl.create 100
  let glob_dep_table : dep list Int.Tbl.t = Int.Tbl.create 100

  (* adds in the tables 'dep in NC => curr in NC' *)
  let register_implication ~in_nc:dep ~implies_in_nc:curr =
    List.iter (fun curr ->
      match dep with
      | Var id ->
        let t = try Variable.Tbl.find id_dep_table id
        with Not_found -> [] in
        Variable.Tbl.replace id_dep_table id (curr :: t)
      | Closure cl ->
        let t = try Set_of_closures_id.Tbl.find fun_dep_table cl
        with Not_found -> [] in
        Set_of_closures_id.Tbl.replace fun_dep_table cl (curr :: t)
      | Global i ->
        let t = try Int.Tbl.find glob_dep_table i
        with Not_found -> [] in
        Int.Tbl.replace glob_dep_table i (curr :: t))
      curr

  (* adds 'curr in NC' *)
  let mark_curr curr =
    List.iter (function
      | Var id ->
        if not (Variable.Set.mem id !variables)
        then variables := Variable.Set.add id !variables
      | Closure cl ->
        if not (Set_of_closures_id.Set.mem cl !closures)
        then closures := Set_of_closures_id.Set.add cl !closures
      | Global i ->
        if not (Int.Set.mem i !globals)
        then globals := Int.Set.add i !globals)
      curr

  (* First loop: iterates on the tree to mark dependencies.

     curr is the variables or closures to wich we add constraints like
     '... in NC => curr in NC' or 'curr in NC'

     It can be empty when no constraint can be added like in the toplevel
     expression or in the body of a function.
  *)
  let rec mark_loop ~toplevel (curr : dep list) (flam : Flambda.t) =
    match flam with
    | Let(str, var, lam, body) ->
      if str = Flambda.Mutable then mark_curr [Var var];
      mark_named ~toplevel [Var var] lam;
      (* adds 'var in NC => curr in NC'
         This is not really necessary, but compiling this correctly is
         trickier than eliminating that earlier. *)
      mark_var var curr;
      mark_loop ~toplevel curr body

    | Let_rec(defs, body) ->
      List.iter (fun (var, def) ->
          mark_named ~toplevel [Var var] def;
          (* adds 'var in NC => curr in NC' same remark as let case *)
          mark_var var curr)
        defs;
      mark_loop ~toplevel curr body

    | Var var -> mark_var var curr

    (* Not constant cases: we mark directly 'curr in NC' and mark
       bound variables as in NC also *)

    | Assign { being_assigned; new_value; } ->
      (* CR mshinwell: not sure about this next comment.  Check this is
         right *)
      (* the assigned is also not constant *)
      mark_curr [Var being_assigned];
      mark_curr [Var new_value]

    | Try_with (f1,id,f2) ->
      mark_curr [Var id];
      mark_curr curr;
      mark_loop ~toplevel [] f1;
      mark_loop ~toplevel [] f2

    | Static_catch (_,ids,f1,f2) ->
      List.iter (fun id -> mark_curr [Var id]) ids;
      mark_curr curr;
      mark_loop ~toplevel [] f1;
      mark_loop ~toplevel [] f2
      (* If recursive staticcatch is introduced: this becomes
         ~toplevel:false *)

    | For { bound_var; from_value; to_value; direction = _; body; } ->
      mark_curr [Var bound_var];
      mark_curr [Var from_value];
      mark_curr [Var to_value];
      mark_curr curr;
      mark_loop ~toplevel:false [] body

    | While (f1,body) ->
      mark_curr curr;
      mark_loop ~toplevel [] f1;
      mark_loop ~toplevel:false [] body

    | If_then_else (f1,f2,f3) ->
      mark_curr curr;
      mark_curr [Var f1];
      mark_loop ~toplevel [] f2;
      mark_loop ~toplevel [] f3

    | Static_raise (_,l) ->
      mark_curr curr;
      List.iter (mark_loop ~toplevel []) l

    | Apply ({func; args; _ }) ->
      mark_curr [Var func];
      mark_curr curr;
      mark_vars args curr;

    | Switch (arg,sw) ->
      mark_curr curr;
      mark_var arg curr;
      List.iter (fun (_,l) -> mark_loop ~toplevel [] l) sw.consts;
      List.iter (fun (_,l) -> mark_loop ~toplevel [] l) sw.blocks;
      Misc.may (fun l -> mark_loop ~toplevel [] l) sw.failaction

    | String_switch (arg,sw,def) ->
      mark_curr curr;
      mark_var arg curr;
      List.iter (fun (_,l) -> mark_loop ~toplevel [] l) sw;
      Misc.may (fun l -> mark_loop ~toplevel [] l) def

    | Send { kind = _; meth; obj; args; dbg = _; } ->
      mark_var meth curr;
      mark_var obj curr;
      List.iter (fun arg -> mark_var arg curr) args

    | Proved_unreachable ->
      mark_curr curr

  and mark_named ~toplevel curr (named : Flambda.named) =
    match named with
    | Set_of_closures (set_of_closures) ->
      mark_loop_set_of_closures ~toplevel curr set_of_closures

    | Const _ -> ()

    (* a symbol does not necessarilly points to a constant: toplevel
       modules are declared as symbols, but can constain not constant
       values *)
    | Symbol(_sym) ->
      (* for a later patch: *)
      (* if not (SymbolSet.mem sym *)
      (*           (Compilenv.approx_env ()).Flambdaexport.ex_constants) *)
      (* then mark_curr curr *)

      (* Until we have informations from external modules, we consider
         symbols are not constants (e.g. a module symbol).
         For clambda constants are 'things that can be refered to by a symbol',
         and it is obviously the case. *)
      if not for_clambda
      then mark_curr curr

    (* globals are symbols: handle like symbols *)
    | Prim(Lambda.Pgetglobal _id, [], _) ->
      if not for_clambda
      then mark_curr curr

    (* Constant constructors: those expressions are constant if all their parameters are:
       - makeblock is compiled to a constant block
       - offset is compiled to a pointer inside a constant closure.
         See Cmmgen for the details

       makeblock(Mutable) can be a 'constant' if it is allocated at toplevel: if this
       expression is evaluated only once. This is only allowed when for_clambda, i.e.
       when we are checking wether a variable can be statically allocated.
    *)

    | Prim(Lambda.Pmakeblock(_tag, Asttypes.Immutable), args, _dbg) ->
      mark_vars args curr

(*  (* If global mutables are allowed: *)
    | Prim(Lambda.Pmakeblock(_tag, Asttypes.Mutable), args, _dbg, _)
      when for_clambda && toplevel ->
      List.iter (mark_loop ~toplevel curr) args
*)

    | Project_closure ({ set_of_closures; closure_id; }) ->
      if Closure_id.in_compilation_unit compilation_unit closure_id then
        mark_var set_of_closures curr
      else
        mark_curr curr
    | Move_within_set_of_closures
        ({ closure; start_from = _; move_to = _ }) ->
      register_implication ~in_nc:(Var closure) ~implies_in_nc:curr
    | Project_var ({ closure; closure_id = _; var = _ }) ->
      register_implication ~in_nc:(Var closure) ~implies_in_nc:curr
    | Prim(Lambda.Pfield _, [f1], _) ->
      if for_clambda then mark_curr curr;
      mark_var f1 curr
    | Prim(Lambda.Pgetglobalfield(id,i), [], _) ->
      (* adds 'global i in NC => curr in NC' *)
      if Ident.same id (Compilation_unit.get_persistent_ident compilation_unit)
      then
        register_implication ~in_nc:(Global i) ~implies_in_nc:curr
      else
        mark_curr curr

    | Prim(Lambda.Psetglobalfield (_,i), [f], _) ->
      mark_curr curr;
      (* adds 'f in NC => global i in NC' *)
      register_implication ~in_nc:(Var f) ~implies_in_nc:[Global i]

    | Prim (_, args, _) ->
      mark_curr curr;
      mark_vars args curr
    | Expr flam ->
      mark_loop ~toplevel curr flam

  and mark_var var curr =
    (* adds 'id in NC => curr in NC' *)
    register_implication ~in_nc:(Var var) ~implies_in_nc:curr

  and mark_vars vars curr =
    (* adds 'id in NC => curr in NC' *)
    List.iter (fun var -> mark_var var curr) vars

  (* CR mshinwell: [toplevel] is now unused, is that correct?
     CRX pchambart: [toplevel] is intended for allowing allocations of
        mutable block statically. This is not yet activated because of
        missing parts in the gc currently. *)
  and mark_loop_set_of_closures ~toplevel:_ curr
        { Flambda. function_decls; free_vars; specialised_args } =
    (* If a function in the set of closures is specialised, do not consider
       it constant. *)
    (* CR mshinwell for pchambart: This needs more explanation. *)
    Variable.Map.iter (fun _ id ->
          register_implication
            ~in_nc:(Var id)
            ~implies_in_nc:[Closure function_decls.set_of_closures_id])
        specialised_args;
    (* adds 'function_decls in NC => curr in NC' *)
    register_implication ~in_nc:(Closure function_decls.set_of_closures_id)
      ~implies_in_nc:curr;
    (* a closure is constant if its free variables are constants. *)
    Variable.Map.iter (fun inner_id var ->
        register_implication ~in_nc:(Var var)
          ~implies_in_nc:[Var inner_id; Closure function_decls.set_of_closures_id])
      free_vars;
    Variable.Map.iter (fun fun_id (ffunc : Flambda.function_declaration) ->
        (* for each function f in a closure c 'c in NC => f' *)
        register_implication ~in_nc:(Closure function_decls.set_of_closures_id)
          ~implies_in_nc:[Var fun_id];
        (* function parameters are in NC *)
        List.iter (fun id -> mark_curr [Var id]) ffunc.params;
        mark_loop ~toplevel:false [] ffunc.body)
      function_decls.funs

  (* Second loop: propagates implications *)
  let propagate () =
    (* Set of variables/closures added to NC but not their dependencies *)
    let q = Queue.create () in
    Variable.Set.iter (fun v -> Queue.push (Var v) q) !variables;
    Set_of_closures_id.Set.iter (fun v -> Queue.push (Closure v) q) !closures;
    while not (Queue.is_empty q) do
      let deps = try match Queue.take q with
        | Var e -> Variable.Tbl.find id_dep_table e
        | Closure cl -> Set_of_closures_id.Tbl.find fun_dep_table cl
        | Global i -> Int.Tbl.find glob_dep_table i
      with Not_found -> [] in
      List.iter (function
        | Var id as e ->
          if not (Variable.Set.mem id !variables)
          then (variables := Variable.Set.add id !variables;
            Queue.push e q)
        | Closure cl as e ->
          if not (Set_of_closures_id.Set.mem cl !closures)
          then (closures := Set_of_closures_id.Set.add cl !closures;
            Queue.push e q)
        | Global i as e ->
          if not (Int.Set.mem i !globals)
          then (globals := Int.Set.add i !globals;
            Queue.push e q))
        deps
    done

  let res =
    mark_loop ~toplevel:P.toplevel [] P.expr;
    propagate ();
    { id = !variables;
      closure = !closures; }

end

let inconstants ~for_clambda ~compilation_unit (expr : Flambda.t) =
  let module P = struct
    let expr = expr
    let for_clambda = for_clambda
    let compilation_unit = compilation_unit
    let toplevel = true
  end in
  let module A = NotConstants(P) in
  A.res

