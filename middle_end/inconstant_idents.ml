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
module Symbol_field = struct
  include Ext_types.Identifiable.Make(Ext_types.Pair(Symbol)(Int))
  include M
end

type result = {
  id : unit Variable.Tbl.t;
  closure : unit Set_of_closures_id.Tbl.t;
}

module type Param = sig
  val program : Flambda.program
  val for_clambda : bool
  val compilation_unit : Compilation_unit.t
end

module NotConstants(P:Param) = struct
  let for_clambda = P.for_clambda
  let compilation_unit = P.compilation_unit

  type dep =
    | Closure of Set_of_closures_id.t
    | Var of Variable.t
    | Symbol of Symbol.t
    | Symbol_field of Symbol_field.t

  (* Sets representing NC *)
  let variables : unit Variable.Tbl.t = Variable.Tbl.create 42
  let closures : unit Set_of_closures_id.Tbl.t =
    Set_of_closures_id.Tbl.create 42
  let symbols : unit Symbol.Tbl.t = Symbol.Tbl.create 42
  let symbol_fields : unit Symbol_field.Tbl.t = Symbol_field.Tbl.create 42

  (* if the table associates [v1;v2;...;vn] to v, it represents
     v in NC => v1 in NC /\ v2 in NC ... /\ vn in NC *)
  let id_dep_table : dep list Variable.Tbl.t = Variable.Tbl.create 100
  let fun_dep_table : dep list Set_of_closures_id.Tbl.t = Set_of_closures_id.Tbl.create 100
  let symbol_dep_table : dep list Symbol.Tbl.t = Symbol.Tbl.create 100
  let symbol_field_dep_table : dep list Symbol_field.Tbl.t =
    Symbol_field.Tbl.create 100

  (* adds 'dep in NC' *)
  let mark_dep =function
    | Var id ->
      if not (Variable.Tbl.mem variables id)
      then Variable.Tbl.add variables id ()
    | Closure cl ->
      if not (Set_of_closures_id.Tbl.mem closures cl)
      then Set_of_closures_id.Tbl.add closures cl ()
    | Symbol i ->
      if not (Symbol.Tbl.mem symbols i)
      then Symbol.Tbl.add symbols i ()
    | Symbol_field i ->
      if not (Symbol_field.Tbl.mem symbol_fields i)
      then Symbol_field.Tbl.add symbol_fields i ()

  (* adds 'curr in NC' *)
  let mark_curr curr =
    List.iter mark_dep curr

  (* adds in the tables 'dep in NC => curr in NC' *)
  let register_implication ~in_nc:dep ~implies_in_nc:curr =
    List.iter (fun curr_dep ->
      match dep with
      | Var id ->
        if Variable.Tbl.mem variables id then
          mark_dep curr_dep
        else begin
          let t = try Variable.Tbl.find id_dep_table id
          with Not_found -> [] in
          Variable.Tbl.replace id_dep_table id (curr_dep :: t)
        end
      | Closure cl ->
        if Set_of_closures_id.Tbl.mem closures cl then
          mark_dep curr_dep
        else begin
          let t = try Set_of_closures_id.Tbl.find fun_dep_table cl
          with Not_found -> [] in
          Set_of_closures_id.Tbl.replace fun_dep_table cl (curr_dep :: t)
        end
      | Symbol symbol ->
        if Symbol.Tbl.mem symbols symbol then
          mark_dep curr_dep
        else begin
          let t = try Symbol.Tbl.find symbol_dep_table symbol
          with Not_found -> [] in
          Symbol.Tbl.replace symbol_dep_table symbol (curr_dep :: t)
        end
      | Symbol_field field ->
        if Symbol_field.Tbl.mem symbol_fields field then
          mark_dep curr_dep
        else begin
          let t = try Symbol_field.Tbl.find symbol_field_dep_table field
            with Not_found -> [] in
          Symbol_field.Tbl.replace symbol_field_dep_table field (curr_dep :: t)
        end)
      curr

  (* First loop: iterates on the tree to mark dependencies.

     curr is the variables or closures to wich we add constraints like
     '... in NC => curr in NC' or 'curr in NC'

     It can be empty when no constraint can be added like in the toplevel
     expression or in the body of a function.
  *)
  let rec mark_loop ~toplevel (curr : dep list) (flam : Flambda.t) =
    match flam with
    | Let { var; defining_expr = lam; body; _ } ->
      mark_named ~toplevel [Var var] lam;
      (* adds 'var in NC => curr in NC'
         This is not really necessary, but compiling this correctly is
         trickier than eliminating that earlier. *)
      mark_var var curr;
      mark_loop ~toplevel curr body
    | Let_mutable (_mut_var, var, body) ->
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

    | Assign _ ->
      mark_curr curr

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
      mark_var from_value curr;
      mark_var to_value curr;
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
      List.iter (fun v -> mark_var v curr) l

    | Apply ({func; args; _ }) ->
      mark_curr curr;
      mark_var func curr;
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
    | Const _ | Allocated_const _ -> ()
    | Read_mutable _ -> mark_curr curr
    | Symbol symbol ->
      register_implication ~in_nc:(Symbol symbol) ~implies_in_nc:curr
    | Read_symbol_field (symbol, index) ->
      register_implication ~in_nc:(Symbol_field (symbol, index)) ~implies_in_nc:curr
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
    | Prim (Pmakearray (Pfloatarray, Immutable), args, _) ->
      mark_vars args curr
    | Prim (Pmakearray (Pfloatarray, Mutable), args, _) ->
      if for_clambda && toplevel then mark_vars args curr
      else mark_curr curr
    | Prim (Pduparray (Pfloatarray, Immutable), [arg], _) ->
      mark_var arg curr
    | Prim (Pduparray (Pfloatarray, Mutable), [arg], _) ->
      if for_clambda && toplevel then mark_var arg curr
      else mark_curr curr
    | Prim (Pduparray _, _, _) ->
      Misc.fatal_errorf
        "Unsupported case of Pduparray in Inconstant_idents: %a"
        Flambda.print_named named
    | Project_closure ({ set_of_closures; closure_id; }) ->
      if Closure_id.in_compilation_unit closure_id compilation_unit then
        mark_var set_of_closures curr
      else
        mark_curr curr
    | Move_within_set_of_closures
        ({ closure; start_from = _; move_to = _ }) ->
      mark_var closure curr
    | Project_var ({ closure; closure_id = _; var = _ }) ->
      mark_var closure curr
    | Prim(Lambda.Pfield _, [f1], _) ->
      mark_curr curr;
      mark_var f1 curr
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

  (* [toplevel] is intended for allowing static allocations of mutable
     blocks.  This feature should be available in a future release once the
     necessary GC changes have been merged. (See GPR#178.) *)
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

  let mark_constant_defining_value (const:Flambda.constant_defining_value) =
    match const with
    | Allocated_const _
    | Block _
    | Project_closure _ -> ()
    | Set_of_closures set_of_closure ->
      mark_loop_set_of_closures ~toplevel:true [] set_of_closure

  let mark_program (program : Flambda.program) =
    let rec loop (program : Flambda.program_body) =
      match program with
      | End _ -> ()
      | Initialize_symbol (symbol,_tag,fields,program) ->
        List.iteri (fun i field ->
            mark_loop ~toplevel:true
              [Symbol symbol; Symbol_field (symbol,i)] field)
          fields;
        loop program
      | Effect (expr, program) ->
        mark_loop ~toplevel:true [] expr;
        loop program
      | Let_symbol (_, def, program) ->
        mark_constant_defining_value def;
        loop program
      | Let_rec_symbol (defs, program) ->
        List.iter (fun (_, def) -> mark_constant_defining_value def) defs;
        loop program
    in
    loop program.program_body

  (* There is no information available about the contents of imported
     symbols, so we must consider all their fields as inconstant. *)
  (* CR pchambart: recover that from the cmx informations *)
  let mark_external_symbol_fields_as_inconstant imported_symbols =
    Symbol_field.Tbl.iter (fun (symbol, field) _ ->
        if Symbol.Set.mem symbol imported_symbols then
          mark_curr [Symbol_field (symbol, field)])
      symbol_field_dep_table

  (* Second loop: propagates implications *)
  let propagate () =
    (* Set of variables/closures added to NC but not their dependencies *)
    let q = Queue.create () in
    Variable.Tbl.iter (fun v () -> Queue.push (Var v) q) variables;
    Set_of_closures_id.Tbl.iter
      (fun v () -> Queue.push (Closure v) q) closures;
    Symbol.Tbl.iter (fun v () -> Queue.push (Symbol v) q) symbols;
    Symbol_field.Tbl.iter
      (fun v () -> Queue.push (Symbol_field v) q) symbol_fields;
    while not (Queue.is_empty q) do
      let deps = try match Queue.take q with
        | Var e -> Variable.Tbl.find id_dep_table e
        | Closure cl -> Set_of_closures_id.Tbl.find fun_dep_table cl
        | Symbol s -> Symbol.Tbl.find symbol_dep_table s
        | Symbol_field s -> Symbol_field.Tbl.find symbol_field_dep_table s
      with Not_found -> [] in
      List.iter (function
        | Var id as e ->
          if not (Variable.Tbl.mem variables id)
          then (Variable.Tbl.add variables id ();
            Queue.push e q)
        | Closure cl as e ->
          if not (Set_of_closures_id.Tbl.mem closures cl)
          then (Set_of_closures_id.Tbl.add closures cl ();
            Queue.push e q)
        | Symbol s as e ->
          if not (Symbol.Tbl.mem symbols s)
          then (Symbol.Tbl.add symbols s ();
                Queue.push e q)
        | Symbol_field s as e ->
          if not (Symbol_field.Tbl.mem symbol_fields s)
          then (Symbol_field.Tbl.add symbol_fields s ();
            Queue.push e q))
        deps
    done

  let res =
    mark_program P.program;
    mark_external_symbol_fields_as_inconstant
      (Flambda_utils.imported_symbols P.program);
    propagate ();
    stop_timing "Propagate";
    { id = variables;
      closure = closures; }

end

(* let inconstants ~for_clambda ~compilation_unit (expr : Flambda.t) = *)
(*   let module P = struct *)
(*     let expr = expr *)
(*     let for_clambda = for_clambda *)
(*     let compilation_unit = compilation_unit *)
(*     let toplevel = true *)
(*   end in *)
(*   let module A = NotConstants(P) in *)
(*   Format.eprintf "inconstants returns %a\n%a@ " *)
(*     Variable.Set.print A.res.id *)
(*     Set_of_closures_id.Set.print A.res.closure; *)
(*   A.res *)

let inconstants_on_program ~for_clambda ~compilation_unit (program : Flambda.program) =
  let module P = struct
    let program = program
    let for_clambda = for_clambda
    let compilation_unit = compilation_unit
  end in
  let module A = NotConstants(P) in
  (* Format.eprintf "inconstants returns %a\n%a@ " *)
  (*   Variable.Set.print A.res.id *)
  (*   Set_of_closures_id.Set.print A.res.closure; *)
  A.res

let variable var { id } =
  Variable.Tbl.mem id var

let closure cl { closure } =
  Set_of_closures_id.Tbl.mem closure cl
