(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Detection of not constant values *)

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

open Ext_types
open Symbol
open Abstract_identifiers
open Flambda

type constant_result = {
  not_constant_id : VarSet.t;
  not_constant_closure : FunSet.t;
}

module type Param = sig
  type t
  val expr : t Flambda.flambda
  val for_clambda : bool
  val compilation_unit : compilation_unit
end

module NotConstants(P:Param) = struct


  let for_clambda = P.for_clambda
  let compilation_unit = P.compilation_unit

  type dep =
    | Closure of FunId.t
    | Var of Variable.t
    | Global of int (* position of the global *)

  (* Sets representing NC *)
  let variables = ref VarSet.empty
  let closures = ref FunSet.empty
  let globals = ref IntSet.empty

  (* if the table associates [v1;v2;...;vn] to v, it represents
     v in NC => v1 in NC /\ v2 in NC ... /\ vn in NC *)
  let id_dep_table : dep list VarTbl.t = VarTbl.create 100
  let fun_dep_table : dep list FunTbl.t = FunTbl.create 100
  let glob_dep_table : dep list IntTbl.t = IntTbl.create 100

  (* adds in the tables 'dep in NC => curr in NC' *)
  let register_implication ~in_nc:dep ~implies_in_nc:curr =
    List.iter (fun curr ->
      match dep with
      | Var id ->
        let t = try VarTbl.find id_dep_table id
        with Not_found -> [] in
        VarTbl.replace id_dep_table id (curr :: t)
      | Closure cl ->
        let t = try FunTbl.find fun_dep_table cl
        with Not_found -> [] in
        FunTbl.replace fun_dep_table cl (curr :: t)
      | Global i ->
        let t = try IntTbl.find glob_dep_table i
        with Not_found -> [] in
        IntTbl.replace glob_dep_table i (curr :: t))
      curr

  (* adds 'curr in NC' *)
  let mark_curr curr =
    List.iter (function
      | Var id ->
        if not (VarSet.mem id !variables)
        then variables := VarSet.add id !variables
      | Closure cl ->
        if not (FunSet.mem cl !closures)
        then closures := FunSet.add cl !closures
      | Global i ->
        if not (IntSet.mem i !globals)
        then globals := IntSet.add i !globals)
      curr

  (* First loop: iterates on the tree to mark dependencies.

     curr is the variables or closures to wich we add constraints like
     '... in NC => curr in NC' or 'curr in NC'

     It can be empty when no constraint can be added like in the toplevel
     expression or in the body of a function.
  *)
  let rec mark_loop (curr:dep list) = function

    | Flet(str, id, lam, body, _) ->
      if str = Assigned then mark_curr [Var id];
      mark_loop [Var id] lam;
      (* adds 'id in NC => curr in NC'
         This is not really necessary, but compiling this correctly is
         trickier than eliminating that earlier. *)
      register_implication ~in_nc:(Var id) ~implies_in_nc:curr;
      mark_loop curr body

    | Fletrec(defs, body, _) ->
      List.iter (fun (id,def) ->
          mark_loop [Var id] def;
          (* adds 'id in NC => curr in NC' same remark as let case *)
          register_implication ~in_nc:(Var id) ~implies_in_nc:curr) defs;
      mark_loop curr body

    | Fvar (id,_) ->
      (* adds 'id in NC => curr in NC' *)
      register_implication ~in_nc:(Var id) ~implies_in_nc:curr

    | Fclosure ({ cl_fun = funcs ; cl_free_var = fv; cl_specialised_arg },_) ->

      (* If a function in the closure is specialised, do not consider
         it constant *)
      VarMap.iter (fun _ id ->
            register_implication
              ~in_nc:(Var id)
              ~implies_in_nc:[Closure funcs.ident]) cl_specialised_arg;
      (* adds 'funcs in NC => curr in NC' *)
      register_implication ~in_nc:(Closure funcs.ident) ~implies_in_nc:curr;
      (* a closure is constant if its free variables are constants. *)
      VarMap.iter (fun inner_id lam ->
        mark_loop [Closure funcs.ident; Var inner_id] lam) fv;
      VarMap.iter (fun fun_id ffunc ->
        (* for each function f in a closure c 'c in NC => f' *)
        register_implication ~in_nc:(Closure funcs.ident) ~implies_in_nc:[Var fun_id];
        (* function parameters are in NC *)
        List.iter (fun id -> mark_curr [Var id]) ffunc.params;
        mark_loop [] ffunc.body) funcs.funs

    | Fconst _ -> ()

    (* a symbol does not necessarilly points to a constant: toplevel
       modules are declared as symbols, but can constain not constant
       values *)
    | Fsymbol(_sym,_) ->
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
    | Fprim(Lambda.Pgetglobal _id, [], _, _) ->
      if not for_clambda
      then mark_curr curr

    (* Constant constructors: those expressions are constant if all their parameters are:
       - makeblock is compiled to a constant block
       - offset is compiled to a pointer inside a constant closure.
         See Cmmgen for the details *)

    | Fprim(Lambda.Pmakeblock(_tag, Asttypes.Immutable), args, _dbg, _) ->
      List.iter (mark_loop curr) args

    | Ffunction ({fu_closure; fu_fun; _}, _) ->
      if Closure_function.in_compilation_unit compilation_unit fu_fun
      then mark_loop curr fu_closure
      else mark_curr curr

    | Fvariable_in_closure ({vc_closure = f1; _},_)
    | Fprim(Lambda.Pfield _, [f1], _, _) ->
      if for_clambda
      then mark_curr curr;
      mark_loop curr f1

    | Fprim(Lambda.Pgetglobalfield(id,i), [], _, _) ->
      (* adds 'global i in NC => curr in NC' *)
      if for_clambda
      then mark_curr curr
      else
        (* This is correct only if there is a rebind phase after !
           Clambdagen cannot handle this *)
        (* if some inlining produced some unreachable code like
           {[let a = 0 in
             if a
             then a.(0)
             else ... ]}
           then a.(0) cannot be compiled. There must be a specialisation
           phase after that eliminating the then branch and a dead code
           elimination eliminating potential reference to a.(0) *)
      if Ident.same id (Compilation_unit.get_persistent_ident compilation_unit)
      then register_implication ~in_nc:(Global i) ~implies_in_nc:curr
      else mark_curr curr

    | Fprim(Lambda.Psetglobalfield i, [f], _, _) ->
      mark_curr curr;
      (* adds 'f in NC => global i in NC' *)
      mark_loop [Global i] f

    (* Not constant cases: we mark directly 'curr in NC' and mark
       bound variables as in NC also *)

    | Fassign (id, f1, _) ->
      (* the assigned is also not constant *)
      mark_curr [Var id];
      mark_curr curr;
      mark_loop [] f1

    | Ftrywith (f1,id,f2,_) ->
      mark_curr [Var id];
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2

    | Fstaticcatch (_,ids,f1,f2,_) ->
      List.iter (fun id -> mark_curr [Var id]) ids;
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2

    | Ffor (id,f1,f2,_,f3,_) ->
      mark_curr [Var id];
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2;
      mark_loop [] f3

    | Fsequence (f1,f2,_)
    | Fwhile (f1,f2,_) ->
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2

    | Fifthenelse (f1,f2,f3,_) ->
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2;
      mark_loop [] f3

    | Fstaticraise (_,l,_)
    | Fprim (_,l,_,_) ->
      mark_curr curr;
      List.iter (mark_loop []) l

    | Fapply ({ap_function = f1; ap_arg = fl; _ },_) ->
      mark_curr curr;
      mark_loop [] f1;
      List.iter (mark_loop []) fl

    | Fswitch (arg,sw,_) ->
      mark_curr curr;
      mark_loop [] arg;
      List.iter (fun (_,l) -> mark_loop [] l) sw.fs_consts;
      List.iter (fun (_,l) -> mark_loop [] l) sw.fs_blocks;
      Misc.may (fun l -> mark_loop [] l) sw.fs_failaction

    | Fsend (_,f1,f2,fl,_,_) ->
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2;
      List.iter (mark_loop []) fl

    | Fevent (f1, _, _) ->
      mark_curr curr;
      mark_loop [] f1;

    | Funreachable _ ->
      mark_curr curr

  (* Second loop: propagates implications *)
  let propagate () =
    (* Set of variables/closures added to NC but not their dependencies *)
    let q = Queue.create () in
    VarSet.iter (fun v -> Queue.push (Var v) q) !variables;
    FunSet.iter (fun v -> Queue.push (Closure v) q) !closures;
    while not (Queue.is_empty q) do
      let deps = try match Queue.take q with
        | Var e -> VarTbl.find id_dep_table e
        | Closure cl -> FunTbl.find fun_dep_table cl
        | Global i -> IntTbl.find glob_dep_table i
      with Not_found -> [] in
      List.iter (function
        | Var id as e ->
          if not (VarSet.mem id !variables)
          then (variables := VarSet.add id !variables;
            Queue.push e q)
        | Closure cl as e ->
          if not (FunSet.mem cl !closures)
          then (closures := FunSet.add cl !closures;
            Queue.push e q)
        | Global i as e ->
          if not (IntSet.mem i !globals)
          then (globals := IntSet.add i !globals;
            Queue.push e q))
        deps
    done

  let res =
    mark_loop [] P.expr;
    propagate ();
    { not_constant_id = !variables;
      not_constant_closure = !closures; }

end

let not_constants (type a) ~for_clambda ~compilation_unit
    (expr:a Flambda.flambda) =
  let module P = struct
    type t = a
    let expr = expr
    let for_clambda = for_clambda
    let compilation_unit = compilation_unit
  end in
  let module A = NotConstants(P) in
  A.res

