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

(* We say that an [Ident.t] is "linear" iff:
   (a) it is used exactly once;
   (b) it is never assigned to (using [Uassign]).
*)
type ident_info =
  { used : Ident.Set.t;
    linear : Ident.Set.t;
    assigned : Ident.Set.t;
  }

let ignore_uconstant (_ : Clambda.uconstant) = ()
let ignore_function_label (_ : Clambda.function_label) = ()
let ignore_debuginfo (_ : Debuginfo.t) = ()
let ignore_int (_ : int) = ()
let ignore_ident (_ : Ident.t) = ()
let ignore_primitive (_ : Lambda.primitive) = ()
let ignore_string (_ : string) = ()
let ignore_int_array (_ : int array) = ()
let ignore_ident_list (_ : Ident.t list) = ()
let ignore_direction_flag (_ : Asttypes.direction_flag) = ()
let ignore_meth_kind (_ : Lambda.meth_kind) = ()

let make_ident_info (clam : Clambda.ulambda) : ident_info =
  let t : int Ident.Tbl.t = Ident.Tbl.create 42 in
  let assigned_idents = ref Ident.Set.empty in
  let rec loop : Clambda.ulambda -> unit = function
    (* No underscores in the pattern match, to reduce the chance of failing
       to traverse some subexpression. *)
    | Uvar id ->
      begin match Ident.Tbl.find t id with
      | n -> Ident.Tbl.replace t id (n + 1)
      | exception Not_found -> Ident.Tbl.add t id 1
      end
    | Uconst const ->
      (* The only variables that might occur in [const] are those in constant
         closures---and those are all bound by such closures.  It follows that
         [const] cannot contain any variables that are bound in the current
         scope, so we do not need to count them here.  (The function bodies
         of the closures will be traversed when this function is called from
         [Cmmgen.transl_function].) *)
      ignore_uconstant const
    | Udirect_apply (label, args, dbg) ->
      ignore_function_label label;
      List.iter loop args;
      ignore_debuginfo dbg
    | Ugeneric_apply (func, args, dbg) ->
      loop func;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uclosure (functions, captured_variables) ->
      List.iter loop captured_variables;
      List.iter (fun { Clambda. label; arity; params; body; dbg } ->
          ignore_function_label label;
          ignore_int arity;
          ignore_ident_list params;
          loop body;
          ignore_debuginfo dbg)
        functions
    | Uoffset (expr, offset) ->
      loop expr;
      ignore_int offset
    | Ulet (ident, def, body) ->
      ignore_ident ident;
      loop def;
      loop body
    | Uletrec (defs, body) ->
      List.iter (fun (ident, def) ->
          ignore_ident ident;
          loop def)
        defs;
      loop body
    | Uprim (prim, args, dbg) ->
      ignore_primitive prim;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uswitch (cond, { us_index_consts; us_actions_consts;
          us_index_blocks; us_actions_blocks }) ->
      loop cond;
      ignore_int_array us_index_consts;
      Array.iter loop us_actions_consts;
      ignore_int_array us_index_blocks;
      Array.iter loop us_actions_blocks
    | Ustringswitch (cond, branches, default) ->
      loop cond;
      List.iter (fun (str, branch) ->
          ignore_string str;
          loop branch)
        branches;
      Misc.may loop default
    | Ustaticfail (static_exn, args) ->
      ignore_int static_exn;
      List.iter loop args
    | Ucatch (static_exn, idents, body, handler) ->
      ignore_int static_exn;
      ignore_ident_list idents;
      loop body;
      loop handler
    | Utrywith (body, ident, handler) ->
      loop body;
      ignore_ident ident;
      loop handler
    | Uifthenelse (cond, ifso, ifnot) ->
      loop cond;
      loop ifso;
      loop ifnot
    | Usequence (e1, e2) ->
      loop e1;
      loop e2
    | Uwhile (cond, body) ->
      loop cond;
      loop body
    | Ufor (ident, low, high, direction_flag, body) ->
      ignore_ident ident;
      loop low;
      loop high;
      ignore_direction_flag direction_flag;
      loop body
    | Uassign (ident, expr) ->
      assigned_idents := Ident.Set.add ident !assigned_idents;
      loop expr
    | Usend (meth_kind, e1, e2, args, dbg) ->
      ignore_meth_kind meth_kind;
      loop e1;
      loop e2;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uunreachable ->
      ()
  in
  loop clam;
  let linear =
    Ident.Tbl.fold (fun id n acc ->
        assert (n >= 1);
        if n = 1 && not (Ident.Set.mem id !assigned_idents)
        then Ident.Set.add id acc
        else acc)
      t Ident.Set.empty
  in
  let assigned = !assigned_idents in
  let used =
    (* This doesn't work transitively and thus is somewhat restricted.  In
       particular, it does not allow us to get rid of useless chains of [let]s.
       However it should be sufficient to remove the majority of unnecessary
       [let] bindings that might hinder [Cmmgen]. *)
    Ident.Tbl.fold (fun id _n acc -> Ident.Set.add id acc)
      t assigned
  in
  { used; linear; assigned; }

(* We say that an expression is "moveable" iff it has neither effects nor
   coeffects.  (See semantics_of_primitives.mli.) *)

type moveable = Fixed | Moveable

let both_moveable a b =
  match a, b with
  | Moveable, Moveable -> Moveable
  | Moveable, Fixed
  | Fixed, Moveable
  | Fixed, Fixed -> Fixed

let primitive_moveable (prim : Lambda.primitive)
      (args : Clambda.ulambda list) =
  let second_arg_is_definitely_not_zero =
    match args with
    | _::(Uconst
         (Uconst_ref (_,
                      Some ( Uconst_int32 0l
                           | Uconst_int64 0L
                           | Uconst_nativeint 0n))
         | Uconst_int 0
         | Uconst_ptr 0))::_ ->
      false
    | _::(Uconst
        (Uconst_ref (_,
                      Some ( Uconst_int32 _
                           | Uconst_int64 _
                           | Uconst_nativeint _))
         | Uconst_int _
         | Uconst_ptr _))::_ ->
      true
    | _ ->
      false
  in
  match
    Semantics_of_primitives.for_primitive prim
      ~second_arg_is_definitely_not_zero
  with
  | No_effects, No_coeffects -> Moveable
  | No_effects, Has_coeffects
  | Has_effects, No_coeffects
  | Has_effects, Has_coeffects -> Fixed

(** Eliminate, through substitution, [let]-bindings of linear variables with
    moveable defining expressions. *)
let rec un_anf_and_moveable ident_info env (clam : Clambda.ulambda)
      : Clambda.ulambda * moveable =
  match clam with
  | Uvar id ->
    begin match Ident.Map.find id env with
    (* [clam] is necessarily pure since an expression must be pure to be in the
       environment *)
    | e -> e, Moveable
    | exception Not_found ->
      let moveable =
        if Ident.Set.mem id ident_info.assigned then
          Fixed
        else
          Moveable
      in
      clam, moveable
    end
  | Uconst _ ->
    (* Constant closures are rewritten separately. *)
    clam, Moveable
  | Udirect_apply (label, args, dbg) ->
    let args = un_anf_list ident_info env args in
    Udirect_apply (label, args, dbg), Fixed
  | Ugeneric_apply (func, args, dbg) ->
    let func = un_anf ident_info env func in
    let args = un_anf_list ident_info env args in
    Ugeneric_apply (func, args, dbg), Fixed
  | Uclosure (functions, variables_bound_by_the_closure) ->
    let functions =
      List.map (fun (ufunction : Clambda.ufunction) ->
          { ufunction with body = un_anf ident_info env ufunction.body })
        functions
    in
    let variables_bound_by_the_closure, moveable =
      un_anf_list_and_moveable ident_info env variables_bound_by_the_closure
    in
    Uclosure (functions, variables_bound_by_the_closure), moveable
  | Uoffset (clam, n) ->
    let clam, moveable = un_anf_and_moveable ident_info env clam in
    Uoffset (clam, n), moveable
  | Ulet (id, def, Uvar id') when Ident.same id id' ->
    un_anf_and_moveable ident_info env def
  | Ulet (id, def, body) ->
    let def, def_moveable = un_anf_and_moveable ident_info env def in
    let is_linear = Ident.Set.mem id ident_info.linear in
    let is_used = Ident.Set.mem id ident_info.used in
    begin match def_moveable, is_linear, is_used with
    | Moveable, _, false ->
      (* A moveable expression that is never used may be eliminated. *)
      un_anf_and_moveable ident_info env body
    | Moveable, true, true ->
      (* A moveable expression bound to a linear [Ident.t] may replace the
         single occurrence of the identifier. *)
      let env = Ident.Map.add id def env in
      un_anf_and_moveable ident_info env body
    | Moveable, false, true  (* Moveable but not used linearly. *)
    | Fixed, _, _ ->
      let body, body_moveable = un_anf_and_moveable ident_info env body in
      Ulet (id, def, body), both_moveable def_moveable body_moveable
    end
  | Uletrec (defs, body) ->
    let defs =
      List.map (fun (id, def) -> id, un_anf ident_info env def) defs
    in
    let body = un_anf ident_info env body in
    Uletrec (defs, body), Fixed
  | Uprim (prim, args, dbg) ->
    let args, args_moveable = un_anf_list_and_moveable ident_info env args in
    let moveable =
      both_moveable args_moveable (primitive_moveable prim args)
    in
    Uprim (prim, args, dbg), moveable
  | Uswitch (cond, sw) ->
    let cond = un_anf ident_info env cond in
    let sw =
      { sw with
        us_actions_consts = un_anf_array ident_info env sw.us_actions_consts;
        us_actions_blocks = un_anf_array ident_info env sw.us_actions_blocks;
      }
    in
    Uswitch (cond, sw), Fixed
  | Ustringswitch (cond, branches, default) ->
    let cond = un_anf ident_info env cond in
    let branches =
      List.map (fun (s, branch) -> s, un_anf ident_info env branch)
        branches
    in
    let default = Misc.may_map (un_anf ident_info env) default in
    Ustringswitch (cond, branches, default), Fixed
  | Ustaticfail (n, args) ->
    let args = un_anf_list ident_info env args in
    Ustaticfail (n, args), Fixed
  | Ucatch (n, ids, body, handler) ->
    let body = un_anf ident_info env body in
    let handler = un_anf ident_info env handler in
    Ucatch (n, ids, body, handler), Fixed
  | Utrywith (body, id, handler) ->
    let body = un_anf ident_info env body in
    let handler = un_anf ident_info env handler in
    Utrywith (body, id, handler), Fixed
  | Uifthenelse (cond, ifso, ifnot) ->
    let cond, cond_moveable = un_anf_and_moveable ident_info env cond in
    let ifso, ifso_moveable = un_anf_and_moveable ident_info env ifso in
    let ifnot, ifnot_moveable = un_anf_and_moveable ident_info env ifnot in
    let moveable =
      both_moveable cond_moveable
        (both_moveable ifso_moveable ifnot_moveable)
    in
    Uifthenelse (cond, ifso, ifnot), moveable
  | Usequence (e1, e2) ->
    let e1 = un_anf ident_info env e1 in
    let e2 = un_anf ident_info env e2 in
    Usequence (e1, e2), Fixed
  | Uwhile (cond, body) ->
    let cond = un_anf ident_info env cond in
    let body = un_anf ident_info env body in
    Uwhile (cond, body), Fixed
  | Ufor (id, low, high, direction, body) ->
    let low = un_anf ident_info env low in
    let high = un_anf ident_info env high in
    let body = un_anf ident_info env body in
    Ufor (id, low, high, direction, body), Fixed
  | Uassign (id, expr) ->
    let expr = un_anf ident_info env expr in
    Uassign (id, expr), Fixed
  | Usend (kind, e1, e2, args, dbg) ->
    let e1 = un_anf ident_info env e1 in
    let e2 = un_anf ident_info env e2 in
    let args = un_anf_list ident_info env args in
    Usend (kind, e1, e2, args, dbg), Fixed
  | Uunreachable ->
    Uunreachable, Fixed

and un_anf ident_info env clam : Clambda.ulambda =
  let clam, _moveable = un_anf_and_moveable ident_info env clam in
  clam

and un_anf_list_and_moveable ident_info env clams
      : Clambda.ulambda list * moveable =
  List.fold_right (fun clam (l, acc_moveable) ->
      let clam, moveable = un_anf_and_moveable ident_info env clam in
      clam :: l, both_moveable moveable acc_moveable)
    clams ([], Moveable)

and un_anf_list ident_info env clams : Clambda.ulambda list =
  let clams, _moveable = un_anf_list_and_moveable ident_info env clams in
  clams

and un_anf_array ident_info env clams : Clambda.ulambda array =
  Array.map (un_anf ident_info env) clams

let apply clam =
  let ident_info = make_ident_info clam in
  let clam = un_anf ident_info Ident.Map.empty clam in
  if !Clflags.dump_clambda then begin
    Format.eprintf "@.un-anf:@ %a@." Printclambda.clambda clam
  end;
  clam
