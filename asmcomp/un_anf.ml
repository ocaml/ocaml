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

(* CR mshinwell: consider explicit ignore thing here *)
let make_ident_info (clam : Clambda.ulambda) : ident_info =
  let t : int Ident.Tbl.t = Ident.Tbl.create 10 in
  let assigned_idents = ref Ident.Set.empty in
  let rec loop : Clambda.ulambda -> unit = function
    | Uvar id ->
      begin match Ident.Tbl.find t id with
      | n -> Ident.Tbl.replace t id (n + 1)
      | exception Not_found -> Ident.Tbl.add t id 1
      end
    | Uconst _ ->
      (* CR mshinwell: improve comment *)
      (* Const cannot reference variables bound in this scope.
         Only const_closure can contains variables, but they are
         all bound by the closure *)
      ()
    | Udirect_apply (_, args, _) ->
      List.iter loop args
    | Ugeneric_apply (func, args, _) ->
      loop func;
      List.iter loop args
    | Uclosure (functions, captured_variables) ->
      List.iter loop captured_variables;
      List.iter (fun { Clambda.body } -> loop body) functions
    | Uoffset (expr, _) ->
      loop expr
    | Ulet (_, def, body) ->
      loop def;
      loop body
    | Uletrec (defs, body) ->
      List.iter (fun (_, def) -> loop def) defs;
      loop body
    | Uprim (_, args, _) ->
      List.iter loop args
    | Uswitch (cond, { us_actions_consts; us_actions_blocks }) ->
      loop cond;
      Array.iter loop us_actions_consts;
      Array.iter loop us_actions_blocks
    | Ustringswitch (cond, branches, default) ->
      loop cond;
      List.iter (fun (_,branch) -> loop branch) branches;
      Misc.may loop default
    | Ustaticfail (_, args) ->
      List.iter loop args
    | Ucatch (_, _, body, handler)
    | Utrywith (body, _, handler) ->
      loop body;
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
    | Ufor (_, low, high, _, body) ->
      loop low;
      loop high;
      loop body
    | Uassign (id, expr) ->
      assigned_idents := Ident.Set.add id !assigned_idents;
      loop expr
    | Usend (_, e1, e2, args, _) ->
      loop e1;
      loop e2;
      List.iter loop args
    | Uunreachable ->
      ()
  in
  loop clam;
  let linear =
    Ident.Tbl.fold (fun id n acc ->
        if n = 1 && not (Ident.Set.mem id !assigned_idents)
        then Ident.Set.add id acc
        else acc)
      t Ident.Set.empty
  in
  let used =
    (* This doesn't work transitively and thus is somewhat restricted.  In
       particular, it does not allow us to get rid of useless chains of [let]s.
       However it should be sufficient to remove the majority of unnecessary
       [let] bindings that might hinder [Cmmgen]. *)
    Ident.Tbl.fold (fun id _n acc -> Ident.Set.add id acc)
      t Ident.Set.empty
  in
  { used; linear; assigned = !assigned_idents }

(* For the avoidance of doubt, we say that an expression is "referentially
   transparent" iff it has no effect (cf. [Effect_analysis]) and also does
   not read from mutable values.  As such, in addition to the properties of
   expressions with no effects, modifying its surroundings does not modify
   its result.  In particular it can be run before or after adjacent
   expressions if data dependencies permit.

   [Uvar] expressions need special care: without knowledge of the kind of
   variable involved (specifically, whether or not it is mutable) we cannot
   tell whether such an expression is referentially transparent.
*)

type purity = Pure | Impure

let both_pure a b = match a, b with
  | Pure, Pure -> Pure
  | _ -> Impure

let primitive_purity (prim:Lambda.primitive) (args:Clambda.ulambda list) =
  match prim with
  | Psequand
  | Psequor
  | Pnot
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pandint
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Pintcomp _
  | Poffsetint _
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Pabsfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pfloatcomp _
  | Pstringlength
  | Pmakeblock _
  | Parraylength _
  | Pisint
  | Pisout
  | Pbittest
  | Pbintofint _
  | Pintofbint _
  | Pcvtbint _
  | Pnegbint _
  | Paddbint _
  | Psubbint _
  | Pmulbint _
  | Pandbint _
  | Porbint _
  | Pxorbint _
  | Plslbint _
  | Plsrbint _
  | Pasrbint _
  | Pbintcomp _
  | Pbigarraydim _
  | Pbswap16
  | Pbbswap _
  | Pint_as_pointer ->
    Pure

  | Pdivint | Pmodint
  | Pdivbint _ | Pmodbint _ -> begin
      match args with
      | [_; Uconst
           (Uconst_ref (_,
                        Some ( Uconst_int32 0l
                             | Uconst_int64 0L
                             | Uconst_nativeint 0n))
           | Uconst_int 0
           | Uconst_ptr 0)] ->
        (* raise Division_by_zero *)
        Impure
      | [_; Uconst
           (Uconst_ref (_,
                        Some ( Uconst_int32 _
                             | Uconst_int64 _
                             | Uconst_nativeint _))
           | Uconst_int _
           | Uconst_ptr _)] ->
        Pure
      | _ ->
        (* can raise Division_by_zero *)
        Impure
    end

  | Pidentity
  | Pignore
  | Ploc _
  | Pgetglobal _
  | Pgetglobalfield _
  | Pfield _
  | Pfloatfield _
  | Pccall _
  | Pstringrefu
  | Pmakearray _
  | Parrayrefu _
  | Pbigarrayref _
  | Pstring_load_16 _
  | Pstring_load_32 _
  | Pstring_load_64 _
  | Pbigstring_load_16 _
  | Pbigstring_load_32 _
  | Pbigstring_load_64 _
  | Pduprecord _
  | Pstringrefs
  | Parrayrefs _
  | Psetglobal _
  | Psetfield _
  | Psetfloatfield _
  | Praise _
  | Poffsetref _
  | Pstringsetu
  | Pstringsets
  | Parraysetu _
  | Parraysets _
  | Pbigarrayset _
  | Psetglobalfield _
  | Pstring_set_16 _
  | Pstring_set_32 _
  | Pstring_set_64 _
  | Pbigstring_set_16 _
  | Pbigstring_set_32 _
  | Pbigstring_set_64 _ ->
    Impure

  (* removed by previous passes *)
  | Pctconst _
  | Plazyforce
  | Prevapply _
  | Pdirapply _ -> assert false

(** Eliminate, through substitution, [let]-bindings of linear variables with
    referentially-transparent defining expressions. *)
let rec un_anf_and_purity ident_info env (clam : Clambda.ulambda)
      : Clambda.ulambda * purity =
  match clam with
  | Uvar id ->
    begin match Ident.Map.find id env with
    | e -> e, Pure
    (* It is necessarily pure since an expression must be pure to be in the environment *)
    | exception Not_found ->
      let purity =
        if Ident.Set.mem id ident_info.assigned then
          Impure
        else
          Pure
      in
      clam, purity
    end
  | Uconst _ ->
    (* Constant closures are rewritten separately. *)
    clam, Pure
  | Udirect_apply (label, args, dbg) ->
    let args = un_anf_list ident_info env args in
    Udirect_apply (label, args, dbg), Impure
  | Ugeneric_apply (func, args, dbg) ->
    let func = un_anf ident_info env func in
    let args = un_anf_list ident_info env args in
    Ugeneric_apply (func, args, dbg), Impure
  | Uclosure (functions, captured_variables) ->
    let functions =
      List.map (fun (ufunction:Clambda.ufunction) ->
          { ufunction with body = un_anf ident_info env ufunction.body })
        functions
    in
    let captured_variables, purity =
      un_anf_list_and_purity ident_info env captured_variables
    in
    Uclosure (functions, captured_variables), purity
  | Uoffset (clam, n) ->
    let clam, purity = un_anf_and_purity ident_info env clam in
    Uoffset (clam, n), purity
  | Ulet (id, def, Uvar id') when Ident.same id id' ->
    un_anf_and_purity ident_info env def
  | Ulet (id, def, body) ->
    let def, def_purity = un_anf_and_purity ident_info env def in
    begin match def_purity, Ident.Set.mem id ident_info.linear,
                Ident.Set.mem id ident_info.used with
    | Pure, _, false ->
      un_anf_and_purity ident_info env body
    | Pure, true, _ ->
      let env = Ident.Map.add id def env in
      un_anf_and_purity ident_info env body
    | _ ->
      let body, body_purity = un_anf_and_purity ident_info env body in
      Ulet (id, def, body), both_pure def_purity body_purity
    end
  | Uletrec (defs, body) ->
    let defs =
      List.map (fun (id, def) -> id, un_anf ident_info env def) defs
    in
    let body = un_anf ident_info env body in
    Uletrec (defs, body), Impure
  | Uprim (prim, args, dbg) ->
    let args, args_purity = un_anf_list_and_purity ident_info env args in
    let purity = both_pure args_purity (primitive_purity prim args) in
    Uprim (prim, args, dbg), purity
  | Uswitch (cond, sw) ->
    let cond = un_anf ident_info env cond in
    let sw =
      { sw with
        us_actions_consts = un_anf_array ident_info env sw.us_actions_consts;
        us_actions_blocks = un_anf_array ident_info env sw.us_actions_blocks;
      }
    in
    Uswitch (cond, sw), Impure
  | Ustringswitch (cond, branches, default) ->
    let cond = un_anf ident_info env cond in
    let branches =
      List.map
        (fun (s, branch) -> s, un_anf ident_info env branch)
        branches
    in
    let default = Misc.may_map (un_anf ident_info env) default in
    Ustringswitch (cond, branches, default), Impure
  | Ustaticfail (n, args) ->
    let args = un_anf_list ident_info env args in
    Ustaticfail (n, args), Impure
  | Ucatch (n, ids, body, handler) ->
    let body = un_anf ident_info env body in
    let handler = un_anf ident_info env handler in
    Ucatch (n, ids, body, handler), Impure
  | Utrywith (body, id, handler) ->
    let body = un_anf ident_info env body in
    let handler = un_anf ident_info env handler in
    Utrywith (body, id, handler), Impure
  | Uifthenelse (cond, ifso, ifnot) ->
    let cond, cond_purity = un_anf_and_purity ident_info env cond in
    let ifso, ifso_purity = un_anf_and_purity ident_info env ifso in
    let ifnot, ifnot_purity = un_anf_and_purity ident_info env ifnot in
    let purity =
      both_pure cond_purity
        (both_pure ifso_purity ifnot_purity)
    in
    Uifthenelse (cond, ifso, ifnot), purity
  | Usequence (e1, e2) ->
    let e1 = un_anf ident_info env e1 in
    let e2 = un_anf ident_info env e2 in
    Usequence (e1, e2), Impure
  | Uwhile (cond, body) ->
    let cond = un_anf ident_info env cond in
    let body = un_anf ident_info env body in
    Uwhile (cond, body), Impure
  | Ufor (id, low, high, direction, body) ->
    let low = un_anf ident_info env low in
    let high = un_anf ident_info env high in
    let body = un_anf ident_info env body in
    Ufor (id, low, high, direction, body), Impure
  | Uassign (id, expr) ->
    let expr = un_anf ident_info env expr in
    Uassign (id, expr), Impure
  | Usend (kind, e1, e2, args, dbg) ->
    let e1 = un_anf ident_info env e1 in
    let e2 = un_anf ident_info env e2 in
    let args = un_anf_list ident_info env args in
    Usend (kind, e1, e2, args, dbg), Impure
  | Uunreachable ->
    Uunreachable, Impure

and un_anf ident_info env clam : Clambda.ulambda =
  let clam, _purity = un_anf_and_purity ident_info env clam in
  clam

and un_anf_list_and_purity ident_info env clams : Clambda.ulambda list * purity =
  List.fold_right (fun clam (l, acc_purity) ->
      let clam, purity = un_anf_and_purity ident_info env clam in
      clam :: l, both_pure purity acc_purity)
    clams ([],Pure)

and un_anf_list ident_info env clams : Clambda.ulambda list =
  let clams, _purity = un_anf_list_and_purity ident_info env clams in
  clams

and un_anf_array ident_info env clams : Clambda.ulambda array =
  Array.map (un_anf ident_info env) clams

let apply clam =
  let ident_info = make_ident_info clam in
  let clam = un_anf ident_info Ident.Map.empty clam in
  if !Clflags.dump_clambda then
    Format.eprintf "@.un-anf:@ %a@."
      Printclambda.clambda clam;
  clam
