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

let find_declaration cf ({ funs } : _ Flambda.function_declarations) =
  Variable.Map.find (Closure_id.unwrap cf) funs

let find_declaration_variable cf ({ funs } : _ Flambda.function_declarations) =
  let var = Closure_id.unwrap cf in
  if not (Variable.Map.mem var funs)
  then raise Not_found
  else var

let find_free_variable cv ({ free_vars } : _ Flambda.set_of_closures) =
  Variable.Map.find (Var_within_closure.unwrap cv) free_vars

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
  | Fselect_closure(_,data)
  | Fvar_within_closure(_,data)
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
  | Funreachable data -> data

let description_of_toplevel_node (expr : _ Flambda.t) =
  match expr with
  | Fsymbol (sym, _) -> Format.asprintf "%%%a" Symbol.print sym
  | Fvar (id, _) -> Format.asprintf "var %a" Variable.print id
  | Fconst _ -> "const"
  | Flet (_, id, _, _, _) -> Format.asprintf "let %a" Variable.print id
  | Fletrec _ -> "letrec"
  | Fset_of_closures _ -> "set_of_closures"
  | Fselect_closure _ -> "closure"
  | Fvar_within_closure _ -> "var_within_closure"
  | Fapply _ -> "apply"
  | Fswitch _ -> "switch"
  | Fstringswitch _ -> "stringswitch"
  | Fsend _ -> "send"
  | Fprim _ -> "prim"
  | Fstaticraise  _ -> "staticraise"
  | Fstaticcatch  _ -> "catch"
  | Ftrywith _ -> "trywith"
  | Fifthenelse _ -> "if"
  | Fsequence _ -> "seq"
  | Fwhile _ -> "while"
  | Ffor _ -> "for"
  | Fassign _ -> "assign"
  | Funreachable _ -> "unreachable"

let rec same (l1 : 'a Flambda.t) (l2 : 'a Flambda.t) =
  l1 == l2 || (* it is ok for string case: if they are physicaly the same,
                 it is the same original branch *)
  match (l1, l2) with
  | Fsymbol(s1, _), Fsymbol(s2, _) -> Symbol.equal s1 s2
  | Fsymbol _, _ | _, Fsymbol _ -> false
  | Fvar(v1, _), Fvar(v2, _) -> Variable.equal v1 v2
  | Fvar _, _ | _, Fvar _ -> false
  | Fconst(c1, _), Fconst(c2, _) -> begin
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
      a1.kind = a2.kind &&
      same a1.func a2.func &&
      Misc.samelist same a1.args a2.args
  | Fapply _, _ | _, Fapply _ -> false
  | Fset_of_closures (c1, _), Fset_of_closures (c2, _) ->
    same_set_of_closures c1 c2
  | Fset_of_closures _, _ | _, Fset_of_closures _ -> false
  | Fselect_closure (f1, _), Fselect_closure (f2, _) ->
    same_select_closure f1 f2
  | Fselect_closure _, _ | _, Fselect_closure _ -> false
  | Fvar_within_closure (v1, _), Fvar_within_closure (v2, _) ->
      same v1.closure v2.closure &&
      Closure_id.equal v1.closure_id v2.closure_id &&
      Var_within_closure.equal v1.var v2.var
  | Fvar_within_closure _, _ | _, Fvar_within_closure _ -> false
  | Flet (k1, v1, a1, b1, _), Flet (k2, v2, a2, b2, _) ->
      k1 = k2 && Variable.equal v1 v2 && same a1 a2 && same b1 b2
  | Flet _, _ | _, Flet _ -> false
  | Fletrec (bl1, a1, _), Fletrec (bl2, a2, _) ->
      Misc.samelist samebinding bl1 bl2 && same a1 a2
  | Fletrec _, _ | _, Fletrec _ -> false
  | Fprim (p1, al1, _, _), Fprim (p2, al2, _, _) ->
      p1 = p2 && Misc.samelist same al1 al2
  | Fprim _, _ | _, Fprim _ -> false
  | Fswitch (a1, s1, _), Fswitch (a2, s2, _) ->
      same a1 a2 && sameswitch s1 s2
  | Fswitch _, _ | _, Fswitch _ -> false
  | Fstringswitch (a1, s1, d1, _), Fstringswitch (a2, s2, d2, _) ->
      same a1 a2 &&
      Misc.samelist (fun (s1, e1) (s2, e2) -> s1 = s2 && same e1 e2) s1 s2 &&
      Misc.sameoption same d1 d2
  | Fstringswitch _, _ | _, Fstringswitch _ -> false
  | Fstaticraise (e1, a1, _), Fstaticraise (e2, a2, _) ->
      Static_exception.equal e1 e2 && Misc.samelist same a1 a2
  | Fstaticraise _, _ | _, Fstaticraise _ -> false
  | Fstaticcatch (s1, v1, a1, b1, _), Fstaticcatch (s2, v2, a2, b2, _) ->
      Static_exception.equal s1 s2 && Misc.samelist Variable.equal v1 v2 &&
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
      k1 = k2 && same a1 a2 && same b1 b2 && Misc.samelist same cl1 cl2
  | Fsend _, _ | _, Fsend _ -> false
  | Funreachable _, Funreachable _ -> true

and sameclosure (c1 : _ Flambda.function_declaration)
      (c2 : _ Flambda.function_declaration) =
  Misc.samelist Variable.equal c1.params c2.params &&
  same c1.body c2.body

and same_set_of_closures (c1 : _ Flambda.set_of_closures)
      (c2 : _ Flambda.set_of_closures) =
  Variable.Map.equal sameclosure c1.function_decls.funs c2.function_decls.funs &&
  Variable.Map.equal same c1.free_vars c2.free_vars &&
  Variable.Map.equal Variable.equal c1.specialised_args c2.specialised_args

and same_select_closure (s1 : _ Flambda.select_closure)
      (s2 : _ Flambda.select_closure) =
  Closure_id.equal s1.closure_id s2.closure_id
    && begin match s1.from, s2.from with
      | From_set_of_closures s1, From_set_of_closures s2 ->
        same_set_of_closures s1 s2
      | From_closure_or_another_unit s1, From_closure_or_another_unit s2 ->
        begin match s1, s2 with
        | From_closure_current_unit s1, From_closure_current_unit s2 ->
          begin match s1, s2 with
          | Not_relative v1, Not_relative v2 -> Variable.equal v1 v2
          | Relative (v1, r1), Relative (v2, r2) ->
            Variable.equal v1 v2 && Closure_id.equal r1 r2
          | Not_relative _, Relative _
          | Relative _, Not_relative _ -> false
          end
        | From_another_unit sym1, From_another_unit sym2 ->
          Symbol.equal sym1 sym2
        | From_closure_current_unit _, From_another_unit _
        | From_another_unit _, From_closure_current_unit _ -> false
        end
      | From_set_of_closures _, From_closure_or_another_unit _
      | From_closure_or_another_unit _, From_set_of_closures _ -> false
    end

and samebinding (v1, c1) (v2, c2) =
  Variable.equal v1 v2 && same c1 c2

and sameswitch (fs1 : _ Flambda.switch) (fs2 : _ Flambda.switch) =
  let samecase (n1, a1) (n2, a2) = n1 = n2 && same a1 a2 in
  fs1.numconsts = fs2.numconsts &&
  fs1.numblocks = fs2.numblocks &&
  Misc.samelist samecase fs1.consts fs2.consts &&
  Misc.samelist samecase fs1.blocks fs2.blocks &&
  Misc.sameoption same fs1.failaction fs2.failaction

let can_be_merged = same

(* Sharing key TODO
   Not implemented yet: this avoids sharing anything *)
(* CR mshinwell for pchambart: What is happening about this? *)

type sharing_key = unit
let make_key _ = None

let toplevel_substitution sb tree =
  let sb v = try Variable.Map.find v sb with Not_found -> v in
  let aux (flam : _ Flambda.t) : _ Flambda.t =
    match flam with
    | Fvar (id,e) -> Fvar (sb id,e)
    | Fassign (id,e,d) -> Fassign (sb id,e,d)
    | Fset_of_closures (cl,d) ->
      Fset_of_closures ({cl with
                 specialised_args =
                   Variable.Map.map sb cl.specialised_args},
                d)
    | e -> e
  in
  Flambdaiter.map_toplevel aux tree

let fold_over_exprs_for_variables_bound_by_closure ~fun_id ~clos_id ~clos
      ~init ~f =
  Variable.Set.fold (fun var acc ->
      let expr : _ Flambda.t =
        Fvar_within_closure
          ({ closure = clos_id;
             closure_id = fun_id;
             var = Var_within_closure.wrap var;
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
      (fun id sb -> Variable.Map.add id (Variable.freshen id) sb)
      free_variables Variable.Map.empty in
  let body = toplevel_substitution sb body in
  let subst id = Variable.Map.find id sb in
  let function_declaration : _ Flambda.function_declaration =
    { stub = false;
      params = List.map subst params;
      free_variables = Variable.Set.map subst free_variables;
      body;
      dbg = Debuginfo.none;
    }
  in
  let free_vars =
    Variable.Map.fold (fun id id' fv' ->
        Variable.Map.add id' (Flambda.Fvar(id,Expr_id.create ())) fv')
      (Variable.Map.filter (fun id _ -> not (Variable.Set.mem id param_set)) sb)
      Variable.Map.empty
  in
  let compilation_unit = Compilation_unit.get_current_exn () in
  Fselect_closure ({
      from = From_set_of_closures ({
          function_decls = {
            set_of_closures_id = Set_of_closures_id.create compilation_unit;
            funs = Variable.Map.singleton id function_declaration;
            compilation_unit;
          };
          free_vars;
          specialised_args = Variable.Map.empty;
        });
      closure_id = Closure_id.wrap id;
    },
    Expr_id.create ())
