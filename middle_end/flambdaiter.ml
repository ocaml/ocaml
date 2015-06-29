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

(* CR mshinwell: reduce duplication in this file *)

let apply_on_subexpressions f (flam : _ Flambda.t) =
  match flam with
  | Fsymbol _
  | Fvar _
  | Fconst _
  | Funreachable _
  | Fproject_closure _
  | Fmove_within_set_of_closures _
  | Fproject_var _
  | Fprim _ -> ()

  | Fassign (_,f1,_) ->
    f f1

  | Flet ( _, _, f1, f2,_)
  | Ftrywith (f1,_,f2,_)
  | Fsequence (f1,f2,_)
  | Fwhile (f1,f2,_)
  | Fstaticcatch (_,_,f1,f2,_) ->
    f f1; f f2;

  | Ffor (_,f1,f2,_,f3,_)
  | Fifthenelse (f1,f2,f3,_) ->
    f f1;f f2;f f3

  | Fstaticraise (_,l,_) ->
    List.iter f l

  | Fapply ({func;_},_) ->
    f func
  | Fset_of_closures ({function_decls;free_vars = _; specialised_args = _},_) ->
    Variable.Map.iter (fun _ (ffun : _ Flambda.function_declaration) ->
        f ffun.body)
      function_decls.funs
  | Fletrec (defs, body,_) ->
    List.iter (fun (_,l) -> f l) defs;
    f body
  | Fswitch (arg,sw,_) ->
    f arg;
    List.iter (fun (_,l) -> f l) sw.consts;
    List.iter (fun (_,l) -> f l) sw.blocks;
    Misc.may f sw.failaction
  | Fstringswitch (arg,sw,def,_) ->
    f arg;
    List.iter (fun (_,l) -> f l) sw;
    Misc.may f def
  | Fsend (_,f1,f2,fl,_,_) ->
    List.iter f (f1::f2::fl)

let subexpressions (flam : _ Flambda.t) =
  match flam with
  | Fsymbol _
  | Fvar _
  | Fconst _
  | Funreachable _
  | Fproject_closure _
  | Fmove_within_set_of_closures _
  | Fproject_var _
  | Fprim _ -> []

  | Fassign (_,f1,_) -> [f1]

  | Flet ( _, _, f1, f2,_)
  | Ftrywith (f1,_,f2,_)
  | Fsequence (f1,f2,_)
  | Fwhile (f1,f2,_)
  | Fstaticcatch (_,_,f1,f2,_) ->
      [f1; f2]

  | Ffor (_,f1,f2,_,f3,_)
  | Fifthenelse (f1,f2,f3,_) ->
      [f1; f2; f3]

  | Fstaticraise (_,l,_) -> l

  | Fapply ({func;_},_) ->
      [func]

  | Fset_of_closures ({function_decls;free_vars = _; specialised_args = _},_) ->
      Variable.Map.fold (fun _ (f : _ Flambda.function_declaration) l ->
          f.body :: l)
        function_decls.funs []

  | Fletrec (defs, body,_) ->
      body :: (List.map snd defs)

  | Fswitch (arg,sw,_) ->
      let l = List.fold_left (fun l (_,v) -> v :: l) [arg] sw.consts in
      let l = List.fold_left (fun l (_,v) -> v :: l) l sw.blocks in
      Misc.may_fold (fun f1 l -> f1 :: l) sw.failaction l

  | Fstringswitch (arg,sw,def,_) ->
      let l = List.fold_left (fun l (_,v) -> v :: l) [arg] sw in
      Misc.may_fold (fun f1 l -> f1 :: l) def l

  | Fsend (_,f1,f2,fl,_,_) ->
      (f1::f2::fl)

let iter_general ~toplevel f t =
  let rec aux (t : _ Flambda.t) =
    f t;
    match t with
    | Fsymbol _
    | Fvar _
    | Fconst _
    | Fproject_closure _
    | Fmove_within_set_of_closures _
    | Fproject_var _
    | Fprim _ -> ()

    | Fassign (_,f1,_) ->
      aux f1

    | Flet ( _, _, f1, f2,_)
    | Ftrywith (f1,_,f2,_)
    | Fsequence (f1,f2,_)
    | Fwhile (f1,f2,_)
    | Fstaticcatch (_,_,f1,f2,_) ->
      aux f1; aux f2;

    | Ffor (_,f1,f2,_,f3,_)
    | Fifthenelse (f1,f2,f3,_) ->
      aux f1;aux f2;aux f3

    | Fstaticraise (_,l,_) ->
      iter_list l

    | Fapply ({func = f1; _},_) ->
      aux f1

    | Fset_of_closures ({function_decls = funcs; free_vars = _; specialised_args = _},_) ->
      if not toplevel then begin
        Variable.Map.iter (fun _ (ffun : _ Flambda.function_declaration) ->
            aux ffun.body)
          funcs.funs
      end

    | Fletrec (defs, body,_) ->
      List.iter (fun (_,l) -> aux l) defs;
      aux body
    | Fswitch (arg,sw,_) ->
      aux arg;
      List.iter (fun (_,l) -> aux l) sw.consts;
      List.iter (fun (_,l) -> aux l) sw.blocks;
      Misc.may aux sw.failaction
    | Fstringswitch (arg,sw,def,_) ->
      aux arg;
      List.iter (fun (_,l) -> aux l) sw;
      Misc.may aux def

    | Fsend (_,f1,f2,fl,_,_) ->
      iter_list (f1::f2::fl)
    | Funreachable _ -> ()

  and iter_list l = List.iter aux l in
  aux t

let iter f t = iter_general ~toplevel:false f t
let iter_toplevel f t = iter_general ~toplevel:true f t

let iter_on_sets_of_closures f t =
  let aux (flam : _ Flambda.t) =
    match flam with
    | Fset_of_closures (clos,data) -> f clos data
    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Fproject_closure _
    | Fmove_within_set_of_closures _
    | Fproject_var _ | Flet _ | Fletrec _
    | Fprim _ | Fswitch _ | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fifthenelse _ | Fsequence _ | Fstringswitch _
    | Fwhile _ | Ffor _ | Fsend _ | Funreachable _
      -> ()
  in
  iter aux t

let map_general ~toplevel f tree =
  let rec aux (tree : _ Flambda.t) =
    let exp : _ Flambda.t =
      match tree with
      | Fsymbol _
      | Fvar _
      | Fconst _
      | Fproject_closure _
      | Fproject_var _
      | Fmove_within_set_of_closures _
      | Fprim _ -> tree
      | Fapply ({ func; args; kind; dbg }, annot) ->
          Fapply ({ func = aux func;
                    args;
                    kind; dbg }, annot)
      | Fset_of_closures ({ function_decls; free_vars;
                    specialised_args },annot) ->
          let function_decls : _ Flambda.function_declarations =
            if toplevel
            then function_decls
            else
              { function_decls with
                funs = Variable.Map.map
                    (fun (ffun : _ Flambda.function_declaration) ->
                      { ffun with body = aux ffun.body })
                    function_decls.funs } in
          Fset_of_closures ({ function_decls;
                      free_vars;
                      specialised_args }, annot)
      | Flet(str, id, lam, body, annot) ->
          let lam = aux lam in
          let body = aux body in
          Flet (str, id, lam, body, annot)
      | Fletrec(defs, body, annot) ->
          let defs = List.map (fun (id,lam) -> id,aux lam) defs in
          let body = aux body in
          Fletrec (defs, body, annot)
      | Fstaticraise(i, args, annot) ->
          let args = List.map aux args in
          Fstaticraise (i, args, annot)
      | Fstaticcatch (i, vars, body, handler, annot) ->
          let body = aux body in
          let handler = aux handler in
          Fstaticcatch (i, vars, body, handler, annot)
      | Ftrywith(body, id, handler, annot) ->
          let body = aux body in
          let handler = aux handler in
          Ftrywith(body, id, handler, annot)
      | Fifthenelse(arg, ifso, ifnot, annot) ->
          let arg = aux arg in
          let ifso = aux ifso in
          let ifnot = aux ifnot in
          Fifthenelse(arg, ifso, ifnot, annot)
      | Fsequence(lam1, lam2, annot) ->
          let lam1 = aux lam1 in
          let lam2 = aux lam2 in
          Fsequence(lam1, lam2, annot)
      | Fwhile(cond, body, annot) ->
          let cond = aux cond in
          let body = aux body in
          Fwhile(cond, body, annot)
      | Fsend(kind, met, obj, args, dbg, annot) ->
          let met = aux met in
          let obj = aux obj in
          let args = List.map aux args in
          Fsend(kind, met, obj, args, dbg, annot)
      | Ffor(id, lo, hi, dir, body, annot) ->
          let lo = aux lo in
          let hi = aux hi in
          let body = aux body in
          Ffor(id, lo, hi, dir, body, annot)
      | Fassign(id, lam, annot) ->
          let lam = aux lam in
          Fassign(id, lam, annot)
      | Fswitch(arg, sw, annot) ->
          let arg = aux arg in
          let sw =
            { sw with
              failaction = Misc.may_map aux sw.failaction;
              consts = List.map (fun (i,v) -> i, aux v) sw.consts;
              blocks = List.map (fun (i,v) -> i, aux v) sw.blocks; } in
          Fswitch(arg, sw, annot)
      | Fstringswitch(arg, sw, def, annot) ->
          let arg = aux arg in
          let sw = List.map (fun (i,v) -> i, aux v) sw in
          let def = Misc.may_map aux def in
          Fstringswitch(arg, sw, def, annot)

      | Funreachable _ -> tree
    in
    f exp
  in
  aux tree

let map f tree = map_general ~toplevel:false f tree
let map_toplevel f tree = map_general ~toplevel:true f tree

let fold_subexpressions (type acc) f (acc : acc) (flam : _ Flambda.t)
      : acc * _ Flambda.t =
  match flam with
  | Ftrywith(body,id,handler,d) ->
      let acc, body = f acc Variable.Set.empty body in
      let acc, handler = f acc (Variable.Set.singleton id) handler in
      acc, Ftrywith(body,id,handler,d)

  | Ffor(id, lo, hi, dir, body, d) ->
      let acc, lo = f acc Variable.Set.empty lo in
      let acc, hi = f acc Variable.Set.empty hi in
      let acc, body = f acc (Variable.Set.singleton id) body in
      acc, Ffor(id, lo, hi, dir, body, d)

  | Flet (kind, id, def, body, d) ->
      let acc, def = f acc Variable.Set.empty def in
      let acc, body = f acc (Variable.Set.singleton id) body in
      acc, Flet (kind, id, def, body, d)

  | Fletrec (defs, body, d) ->
      let vars = Variable.Set.of_list (List.map fst defs) in
      let acc, defs =
        List.fold_right (fun (var,def) (acc, defs) ->
            let acc, def = f acc vars def in
            acc, (var,def) :: defs)
          defs (acc,[]) in
      let acc, body = f acc vars body in
      acc, Fletrec (defs, body, d)

  | Fstaticcatch (exn,ids,body,handler,d) ->
      let acc, body = f acc Variable.Set.empty body in
      let acc, handler = f acc (Variable.Set.of_list ids) handler in
      acc, Fstaticcatch (exn,ids,body,handler,d)

  | Fstaticraise (exn, args, d) ->
      let acc, args =
        List.fold_right
          (fun arg (acc, l) ->
             let acc, arg = f acc Variable.Set.empty arg in
             acc, arg :: l) args (acc,[]) in
      acc, Fstaticraise (exn, args, d)

  | Fset_of_closures ({ function_decls; free_vars = _; specialised_args = _ } as closure, d) ->
      let acc, funs =
        Variable.Map.fold
          (fun v (fun_decl : _ Flambda.function_declaration) (acc, funs) ->
            let acc, body = f acc fun_decl.free_variables fun_decl.body in
            acc, Variable.Map.add v { fun_decl with body } funs)
          function_decls.funs (acc, Variable.Map.empty)
      in
      let function_decls = { function_decls with funs } in
      acc, Fset_of_closures({ closure with function_decls; }, d)

  | Fswitch (arg,
             { numconsts; consts; numblocks;
               blocks; failaction }, d) ->
      let acc, arg = f acc Variable.Set.empty arg in
      let aux (i,flam) (acc, l) =
        let acc, flam = f acc Variable.Set.empty flam in
        acc, (i,flam) :: l
      in
      let acc, consts = List.fold_right aux consts (acc,[]) in
      let acc, blocks = List.fold_right aux blocks (acc,[]) in
      let acc, failaction =
        match failaction with
        | None -> acc, None
        | Some flam ->
            let acc, flam = f acc Variable.Set.empty flam in
            acc, Some flam in
      acc,
      Fswitch (arg,
               { numconsts; consts; numblocks;
                 blocks; failaction }, d)

  | Fstringswitch (arg, sw, def, d) ->
      let acc, arg = f acc Variable.Set.empty arg in
      let aux (i,flam) (acc, l) =
        let acc, flam = f acc Variable.Set.empty flam in
        acc, (i,flam) :: l
      in
      let acc, sw = List.fold_right aux sw (acc,[]) in
      let acc, def =
        match def with
        | None -> acc, None
        | Some def ->
            let acc, def = f acc Variable.Set.empty def in
            acc, Some def in
      acc, Fstringswitch (arg, sw, def, d)

  | Fapply ({ func; args; kind; dbg }, d) ->
      let acc, func = f acc Variable.Set.empty func in
      acc, Fapply ({ func; args; kind; dbg }, d)

  | Fsend (kind, e1, e2, args, dbg, d) ->
      let acc, args =
        List.fold_right
          (fun arg (acc, l) ->
             let acc, arg = f acc Variable.Set.empty arg in
             acc, arg :: l) args (acc,[]) in
      let acc, e1 = f acc Variable.Set.empty e1 in
      let acc, e2 = f acc Variable.Set.empty e2 in
      acc, Fsend (kind, e1, e2, args, dbg, d)

  | Fsequence(e1, e2, d) ->
      let acc, e1 = f acc Variable.Set.empty e1 in
      let acc, e2 = f acc Variable.Set.empty e2 in
      acc, Fsequence(e1, e2, d)

  | Fwhile(cond, body, d) ->
      let acc, cond = f acc Variable.Set.empty cond in
      let acc, body = f acc Variable.Set.empty body in
      acc, Fwhile(cond, body, d)

  | Fifthenelse(cond,ifso,ifnot,d) ->
      let acc, cond = f acc Variable.Set.empty cond in
      let acc, ifso = f acc Variable.Set.empty ifso in
      let acc, ifnot = f acc Variable.Set.empty ifnot in
      acc, Fifthenelse(cond,ifso,ifnot,d)

  | Fassign (v,flam,d) ->
      let acc, flam = f acc Variable.Set.empty flam in
      acc, Fassign (v,flam,d)

  | ( Fsymbol _
    | Fvar _
    | Fconst _
    | Funreachable _
    | Fproject_closure _
    | Fproject_var _
    | Fmove_within_set_of_closures _
    | Fprim _ ) as e ->
      acc, e

let free_variables tree =
  let free = ref Variable.Set.empty in
  let bound = ref Variable.Set.empty in
  let add id =
    if not (Variable.Set.mem id !free) then free := Variable.Set.add id !free in
  let aux (flam : _ Flambda.t) =
    match flam with
    | Fvar (id,_) -> add id
    | Fassign (id,_,_) -> add id
    | Fproject_closure ({ set_of_closures; closure_id = _}, _) ->
      add set_of_closures
    | Fmove_within_set_of_closures
        ({ closure; start_from = _; move_to = _ }, _) ->
      add closure
    | Fproject_var ({ closure; closure_id = _; var = _ }, _) ->
      add closure
    | Fset_of_closures ({specialised_args},_) ->
        (* CR mshinwell for pchambart: add comment explaining why
           the [free_variables] inside [Fset_of_closures] isn't counted
           here *)
        Variable.Map.iter (fun _ id -> add id) specialised_args
    | Ftrywith(_,id,_,_)
    | Ffor(id, _, _, _, _, _)
    | Flet ( _, id, _, _,_) ->
        bound := Variable.Set.add id !bound
    | Fletrec (l, _,_) ->
        List.iter (fun (id,_) -> bound := Variable.Set.add id !bound) l
    | Fstaticcatch (_,ids,_,_,_) ->
        List.iter (fun id -> bound := Variable.Set.add id !bound) ids
    | _ -> ()
  in
  iter_toplevel aux tree;
  Variable.Set.diff !free !bound

let map_data (type t1) (type t2) (f:t1 -> t2)
      (tree:t1 Flambda.t) : t2 Flambda.t =
  let rec mapper : t1 Flambda.t -> t2 Flambda.t = function
    | Fsymbol (sym, v) -> Fsymbol (sym, f v)
    | Fvar (id, v) -> Fvar (id, f v)
    | Fconst (cst, v) -> Fconst (cst, f v)
    | Flet(str, id, lam, body, v) ->
        Flet(str, id, mapper lam, mapper body, f v)
    | Fletrec(defs, body, v) ->
        let defs = List.map (fun (id,def) -> (id, mapper def)) defs in
        Fletrec( defs, mapper body, f v)
    | Fapply ({ func; args; kind; dbg }, v) ->
        Fapply ({ func = mapper func;
                  args;
                  kind; dbg }, f v)
    | Fset_of_closures ({ function_decls; free_vars;
                  specialised_args }, v) ->
        let function_decls =
          { function_decls with
            funs = Variable.Map.map
                (fun (ffun : _ Flambda.function_declaration) ->
                  { ffun with body = mapper ffun.body })
                function_decls.funs } in
        Fset_of_closures ({ function_decls;
                    free_vars;
                    specialised_args }, f v)
    | Fproject_closure (project_closure, v) ->
      Fproject_closure (project_closure, f v)
    | Fmove_within_set_of_closures (move_within_set_of_closures, v) ->
      Fmove_within_set_of_closures (move_within_set_of_closures, f v)
    | Fproject_var (project_var, v) ->
      Fproject_var (project_var, f v)
    | Fswitch(arg, sw, v) ->
        let aux l = List.map (fun (i,v) -> i, mapper v) l in
        let sw = { sw with
                   consts = aux sw.consts;
                   blocks = aux sw.blocks;
                   failaction = Misc.may_map mapper sw.failaction } in
        Fswitch(mapper arg, sw, f v)
    | Fstringswitch (arg, sw, def, v) ->
        let sw = List.map (fun (i,v) -> i, mapper v) sw in
        Fstringswitch (mapper arg, sw, Misc.may_map mapper def, f v)
    | Fsend(kind, met, obj, args, dbg, v) ->
        Fsend(kind, mapper met, mapper obj, list_mapper args, dbg, f v)
    | Fprim(prim, args, dbg, v) ->
        Fprim(prim, args, dbg, f v)
    | Fstaticraise (i, args, v) ->
        Fstaticraise (i, list_mapper args, f v)
    | Fstaticcatch (i, vars, body, handler, v) ->
        Fstaticcatch (i, vars, mapper body, mapper handler, f v)
    | Ftrywith(body, id, handler, v) ->
        Ftrywith(mapper body, id, mapper handler, f v)
    | Fifthenelse(arg, ifso, ifnot, v) ->
        Fifthenelse(mapper arg, mapper ifso, mapper ifnot, f v)
    | Fsequence(lam1, lam2, v) ->
        Fsequence(mapper lam1, mapper lam2, f v)
    | Fwhile(cond, body, v) ->
        Fwhile(mapper cond, mapper body, f v)
    | Ffor(id, lo, hi, dir, body, v) ->
        Ffor(id, mapper lo, mapper hi, dir, mapper body, f v)
    | Fassign(id, lam, v) ->
        Fassign(id, mapper lam, f v)
    | Funreachable v -> Funreachable (f v)
  and list_mapper l = List.map mapper l in
  mapper tree
