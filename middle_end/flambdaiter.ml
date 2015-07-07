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

let apply_on_subexpressions f f_named (flam : _ Flambda.t) =
  match flam with
  | Fvar _ -> ()
  | Fapply ({func;_},_) ->
    f func
  | Fproject_var _ -> ()
  | Flet (_, _, defining_expr, body) ->
    f_named defining_expr;
    f body
  | Fletrec (defs, body,_) ->
    List.iter (fun (_,l) -> f_named l) defs;
    f body
  | Fseq_prim (_, args, _, _) ->
    List.iter f args
  | Fswitch (arg,sw,_) ->
    f arg;
    List.iter (fun (_,l) -> f l) sw.consts;
    List.iter (fun (_,l) -> f l) sw.blocks;
    Misc.may f sw.failaction
  | Fstringswitch (arg,sw,def,_) ->
    f arg;
    List.iter (fun (_,l) -> f l) sw;
    Misc.may f def
  | Fstaticraise (_,l,_) ->
    List.iter f l
  | Fstaticcatch (_,_,f1,f2,_) ->
    f f1; f f2;
  | Ftrywith (f1,_,f2,_) ->
    f f1; f f2
  | Fifthenelse (f1,f2,f3,_) ->
    f f1;f f2;f f3
  | Fsequence (f1,f2,_)
  | Fwhile (f1,f2,_) ->
    f f1; f f2
  | Ffor (_,f1,f2,_,f3,_) ->
    f f1;f f2;f f3
  | Fassign (_,f1,_) ->
    f f1
  | Funreachable _ -> ()


  | Fsymbol _
  | Fconst _
  | Funreachable _
  | Fproject_closure _
  | Fmove_within_set_of_closures _
  | Fprim _ -> ()






  | Fset_of_closures ({function_decls;free_vars = _; specialised_args = _},_) ->
    Variable.Map.iter (fun _ (ffun : _ Flambda.function_declaration) ->
        f ffun.body)
      function_decls.funs
  | Fsend (_,f1,f2,fl,_,_) ->
    List.iter f (f1::f2::fl)

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
    | Fseq_prim (_, args, _, _) ->
      iter_list args

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
    | Fprim _ | Fseq_prim _ | Fswitch _ | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fifthenelse _ | Fsequence _ | Fstringswitch _
    | Fwhile _ | Ffor _ | Fsend _ | Funreachable _
      -> ()
  in
  iter aux t

let map_general ~toplevel f f_named tree =
  let rec aux (tree : _ Flambda.t) =
    let exp : _ Flambda.t =
      match tree with
      | Fvar _ -> tree
      | Fapply ({ func; args; kind; dbg }, annot) ->
        Fapply ({ func = aux func; args; kind; dbg }, annot)
      | Fproject_var _ -> tree
      | Flet (str, id, lam, body, annot) ->
        let lam = aux_named lam in
        let body = aux body in
        Flet (str, id, lam, body, annot)
      | Fletrec (defs, body, annot) ->
        let defs = List.map (fun (id, lam) -> id, aux_named lam) defs in
        let body = aux body in
        Fletrec (defs, body, annot)
      | Fseq_prim (prim, args, dbg, annot) ->
        Fseq_prim (prim, List.map aux args, dbg, annot)
      | Fswitch(arg, sw, annot) ->
        let arg = aux arg in
        let sw =
          { sw with
            failaction = Misc.may_map aux sw.failaction;
            consts = List.map (fun (i,v) -> i, aux v) sw.consts;
            blocks = List.map (fun (i,v) -> i, aux v) sw.blocks;
          }
        in
        Fswitch (arg, sw, annot)
      | Fstringswitch(arg, sw, def, annot) ->
        let arg = aux arg in
        let sw = List.map (fun (i,v) -> i, aux v) sw in
        let def = Misc.may_map aux def in
        Fstringswitch(arg, sw, def, annot)
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
      | Funreachable _ -> tree
    in
    f exp

  and aux_named (named : _ Flambda.named) =
    let named : _ Flambda.named =
      match named with
      | Fsymbol _
      | Fconst _
      | Fproject_closure _
      | Fmove_within_set_of_closures _
      | Fprim _ -> named
      | Fset_of_closures ({ function_decls; free_vars;
            specialised_args }, annot) ->
        let function_decls : _ Flambda.function_declarations =
          if toplevel then function_decls
          else
            { function_decls with
              funs = Variable.Map.map
                  (fun (ffun : _ Flambda.function_declaration) ->
                    { ffun with body = aux ffun.body })
                function_decls.funs;
            }
        in
        Fset_of_closures ({ function_decls; free_vars; specialised_args },
          annot)
      | Fexpr expr -> aux expr
    in
    f_named named
  in
  aux tree

let map f tree = map_general ~toplevel:false f tree
let map_toplevel f tree = map_general ~toplevel:true f tree

(*
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
    | Fseq_prim(prim, args, dbg, v) ->
        Fseq_prim(prim, list_mapper args, dbg, f v)
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
*)
