(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Operations on core types *)

open Misc
open Asttypes
open Typedtree

exception Unify

let current_level = ref 0
let global_level = ref 1
let generic_level = (-1)

let begin_def () = incr current_level
let end_def () = decr current_level

let reset_global_level () =
  global_level := !current_level + 1

let newty desc         = { desc = desc; level = !current_level }
let new_global_ty desc = { desc = desc; level = !global_level }
let newgenty desc      = { desc = desc; level = generic_level }
let newvar ()          = { desc = Tvar; level = !current_level }
let new_global_var ()  = new_global_ty Tvar
let new_gen_var ()     = newgenty Tvar
let newobj fields      = newty (Tobject (fields, ref None))

let rec repr = function
    {desc = Tlink t'} as t ->
      let r = repr t' in
      if r != t' then t.desc <- Tlink r;
      r
  | t -> t

let rec repr2 = function                (* No path compression *)
                                        (* during unification *)
    {desc = Tlink ty} ->
      repr2 ty
  | t -> t

let none = newty (Ttuple [])                (* Clearly ill-formed type *)

(* --- *)

let flatten_fields ty =
  let rec flatten l ty =
    let ty = repr ty in
    match ty.desc with
      Tfield(s, ty1, ty2) ->
        flatten ((s, ty1)::l) ty2
    | Tvar | Tnil ->
        (l, ty)
    | _ ->
      fatal_error "Ctype.flatten_fields"
  in
    let (l, r) = flatten [] ty in
      (List.rev l, r)

let build_fields =
  List.fold_right
    (fun (s, ty1) ty2 ->
       {desc = Tfield(s, ty1, ty2);
        level = ty2.level})

let associate_fields fields1 fields2 =
  let rec associate p s s' =
    function
      (l, []) ->
        (List.rev p, (List.rev s) @ l, List.rev s')
    | ([], l') ->
        (List.rev p, List.rev s, (List.rev s') @ l')
    | (((n, t)::r as l), ((n', t')::r' as l')) ->
        if Label. (=) n n' then
          associate ((t, t')::p) s s' (r, r')
        else if Label. (<) n  n' then
          associate p ((n, t)::s) s' (r, l')
        else
          associate p s ((n', t')::s') (l, r')
  in let sort = Sort.list (fun (n, _) (n', _) -> Label. (<) n n') in
  associate [] [] [] (sort fields1, sort fields2)

(* Check whether an object is open *)

let rec opened ty =
  match (repr ty).desc with
    Tfield(_, _, t) -> opened t
  | Tvar            -> true
  | Tnil            -> false
  | _               -> fatal_error "Ctype.opened"

let opened_object ty =
  match (repr ty).desc with
    Tobject (ty', _)  -> opened ty'
  | Tconstr (_, _, _) -> false
  | _                 -> fatal_error "Ctype.opened_object"

(* Type generalization *)

let rec generalize ty =
  let ty = repr ty in
  if ty.level > !current_level then begin
    ty.level <- generic_level;
    match ty.desc with
      Tvar               -> ()
    | Tarrow(t1, t2)     -> generalize t1; generalize t2
    | Ttuple tl          -> List.iter generalize tl
    | Tconstr(_, tl, ab) -> ab := []; List.iter generalize tl
    | Tobject(f, {contents = Some (_, p)})
                         -> generalize f; List.iter generalize p
    | Tobject(f, _)      -> generalize f
    | Tfield(_, t1, t2)  -> generalize t1; generalize t2
    | Tnil               -> ()
    | Tlink _            -> fatal_error "Ctype.generalize"
  end

let rec make_nongen ty =
  let ty = repr ty in
  if ty.level > !current_level then begin
    ty.level <- !current_level;
    match ty.desc with
      Tvar              -> ()
    | Tarrow(t1, t2)    ->  make_nongen t1; make_nongen t2
    | Ttuple tl         -> List.iter make_nongen tl
    | Tconstr(p, tl, _) -> List.iter make_nongen tl
    | Tobject (f, _)    -> make_nongen f
    | Tfield(_, t1, t2) -> make_nongen t1; make_nongen t2
    | Tnil              -> ()
    | Tlink _           -> fatal_error "Ctype.make_nongen"
  end

(* Remove abbreviations from generalized types *)

let visited = ref ([] : type_expr list)

let remove_abbrev ty =
  let rec remove ty =
    let ty = repr ty in
    if ty.level = generic_level & not (List.memq ty !visited) then begin
      visited := ty :: !visited;
      match ty.desc with
        Tvar               -> ()
      | Tarrow(t1, t2)     -> remove t1; remove t2
      | Ttuple tl          -> List.iter remove tl
      | Tconstr(_, tl, ab) -> ab := []; List.iter remove tl
      | Tobject(f, {contents = Some (_, p)})
                           -> remove f; List.iter remove p
      | Tobject(f, _)      -> remove f
      | Tfield(_, t1, t2)  -> remove t1; remove t2
      | Tnil               -> ()
      | Tlink _            -> fatal_error "Ctype.remove_abbrev"
      end
  in
    visited := []; remove ty; visited := []


(* Taking instances of type schemes *)

type 'a visited = Zero | One | Many of 'a

let inst_subst = ref ([] : (type_expr * type_expr) list)

let rec copy_rec abbrev visited ty =
  let ty = repr ty in
  if ty.level <> generic_level then ty else
  try
    match List.assq ty visited with
      {contents = Zero} as v ->
      	let t = newvar () in
	v := Many t;
	let ty' = copy_rec_2 abbrev visited ty v in
	t.desc <- ty'.desc;
	t
    | {contents = One} as v ->
        let t = newvar () in
        v := Many t;
        t
    | {contents = Many t} ->
        t
  with Not_found ->
    let v = ref One in
    let ty' = copy_rec_2 abbrev ((ty, v)::visited) ty v in
    match v with
      {contents = Many t} ->
      	t.desc <- ty'.desc;
	t
    | _ ->
      	ty'

and copy_rec_2 abbrev visited ty v =
  match ty.desc with
    Tvar ->
      begin try List.assq ty !inst_subst with Not_found ->
        let ty' = newvar () in
        inst_subst := (ty, ty') :: !inst_subst;
        ty'
      end
  | Tarrow (t1, t2) ->
      newty (Tarrow (copy_rec abbrev visited t1,
                     copy_rec abbrev visited t2))
  | Ttuple tl ->
      newty (Ttuple (List.map (copy_rec abbrev visited) tl))
  | Tconstr (p, [], _) ->
      newty (Tconstr (p, [], ref abbrev))
  | Tconstr (p, tl, _) ->
      newty (Tconstr (p, List.map (copy_rec abbrev visited) tl,
                      ref abbrev))
  | Tobject (t1, {contents = name}) ->
      let ty' () =
        let name' =
          match name with
            None ->
              None
          | Some (p, tl) ->
              Some (p, List.map (copy_rec abbrev visited) tl)
        in
          newty (Tobject (copy_rec abbrev visited t1, ref name'))
      in
      if opened_object ty then
        try
          List.assq ty !inst_subst
        with Not_found ->
          if v = ref One then begin
            let t = newvar () in
    	    v := Many t;
            inst_subst := (ty, t):: !inst_subst
          end;
          ty' ()
      else
        ty' ()
  | Tfield (label, t1, t2) ->
      newty (Tfield (label, copy_rec abbrev visited t1,
                            copy_rec abbrev visited t2))
  | Tnil ->
      newty Tnil
  | Tlink _ ->
      fatal_error "Ctype.copy_rec"

let copy ty = copy_rec [] [] ty
let subst abbrev ty = copy_rec abbrev [] ty
let copy_parameterized params ty = copy_rec [] params ty

let instance sch =
  inst_subst := [];
  let ty = copy sch in
  inst_subst := [];
  ty

let instance_constructor cstr =
  inst_subst := [];
  let ty_res = copy cstr.cstr_res in
  let ty_args = List.map copy cstr.cstr_args in
  inst_subst := [];
  (ty_args, ty_res)

let instance_label lbl =
  inst_subst := [];
  let ty_res = copy lbl.lbl_res in
  let ty_arg = copy lbl.lbl_arg in
  inst_subst := [];
  (ty_arg, ty_res)

let substitute abbrev params args body =
  inst_subst := List.combine params args;
  let ty = subst abbrev body in
  inst_subst := [];
  ty

let instance_parameterized_type sch_args sch =
  inst_subst := [];
  let params = List.map (function p -> (repr p, ref Zero)) sch_args in
  let ty_args = List.map (copy_parameterized params) sch_args in
  let ty = copy_parameterized params sch in
  inst_subst := [];
  (ty_args, ty)

let instance_parameterized_type_2 sch_args sch_lst sch =
  inst_subst := [];
  let params = List.map (function p -> (repr p, ref Zero)) sch_args in
  let ty_args = List.map (copy_parameterized params) sch_args in
  let ty_lst = List.map (copy_parameterized params) sch_lst in
  let ty = copy_parameterized params sch in
  inst_subst := [];
  (ty_args, ty_lst, ty)

let instance_class cl =
  inst_subst := [];
  let params0 = List.map (function p -> (repr p, ref Zero)) cl.cty_params in
  let params = List.map (copy_parameterized params0) cl.cty_params in
  let args = List.map (copy_parameterized params0) cl.cty_args in
  let vars =
    Vars.fold
      (fun lab (mut, ty) ->
         Vars.add lab (mut, copy_parameterized params0 ty))
      cl.cty_vars
      Vars.empty in
  let self = copy_parameterized params0 cl.cty_self in
  inst_subst := [];
  (params, args, vars, self)

(* Unification *)

let rec update_level level ty =
  let ty = repr2 ty in
  if ty.level > level then begin
    ty.level <- level;
    match ty.desc with
      Tvar               -> ()
    | Tarrow(t1,t2)      -> update_level level t1; update_level level t2
    | Ttuple(ty_list)    -> List.iter (update_level level) ty_list
    | Tconstr(_, tl, _)  -> List.iter (update_level level) tl
    | Tobject (f, _)     -> update_level level f
    | Tfield(_, t1, t2)  -> update_level level t1; update_level level t2
    | Tnil               -> ()
    | Tlink _            -> fatal_error "Ctype.update_level"
  end

exception Cannot_expand

let rec find_expans p1 =
  function
    [] ->
      None
  | (p2, ty)::l ->
      if Path.same p1 p2 then
        Some ty
      else
        find_expans p1 l

let expand_abbrev env path args abbrev level =
  match find_expans path !abbrev with
    Some ty ->
      update_level level ty;
      ty
  | None ->
      try
        let decl = Env.find_type path env in
        match decl.type_manifest with
          Some body ->
            let v = newvar () in
            abbrev := (path, v)::!abbrev;
            let old_level = !current_level in
            current_level := level;
            let ty = substitute !abbrev decl.type_params args body in
            current_level := old_level;
            v.desc <- Tlink ty;
            ty
        | _ ->
            raise Cannot_expand
      with Not_found ->
        raise Cannot_expand

let generic_abbrev env path =
  try
    let decl = Env.find_type path env in
    match decl.type_manifest with
      Some body ->
        body.level = generic_level
    | _ ->
        false
  with
    Not_found ->
      false

let occur env ty0 ty =
  let visited = ref ([] : type_expr list) in
  let rec occur_rec ty =
    match ty.desc with
      Tlink ty' ->
        occur_rec ty'
    | Tvar ->
        if ty == ty0 then raise Unify else
        ()
    | Tarrow(t1, t2) ->
        occur_rec t1; occur_rec t2
    | Ttuple tl ->
        List.iter occur_rec tl
    | Tconstr(p, [], _) ->
        ()
    | Tconstr(p, tl, abbrev) ->
        if not (List.memq ty !visited) then begin
          visited := ty :: !visited;
          try List.iter occur_rec tl with Unify ->
          try occur_rec (expand_abbrev env p tl abbrev ty.level)
          with Cannot_expand ->
          ()
        end
    | Tobject (_, _) ->
        ()
    | Tfield (_, _, _) | Tnil ->
        fatal_error "Ctype.occur"
  in
    occur_rec ty

let rec unify_rec env a1 a2 t1 t2 =     (* Variables and abbreviations *)
  if t1 == t2 then () else
  let t1 = repr2 t1 in
  let t2 = repr2 t2 in
  if t1 == t2 then () else
  match (t1.desc, t2.desc) with
    (Tvar, _) ->
       update_level t1.level t2;
       begin match a2 with
         None    -> occur env t1 t2; t1.desc <- Tlink t2
       | Some l2 -> occur env t1 l2; t1.desc <- Tlink l2
       end
  | (_, Tvar) ->
       update_level t2.level t1;
       begin match a1 with
         None    -> occur env t2 t1; t2.desc <- Tlink t1
       | Some l1 -> occur env t2 l1; t2.desc <- Tlink l1
       end
  | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _)) when Path.same p1 p2 ->
       unify_core env a1 a2 t1 t2
  | (Tconstr (p1, tl1, abbrev1), Tconstr (p2, tl2, abbrev2)) ->
      begin
        try
          let t3 = expand_abbrev env p1 tl1 abbrev1 t1.level in
          update_level t2.level t1;
          unify_rec env (Some t1) a2 t3 t2
        with Cannot_expand ->
        try
          let t3 = expand_abbrev env p2 tl2 abbrev2 t2.level in
          update_level t1.level t2;
          unify_rec env a1 (Some t2) t1 t3
        with Cannot_expand ->
          raise Unify
      end
  | (Tconstr (p1, tl1, abbrev1), _) ->
      begin try
        let t3 = expand_abbrev env p1 tl1 abbrev1 t1.level in
        update_level t2.level t1;
        unify_rec env (Some t1) a2 t3 t2
      with Cannot_expand ->
        unify_core env a1 a2 t1 t2
      end
  | (_, Tconstr (p2, tl2, abbrev2)) ->
      begin try
        let t3 = expand_abbrev env p2 tl2 abbrev2 t2.level in
        update_level t1.level t2;
        unify_rec env a1 (Some t2) t1 t3
      with Cannot_expand ->
        unify_core env a1 a2 t1 t2
      end
  | (_, _) ->
      unify_core env a1 a2 t1 t2

and unify_core env a1 a2 t1 t2 =        (* Other cases *)
  let d1 = t1.desc and d2 = t2.desc in
  begin match (a1, a2) with
    (None,    Some l2) ->
      update_level t1.level t2; t1.desc <- Tlink l2
  | (Some l1, None) ->
      update_level t2.level t1; t2.desc <- Tlink l1
  | (_, _) ->
      update_level t1.level t2; t1.desc <- Tlink t2
  end;
  try
    match (d1, d2) with
      (Tarrow (t1, u1), Tarrow (t2, u2)) ->
        unify_rec env None None t1 t2; unify_rec env None None u1 u2
    | (Ttuple tl1, Ttuple tl2) ->
        unify_list env tl1 tl2
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) (*when Path.same p1 p2*) ->
        ()
    | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _)) (*when Path.same p1 p2*) ->
        unify_list env tl1 tl2
    | (Tobject (fi1, nm1), Tobject (fi2, nm2)) ->
        let old_nm = !nm2 in
        begin match old_nm with
          Some (_, va::_) when (repr va).desc = Tvar -> ()
        | _                                          -> nm2 := !nm1
        end;
        begin try unify_fields env fi1 fi2 with exn ->
          nm2 := old_nm;
          raise exn
        end
    | (_, _) ->
        raise Unify
  with exn ->
    t1.desc <- d1;
    t2.desc <- d2;
    raise exn

and unify_list env tl1 tl2 =
  try
    List.iter2 (unify_rec env None None) tl1 tl2
  with Invalid_argument _ ->
    raise Unify

and unify_fields env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let va = newvar () in
    begin match rest1.desc with
      Tvar ->
        let nr = build_fields miss2 va in
        update_level rest1.level nr;
        rest1.desc <- Tlink nr
    | Tnil ->
        if miss2 <> [] then raise Unify;
        va.desc <- Tlink {desc = Tnil; level = va.level}
    | _ ->
        fatal_error "Ctype.unify_fields (1)"
    end;
    begin match rest2.desc with
      Tvar ->
        let nr = build_fields miss1 va in
        update_level rest2.level nr;
        rest2.desc <- Tlink nr
    | Tnil ->
        if miss1 <> [] then raise Unify;
        va.desc <- Tlink {desc = Tnil; level = va.level}
    | _ ->
        fatal_error "Ctype.unify_fields (2)"
    end;
    List.iter (fun (t1, t2) -> unify_rec env None None t1 t2) pairs

let unify env ty1 ty2 =
  unify_rec env None None ty1 ty2

let rec filter_arrow env t =
  let t = repr t in
  match t.desc with
    Tvar ->
      let t1 = newvar () and t2 = newvar () in
      let t' = newty (Tarrow (t1, t2)) in
      update_level t.level t';
      t.desc <- Tlink t';
      (t1, t2)
  | Tarrow(t1, t2) ->
      (t1, t2)
  | Tconstr(p, tl, abbrev) ->
      begin try
        filter_arrow env (expand_abbrev env p tl abbrev t.level)
      with Cannot_expand ->
        raise Unify
      end
  | _ ->
      raise Unify

let rec filter_method_field name ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      let ty1 = newvar () and ty2 = newvar () in
      let ty' = newty (Tfield (name, ty1, ty2)) in
      update_level ty.level ty';
      ty.desc <- Tlink ty';
      ty1
  | Tfield(n, ty1, ty2) ->
      if Label. (=) n name then
        ty1
      else
        filter_method_field name ty2
  | _ ->
      raise Unify

let rec filter_method env name ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      let ty1 = newvar ()in
      let ty' = newobj ty1 in
      update_level ty.level ty';
      ty.desc <- Tlink ty';
      filter_method_field name ty1
  | Tobject(f, _) ->
      filter_method_field name f
  | Tconstr(p, tl, abbrev) ->
      begin try
        filter_method env name (expand_abbrev env p tl abbrev ty.level)
      with Cannot_expand ->
        raise Unify
      end
  | _ ->
      raise Unify

(* Matching between type schemes *)

let rec moregen_occur ty0 ty =
  let visited = ref [] in
  let rec occur_rec ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      (* ty0 has level = !current_level iff it is generic
         in the original type scheme. In this case, it can be freely
         instantiated. Otherwise, ty0 is not generic
         and cannot be instantiated by a type that contains
         generic variables. *)
      if ty.level = generic_level & ty0.level < !current_level
      then raise Unify
  | Tarrow(t1, t2) ->
      occur_rec t1; occur_rec t2
  | Ttuple tl ->
      List.iter occur_rec tl
  | Tconstr(p, tl, _) ->
      if not (List.memq ty !visited) then begin
        visited := ty::!visited;
        List.iter occur_rec tl
      end
  | Tobject(f, _) ->
      if not (List.memq ty !visited) then begin
        visited := ty::!visited;
        occur_rec f
      end
  | Tfield(_, t1, t2) ->
      occur_rec t1; occur_rec t2
  | Tnil ->
      ()
  | Tlink _ ->
      fatal_error "Ctype.moregen_occur"
  in
    occur_rec ty

let rec moregen env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else
  let d1 = t1.desc in
  try
    begin match (t1.desc, t2.desc) with
      (Tvar, _) ->
        if t1.level = generic_level then raise Unify;
        occur env t1 t2;
        moregen_occur t1 t2;
        t1.desc <- Tlink t2
    | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
        moregen env t1 t2; moregen env u1 u2
    | (Ttuple tl1, Ttuple tl2) ->
        moregen_list env tl1 tl2
    | (Tconstr(p1, tl1, abbrev1), Tconstr(p2, tl2, abbrev2)) ->
        if Path.same p1 p2 then begin
          t1.desc <- Tlink t2;
          moregen_list env tl1 tl2;
          t1.desc <- d1
        end else begin
          try
            moregen env (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
          with Cannot_expand ->
          try
            moregen env t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
          with Cannot_expand ->
            raise Unify
        end
    | (Tobject(f1, _), Tobject(f2, _)) ->
        t1.desc <- Tlink t2;
        moregen_fields env f1 f2;
        t1.desc <- d1
    | (Tconstr(p1, tl1, abbrev1), _) ->
        begin try
          moregen env (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
        with Cannot_expand ->
          raise Unify
        end
    | (_, Tconstr(p2, tl2, abbrev2)) ->
        begin try
          moregen env t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
        with Cannot_expand ->
          raise Unify
        end
    | (_, _) ->
        raise Unify
    end
  with exn ->
    t1.desc <- d1;
    raise exn

and moregen_list env tl1 tl2 =
  try
    List.iter2 (moregen env) tl1 tl2
  with Invalid_argument _ ->
    raise Unify

and moregen_fields env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  if miss1 <> [] then raise Unify;
  begin match rest1.desc with
    Tvar ->
      if rest1.level = generic_level then raise Unify;
      let fi = build_fields miss2 rest2 in
      moregen_occur rest1 fi
  | Tnil ->
      if miss2 <> [] then raise Unify;
      if rest2.desc <> Tnil then raise Unify
  | _ ->
      fatal_error "moregen_fields"
  end;
  List.iter (fun (t1, t2) -> moregen env t1 t2) pairs

let moregeneral env sch1 sch2 =
  begin_def();
  try
    moregen env (instance sch1) sch2;
    remove_abbrev sch2;
    end_def();
    true
  with Unify ->
    remove_abbrev sch2;
    end_def();
    false

(* Equivalence between parameterized types *)

let equal env params1 ty1 params2 ty2 =
  let subst = ref (List.combine params1 params2) in
  let type_pairs = ref [] in
  let rec eqtype t1 t2 =
    let t1 = repr t1 in
    let t2 = repr t2 in
    match (t1.desc, t2.desc) with
      (Tvar, Tvar) ->
        begin try
          List.assq t1 !subst == t2
        with Not_found ->
          subst := (t1, t2) :: !subst;
          true
        end
    | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
        eqtype t1 t2 & eqtype u1 u2
    | (Ttuple tl1, Ttuple tl2) ->
        eqtype_list tl1 tl2
    | (Tconstr(p1, tl1, abbrev1), Tconstr(p2, tl2, abbrev2)) ->
        List.exists (function (t1', t2') -> t1 == t1' & t2 == t2') !type_pairs
        or begin
          type_pairs := (t1, t2) :: !type_pairs;
          if Path.same p1 p2 then
            eqtype_list tl1 tl2
          else begin
            try
              eqtype (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
            with Cannot_expand ->
            try
              eqtype t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
            with Cannot_expand ->
              false
          end
        end
    | (Tobject (f1, _), Tobject (f2, _)) ->
        List.exists (function (t1', t2') -> t1 == t1' & t2 == t2') !type_pairs
        or begin
          type_pairs := (t1, t2) :: !type_pairs;
          eqtype_fields f1 f2
        end
    | (Tconstr(p1, tl1, abbrev1), _) ->
        begin try
          eqtype (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
        with Cannot_expand ->
          false
        end
    | (_, Tconstr(p2, tl2, abbrev2)) ->
        begin try
          eqtype t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
        with Cannot_expand ->
          false
        end
    | (Tnil, Tnil) ->
        true
    | (_, _) ->
        false
  and eqtype_list tl1 tl2 =
    match (tl1, tl2) with
      ([], []) -> true
    | (t1::r1, t2::r2) -> eqtype t1 t2 & eqtype_list r1 r2
    | (_, _) -> false
  and eqtype_fields ty1 ty2 =
    let (fields1, rest1) = flatten_fields ty1
    and (fields2, rest2) = flatten_fields ty2 in
    List.length fields1 = List.length fields2
      &
    eqtype rest1 rest2
      &
    List.for_all
      (function (label, t) ->
         List.exists
           (function (label', t') ->
              (Label. (=) label label') & (eqtype t t'))
           fields2)
      fields1
  in
    let eq = eqtype ty1 ty2 in
    remove_abbrev ty1; remove_abbrev ty2;
    eq

(* Subtyping *)

let visited = ref ([] : type_expr list)

let rec build_subtype env vars t =
  if List.memq t vars then (t, false) else
  match t.desc with
    Tlink t' ->
      build_subtype env vars t'
  | Tvar ->
      (t, false)
  | Tarrow(t1, t2) ->
      let (t1', c1) = build_supertype env vars t1 in
      let (t2', c2) = build_subtype env vars t2 in
      if c1 or c2 then (new_global_ty (Tarrow(t1', t2')), true)
      else (t, false)
  | Ttuple tlist ->
      let (tlist', clist) =
        List.split (List.map (build_subtype env vars) tlist)
      in
      if List.exists (function c -> c) clist then
        (new_global_ty (Ttuple tlist'), true)
      else (t, false)
  | Tconstr(p, tl, abbrev) ->
      if generic_abbrev env p then begin
        let t' = expand_abbrev env p tl abbrev t.level in
        let (t'', c) = build_subtype env vars t' in
        if c then (t'', true)
        else (t, false)
      end else
        (t, false)
  | Tobject (t1, _) ->
      if opened t1 then
        (t, false)
      else if List.memq t !visited then
        (t, false)
      else begin
        let old_visited = !visited in
        visited := t :: old_visited;
        let (t1', _) = build_subtype env vars t1 in
        visited := old_visited;
        (new_global_ty (Tobject (t1', ref None)), true)
      end
  | Tfield(s, t1, t2) ->
      let (t1', _) = build_subtype env vars t1 in
      let (t2', _) = build_subtype env vars t2 in
      (new_global_ty (Tfield(s, t1', t2')), true)
  | Tnil ->
      let v = new_global_var () in
      (v, true)

and build_supertype env vars t =
  if List.memq t vars then (t, false) else
  match t.desc with
    Tlink t' ->
      build_supertype env vars t'
  | Tvar ->
      (t, false)
  | Tarrow(t1, t2) ->
      let (t1', c1) = build_subtype env vars t1 in
      let (t2', c2) = build_supertype env vars t2 in
      if c1 or c2 then (new_global_ty (Tarrow(t1', t2')), true)
      else (t, false)
  | Ttuple tlist ->
      let (tlist', clist) =
        List.split (List.map (build_supertype env vars) tlist)
      in
      if List.exists (function c -> c) clist then
        (new_global_ty (Ttuple tlist'), true)
      else
        (t, false)
  | Tconstr(p, tl, abbrev) ->
      if generic_abbrev env p then begin
        let t' = expand_abbrev env p tl abbrev t.level in
        let (t'', c) = build_supertype env vars t' in
        if c then (t'', c)
        else (t, false)
      end else
        (t, false)
  | Tobject (t1, _) ->
      if opened t1 then
        (t, false)
      else if List.memq t !visited then
        (t, false)
      else begin
        let old_visited = !visited in
        visited := t :: old_visited;
        let (t1', c) = build_supertype env vars t1 in
        visited := old_visited;
        if c then (new_global_ty (Tobject (t1', ref None)), true)
        else (t, false)
      end
  | Tfield(s, t1, t2) ->
      let (t1', c1) = build_supertype env vars t1 in
      let (t2', c2) = build_supertype env vars t2 in
      if c1 or c2 then (new_global_ty (Tfield(s, t1', t2')), true)
      else (t, false)
  | Tnil ->
      (t, false)

let enlarge_type env vars ty =
  visited := [];
  let (ty', _) = build_subtype env vars ty in
    visited := [];
    ty'

let subtypes = ref [];;
let known_subtype t1 t2 =
  List.exists (fun (t1', t2') -> t1 == t1' & t2 == t2') !subtypes

let rec subtype_rec env vars t1 t2 =
  if t1 == t2 then () else
  if List.memq t1 vars or List.memq t2 vars then unify env t1 t2 else
    match (t1.desc, t2.desc) with
      (Tlink t1', _) ->
        subtype_rec env vars t1' t2
    | (_, Tlink t2') ->
        subtype_rec env vars t1 t2'
    | (Tvar, _) | (_, Tvar) ->
        unify env t1 t2
    | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
        subtype_rec env vars t2 t1; subtype_rec env vars u1 u2
    | (Ttuple tl1, Ttuple tl2) ->
        subtype_list env vars tl1 tl2
    | (Tconstr(p1, tl1, abbrev1), Tconstr(p2, tl2, abbrev2)) ->
        if generic_abbrev env p1 then
          subtype_rec env vars (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
        else if generic_abbrev env p2 then
          subtype_rec env vars t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
        else
          unify env t1 t2
    | (Tobject (f1, _), Tobject (f2, _)) ->
        if not (known_subtype t1 t2) then begin
          if opened f1 & opened f2 then
            unify env t1 t2
          else begin
            subtypes := (t1, t2) :: !subtypes;
            subtype_fields env vars f1 f2
          end
        end
    | (Tconstr(p1, tl1, abbrev1), _) ->
        if generic_abbrev env p1 then
          subtype_rec env vars (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
        else
          unify env t1 t2
    | (_, Tconstr(p2, tl2, abbrev2)) ->
        if generic_abbrev env p2 then
          subtype_rec env vars t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
        else
          unify env t1 t2
    | (_, _) ->
        raise Unify

and subtype_list env vars tl1 tl2 =
  try
    List.iter2 (subtype_rec env vars) tl1 tl2
  with Invalid_argument _ ->
    raise Unify

and subtype_fields env vars ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1 in
  let (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  begin match rest1.desc with
    Tvar ->
      let nr = build_fields miss2 (newvar ()) in
      update_level rest1.level nr;
      rest1.desc <- Tlink nr
  | Tnil   -> if miss2 <> [] then raise Unify
  | _      -> fatal_error "Ctype.subtype_fields (1)"
  end;
  begin match rest2.desc with
    Tvar ->
      let nr = build_fields miss1 (newvar ()) in
      update_level rest2.level nr;
      rest2.desc <- Tlink nr
  | Tnil   -> ()
  | _      -> fatal_error "Ctype.subtype_fields (2)"
  end;
  List.iter (fun (t1, t2) -> subtype_rec env vars t1 t2) pairs

let subtype env vars ty1 ty2 =
  subtypes := [];
  subtype_rec env vars ty1 ty2;
  subtypes := []

(* Remove dependencies *)

let inst_subst = ref ([] : (type_expr * type_expr) list)

let rec nondep_type_rec env id ty =
  let ty = repr ty in
  if ty.desc = Tvar then ty else
  try List.assq ty !inst_subst with Not_found ->
    let ty' = new_gen_var () in
    inst_subst := (ty, ty') :: !inst_subst;
    ty'.desc <-
      begin match ty.desc with
        Tvar ->
          Tvar
      | Tarrow(t1, t2) ->
          Tarrow(nondep_type_rec env id t1, nondep_type_rec env id t2)
      | Ttuple tl ->
          Ttuple(List.map (nondep_type_rec env id) tl)
      | Tconstr(p, tl, abbrev) ->
          if Path.isfree id p then
            begin try
              (nondep_type_rec env id
                 (expand_abbrev env p tl (ref !abbrev) ty.level)).desc
            with Cannot_expand ->
              raise Not_found
            end
          else
            Tconstr(p, List.map (nondep_type_rec env id) tl, ref [])
      | Tobject (t1, name) ->
          Tobject (nondep_type_rec env id t1,
                 ref (match !name with
                        None -> None
                      | Some (p, tl) ->
                          if Path.isfree id p then None
                          else Some (p, List.map (nondep_type_rec env id) tl)))
       | Tfield(label, t1, t2) ->
           Tfield(label, nondep_type_rec env id t1, nondep_type_rec env id t2)
       | Tnil ->
           Tnil
       | Tlink _ ->
           fatal_error "Ctype.nondep_type"
       end;
     ty'

let nondep_type env id ty =
  inst_subst := [];
  let ty' = nondep_type_rec env id ty in
  inst_subst := [];
  ty'

let nondep_class_type env id decl =
  inst_subst := [];
  let decl =
    { cty_params = List.map (nondep_type_rec env id) decl.cty_params;
      cty_args = List.map (nondep_type_rec env id) decl.cty_args;
      cty_vars =
        Vars.fold (fun l (m, t) -> Vars.add l (m, nondep_type_rec env id t))
          decl.cty_vars Vars.empty;
      cty_self = nondep_type_rec env id decl.cty_self;
      cty_concr = decl.cty_concr;
      cty_new =
        begin match decl.cty_new with
          None    -> None
        | Some ty -> Some (nondep_type_rec env id ty)
        end }
  in
  inst_subst := [];
  decl

(* Type pruning *)

let inst_subst = ref ([] : (type_expr * type_expr) list)

let rec prune_rec top cstr ty =
  let ty = repr ty in
  try List.assq ty (if top then [] else cstr) with Not_found ->
  match ty.desc with
    Tvar ->
      if ty.level = generic_level then
        begin try
          List.assq ty !inst_subst
        with Not_found ->
          let ty' = newvar() in
          inst_subst := (ty, ty') :: !inst_subst;
          ty'
        end
      else
        ty
  | Tarrow(t1, t2) ->
      newty (Tarrow(prune_rec false cstr t1, prune_rec false cstr t2))
  | Ttuple tl ->
      newty (Ttuple(List.map (prune_rec false cstr) tl))
  | Tconstr(p, tl, _) ->
      begin try
        List.assq ty !inst_subst
      with Not_found ->
        let ty' = newvar() in
        inst_subst := (ty, ty') :: !inst_subst;
        let ty'' = 
          newty (Tconstr(p, List.map (prune_rec false cstr) tl, ref []))
        in
          ty'.desc <- Tlink ty'';
          ty''
      end
  | Tobject (t1, name) ->
      begin try
        List.assq ty !inst_subst
      with Not_found ->
        let ty' = newvar() in
        inst_subst := (ty, ty') :: !inst_subst;
        let ty'' = newty
          (Tobject (prune_rec false cstr t1,
                    ref (match !name with
                           None -> None
                         | Some (p, tl) ->
                             Some (p, List.map (prune_rec false cstr) tl))))
        in
          ty'.desc <- Tlink ty'';
          ty''
      end
  | Tfield(label, t1, t2) ->
      newty (Tfield(label, prune_rec false cstr t1, prune_rec false cstr t2))
  | Tnil ->
      newty Tnil
  | Tlink _             ->
      fatal_error "Ctype.prune_rec"

let prune_cstr cstr (old_cstr, new_cstr) ((ty, v) as c) =
  let c' =
    try (v, List.assq ty old_cstr) with Not_found ->
    match ty.desc with
      Tvar ->
        (v, v)
    | _ ->
        (v, prune_rec true cstr ty)
  in
    (c :: old_cstr, c' :: new_cstr)

let prune ty leaves =
  inst_subst := [];
  let cstr = List.map (fun leaf -> (repr leaf, newvar ())) leaves in
  let new_ty = prune_rec true cstr ty in
  inst_subst := [];
  (new_ty, List.map (fun (ty, v) -> (v, ty)) cstr)

let prune_class_type cl =
  inst_subst := [];
  let cstr = List.map (fun leaf -> (repr leaf, newvar ())) cl.cty_params in
  let args = List.map (prune_rec false cstr) cl.cty_args in
  let vars =
    Vars.fold
      (fun lab (mut, ty) -> Vars.add lab (mut, prune_rec false cstr ty))
      cl.cty_vars Vars.empty in
  let self = prune_rec true cstr cl.cty_self in
  let (_, cstr) = List.fold_left (prune_cstr cstr) ([], []) cstr in
  inst_subst := [];
  (List.rev cstr, args, vars, self)

(* --- *)

let rec row_variable ty =
  let ty = repr ty in
  match ty.desc with
    Tfield (_, _, ty) -> row_variable ty
  | Tvar              -> ty
  | Tnil              -> raise Not_found
  | _                 -> fatal_error "Ctype.row_variable"

let close_object ty =
  let rec close ty =
    let ty = repr ty in
    match ty.desc with
      Tvar              ->
        ty.desc <- Tlink {desc = Tnil; level = ty.level}
    | Tfield(_, _, ty') -> close ty'
    | Tnil              -> ()
    | _                 -> fatal_error "Ctype.close_object (1)"
  in
  match (repr ty).desc with
    Tobject (ty, _)   -> close ty
  | Tconstr (_, _, _) -> ()             (* Already closed *)
  | _                 -> fatal_error "Ctype.close_object (2)"


let set_object_name ty params id =
  match (repr ty).desc with
    Tobject (fi, nm) ->
      begin try
        nm := Some (Path.Pident id, (row_variable fi)::params)
      with Not_found ->
        ()
      end
  | Tconstr (_, _, _) ->
      ()
  | _ ->
      fatal_error "Ctype.set_object_name"

let remove_object_name ty =
  match (repr ty).desc with
    Tobject (_, nm)   -> nm := None
  | Tconstr (_, _, _) -> ()
  | _                 -> fatal_error "Ctype.remove_object_name"

let rec expand_root env ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr(p, tl, abbrev) ->
      begin try
        expand_root env (expand_abbrev env p tl (ref !abbrev) ty.level)
      with Cannot_expand ->
        ty
      end
  | _ ->
      ty

(* Abbreviation correctness *)

exception Nonlinear_abbrev
exception Recursive_abbrev

let rec correct_abbrev_rec env path params constrs visited ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      []
  | Tarrow (ty1, ty2) ->
      let c1 = correct_abbrev_rec env path params constrs visited ty1 in
      let c2 = correct_abbrev_rec env path params constrs visited ty2 in
      c1 @ c2
  | Ttuple tl ->
      List.flatten
        (List.map (correct_abbrev_rec env path params constrs visited) tl)
  | Tconstr(p, args, abbrev) ->
      if Path.same p path then begin
        if
          List.exists (fun (ty1, ty2) -> repr ty1 != repr ty2)
            (List.combine params args)
        then
          raise Nonlinear_abbrev
        else
          [p]
      end else begin
          try
            let ty' = expand_abbrev env p args abbrev ty.level in
            if List.memq ty' constrs then [] else
            let loops =
              correct_abbrev_rec env path params (ty'::constrs) visited ty'
            in
            if List.exists (Path.same p) loops
            then raise Recursive_abbrev
            else loops
          with Cannot_expand ->
          if not (List.memq ty visited) then begin
            List.iter
              (correct_abbrev_rec env path params constrs (ty::visited))
              args;
            ()
          end;
          []
      end
  | Tobject (ty', _) ->
      if not (List.memq ty visited) then begin
        correct_abbrev_rec env path params constrs (ty::visited) ty';
        ()
      end;
      []
  | Tfield(_, ty1, ty2) ->
      correct_abbrev_rec env path params constrs visited ty1;
      correct_abbrev_rec env path params constrs visited ty2;
      []
  | Tnil ->
      []
  | Tlink _ -> fatal_error "Ctype.correct_abbrev_rec"

let correct_abbrev env ident params ty =
  let path = Path.Pident ident in
  let incorrect =
    List.exists (Path.same path) (correct_abbrev_rec env path params [] [] ty)
  in
  remove_abbrev ty;
  if incorrect then
    raise Recursive_abbrev

(* Miscellaneous *)

let unroll_abbrev id tl ty =
  let ty = repr ty in
  match ty.desc with
    Tobject (fi, nm) ->
      ty.desc <-
        Tlink {desc = Tconstr (Path.Pident id, tl, ref []);
               level = generic_level};
      {desc = Tobject (fi, nm); level = ty.level}
  | _ ->
      ty

let visited = ref []

let closed_schema ty =
  let rec closed_schema_rec ty =
    let ty = repr ty in
    match ty.desc with
      Tvar -> ty.level = generic_level
    | Tarrow(t1, t2) -> closed_schema_rec t1 & closed_schema_rec t2
    | Ttuple tl -> List.for_all closed_schema_rec tl
    | Tconstr(p, tl, _) ->
        if not (List.memq ty !visited) then begin
          visited := ty::!visited;
          List.for_all closed_schema_rec tl
        end else
          true
    | Tobject(f, _) ->
        if not (List.memq ty !visited) then begin
          visited := ty::!visited;
          closed_schema_rec f
        end else
          true
    | Tfield(_, t1, t2) ->
        closed_schema_rec t1 & closed_schema_rec t2
    | Tnil ->
        true
    | Tlink _           -> fatal_error "Ctype.closed_schema"
  in
    visited := [];
    let res = closed_schema_rec ty in
    visited := [];
    res

let is_generic ty =
  let ty = repr ty in
  match ty.desc with
    Tvar -> ty.level = generic_level
  | _ -> fatal_error "Ctype.is_generic"

let rec arity ty =
  match (repr ty).desc with
    Tarrow(t1, t2) -> 1 + arity t2
  | _ -> 0
