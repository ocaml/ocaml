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
open Types

(******* Notes

   - As much sharing as possible should be kept : it makes types
     smaller and better abbreviated.
     When necessary, some sharing can be lost. Types will still be
     printed correctly (XXX a faire...), and types defined for a class
     do not depend on sharing thanks to constrained abbreviations (XXX
     a faire...).
   - All nodes of a type must have a level : that way, one know
     whether a node must be duplicated or not when instantiating a
     type.
   - Levels of a type must be decreasing.
   - The level of a type constructor must be superior to the binding
     time of its path.

*)

(****** A faire

   - Revoir affichage des types.
   - Abreviations avec contraintes.
   - Types recursifs sans limitation.
   - #-type implementes comme de vraies abreviations.
   - Effacer les abreviations memorisees plus tot ?

*)

(**** Errors ****)

exception Unify of (type_expr * type_expr) list

exception Subtype of
        (type_expr * type_expr) list * (type_expr * type_expr) list

(**** Type level management ****)

let generic_level = (-1)
let current_level = ref 0
let global_level = ref 1

let init_def level = current_level := level
let begin_def () = incr current_level
let end_def () = decr current_level

let reset_global_level () =
  global_level := !current_level + 1

(**** Some type creators ****)

let newgenty desc      = { desc = desc; level = generic_level }
let newgenvar ()     = newgenty Tvar

let newty desc         = { desc = desc; level = !current_level }
let newvar ()          = { desc = Tvar; level = !current_level }
let newobj fields      = newty (Tobject (fields, ref None))

let new_global_ty desc = { desc = desc; level = !global_level }
let new_global_var ()  = new_global_ty Tvar

let none = newty (Ttuple [])                (* Clearly ill-formed type *)

(**** Representative of a type ****)

let rec repr =
  function
    {desc = Tlink t'} as t ->
      let r = repr t' in
      if r != t' then t.desc <- Tlink r;
      r
  | t -> t


                  (**********************************************)
                  (*  Miscellaneous operations on object types  *)
                  (**********************************************)


(**** Object field manipulation. ****)

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
        if n = n' then
          associate ((t, t')::p) s s' (r, r')
        else if n < n' then
          associate p ((n, t)::s) s' (r, l')
        else
          associate p s ((n', t')::s') (l, r')
  in let sort = Sort.list (fun (n, _) (n', _) -> n < n') in
  associate [] [] [] (sort fields1, sort fields2)

(**** Check whether an object is open ****)

(* XXX Faudra penser a eventuellement expanser l'abreviation *)
let rec opened_object ty =
  match (repr ty).desc with
    Tobject (t, _)  -> opened_object t
  | Tfield(_, _, t) -> opened_object t
  | Tvar            -> true
  | _               -> false

(**** Close an object ****)

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


(**** Object name manipulation ****)
(* XXX Obsolete *)

let rec row_variable ty =
  let ty = repr ty in
  match ty.desc with
    Tfield (_, _, ty) -> row_variable ty
  | Tvar              -> ty
  | Tnil              -> raise Not_found
  | _                 -> fatal_error "Ctype.row_variable"

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

(**** Type level manipulation ****)

let rec generalize ty =
  let ty = repr ty in
  if ty.level > !current_level then begin
    ty.level <- generic_level;
    begin match ty.desc with
      Tconstr(_, tl, ab) -> ab := []
    | _                  -> ()
    end;
    iter_type_expr generalize ty
  end

(* Lower the levels of a type *)
let rec update_level level ty =
  let ty = repr ty in
  if ty.level > level then begin
    ty.level <- level;
    begin match ty.desc with
      Tconstr(p, tl, _)  when level < Path.binding_time p -> raise (Unify [])
    | _ -> ()
    end;
    iter_type_expr (update_level level) ty
  end

let make_nongen ty = update_level !current_level ty


                              (*******************)
                              (*  Instantiation  *)
                              (*******************)

(*
   Generic nodes are duplicated, while non-generic nodes are left
   as-is. The instance cannot be generic.
   During instantiation, the description of a generic node is first
   replaced by a link to a stub ([Tlink (newvar ())]). Once the copy
   is made, it replaces the stub.
   After instantiation, the description of generic node, which was
   stored in [saved_desc], must be put back, using [cleanup_types].
   [bind_param t t'] can be used to replace a node [t] by a node [t'].
*)

let saved_desc = ref []
  (* Saved association of generic node with their description. *)
let abbreviations = ref []
  (* Abbreviation memorized. *)

let rec copy =
  function  (* [repr] cannot be used here. *)
    {desc = Tlink ty'} -> copy ty'
  | ty ->
      if ty.level <> generic_level then ty else
      let desc = ty.desc in
      saved_desc := (ty, desc)::!saved_desc;
      let t = newvar () in              (* Stub *)
      ty.desc <- Tlink t;
      t.desc <-
        begin match desc with
          Tvar ->
            Tvar
        | Tarrow (t1, t2) ->
            Tarrow (copy t1, copy t2)
        | Ttuple tl ->
            Ttuple (List.map copy tl)
        | Tconstr (p, [], _) ->
            Tconstr (p, [], ref !abbreviations)
        | Tconstr (p, tl, _) ->
            Tconstr (p, List.map copy tl, ref !abbreviations)
        | Tobject (t1, {contents = name}) ->
            let name' =
              match name with
                None ->
                  None
              | Some (p, tl) ->
                  Some (p, List.map copy tl)
            in
            Tobject (copy t1, ref name')
        | Tfield (label, t1, t2) ->
            Tfield (label, copy t1, copy t2)
        | Tnil ->
            Tnil
        | Tlink t ->
            Tlink (copy t)
        end;
      t

let bind_param ty ty' =
  saved_desc := (ty, ty.desc)::!saved_desc;
  ty.desc <- Tlink ty'

let cleanup_types () =
  List.iter (fun (ty, desc) -> ty.desc <- desc) !saved_desc;
  saved_desc := []

(**** Variants of instantiations ****)

let instance sch =
  let ty = copy sch in
  cleanup_types ();
  ty

let instance_constructor cstr =
  let ty_res = copy cstr.cstr_res in
  let ty_args = List.map copy cstr.cstr_args in
  cleanup_types ();
  (ty_args, ty_res)

let instance_label lbl =
  let ty_res = copy lbl.lbl_res in
  let ty_arg = copy lbl.lbl_arg in
  cleanup_types ();
  (ty_arg, ty_res)

let instance_parameterized_type sch_args sch =
  let ty_args = List.map copy sch_args in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty)

let instance_parameterized_type_2 sch_args sch_lst sch =
  let ty_args = List.map copy sch_args in
  let ty_lst = List.map copy sch_lst in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty_lst, ty)

let instance_class cl =
  let params = List.map copy cl.cty_params in
  let args = List.map copy cl.cty_args in
  let vars =
    Vars.fold
      (fun lab (mut, ty) ->
         Vars.add lab (mut, copy ty))
      cl.cty_vars
      Vars.empty in
  let self = copy cl.cty_self in
  cleanup_types ();
  (params, args, vars, self)

(**** Instantiation with duplication ****)

let rec subst abbrev params args body =
  if !current_level <> generic_level then begin
    List.iter2 bind_param params args;
    abbreviations := abbrev;
    let ty = copy body in
    abbreviations := [];
    cleanup_types ();
    ty
  end else begin
    (* One cannot expand directly to a generic type. *)
    incr current_level;
    let vars = List.map (fun _ -> newvar ()) args in
    let ty = subst abbrev params vars body in
    decr current_level;
    generalize ty;
    List.iter2 (fun v a -> v.desc <- Tlink a) vars args;
    ty
  end

let substitute params args body = subst [] params args body


                              (****************************)
                              (*  Abbreviation expansion  *)
                              (****************************)


exception Cannot_expand

(* Search whether the abbreviation has been memorized. *)
let rec find_expans p1 =
  function
    [] ->
      None
  | (p2, ty)::l ->
      if Path.same p1 p2 then
        Some ty
      else
        find_expans p1 l

(* Expand an abbreviation.
   The expansion is memorized. *)
let expand_abbrev env path args abbrev level =
  match find_expans path !abbrev with
    Some ty ->
      update_level level ty;
      ty
  | None ->
      let decl =
        try Env.find_type path env with Not_found -> raise Cannot_expand in
      match decl.type_manifest with
        Some body ->
          let v = newvar () in
          abbrev := (path, v)::!abbrev;
          let old_level = !current_level in
          current_level := level;
          let ty = subst !abbrev decl.type_params args body in
          current_level := old_level;
          v.desc <- Tlink ty;
          ty
      | _ ->
          raise Cannot_expand

(* Recursively expand the root of a type. *)
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

(* Recursively expand the root of a type.
   Also expand #-types. *)
(* XXX This is a hack ! *)
let rec full_expand env ty =
  let ty = repr (expand_root env ty) in
  match ty.desc with
    Tobject (fi, {contents = Some (_, v::_)}) when (repr v).desc = Tvar ->
      { desc = Tobject (fi, ref None); level = ty.level }
  | _ ->
      ty

(* Check whether the abbreviation expands to a well-defined type.
   During the typing of a class, abbreviations for correspondings
   types expand to non-generic types. *)
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


(**** Remove abbreviations from generalized types ****)

let visited = ref ([] : type_expr list)

(* XXX Est-ce utile ? Desactive pour l'instant, et ca a l'air de
   toujours marcher... *)
let remove_abbrev ty =
  let rec remove ty =
    let ty = repr ty in
    if ty.level = generic_level & not (List.memq ty !visited) then begin
      visited := ty :: !visited;
      begin match ty.desc with
        Tconstr(_, tl, ab) -> ab := []
      | _ -> ()
      end;
      iter_type_expr remove ty
    end
  in
    remove ty;
    visited := []


                              (*****************)
                              (*  Unification  *)
                              (*****************)



(**** Occur check ****)

(* XXX A supprimer *)
let occur env ty0 ty =
  let visited = ref ([] : type_expr list) in
  let rec occur_rec ty =
    let ty = repr ty in
    if ty == ty0 then raise (Unify []);
    match ty.desc with
      Tvar ->
        ()
    | Tarrow(t1, t2) ->
        occur_rec t1; occur_rec t2
    | Ttuple tl ->
        List.iter occur_rec tl
    | Tconstr(p, [], abbrev) ->
        ()
    | Tconstr(p, tl, abbrev) ->
        if not (List.memq ty !visited) then begin
          visited := ty :: !visited;
          try List.iter occur_rec tl with Unify _ ->
          try
            let ty' = expand_abbrev env p tl abbrev ty.level in
            occur_rec ty'
          with Cannot_expand -> ()
        end
    | Tobject (_, _) | Tfield (_, _, _) | Tnil ->
        ()
    | Tlink _ ->
        fatal_error "Ctype.occur"
  in
    occur_rec ty

(**** Transform error trace ****)
(* XXX Move it somewhere else ? *)

let expand_trace env trace =
  List.fold_right
    (fun (t1, t2) rem ->
       (repr t1, full_expand env t1)::(repr t2, full_expand env t2)::rem)
    trace []

let rec filter_trace =
  function
    (t1, t1')::(t2, t2')::rem ->
      let rem' = filter_trace rem in
      if (t1 == t1') & (t2 == t2') then
        rem'
      else
        (t1, t1')::(t2, t2')::rem
  | _ ->
      []

(**** Unification ****)

let rec unify_rec env a1 a2 t1 t2 =     (* Variables and abbreviations *)
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else
  try
    match (t1.desc, t2.desc) with
      (Tvar, _) ->
         update_level t1.level t2;
         begin match a2 with
           None    ->
             occur env t1 t2; t1.desc <- Tlink t2
         | Some l2 ->
             occur env t1 l2; t1.desc <- Tlink l2
         end
    | (_, Tvar) ->
         update_level t2.level t1;
         begin match a1 with
           None    ->
             occur env t2 t1; t2.desc <- Tlink t1
         | Some l1 ->
             occur env t2 l1; t2.desc <- Tlink l1
         end
    | (Tconstr (p1, tl1, abbrev1), Tconstr (p2, tl2, abbrev2))
            when Path.same p1 p2 ->
        begin
          try
            unify_core env a1 a2 t1 t2
          with Unify lst ->
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
            raise (Unify lst)
        end
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
            raise (Unify [])
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
  with
    Unify [] ->
      raise (Unify [(t1, t2)])
  | Unify (_::l) ->
      raise (Unify ((t1, t2)::l))

and unify_core env a1 a2 t1 t2 =        (* Other cases *)
  let d1 = t1.desc and d2 = t2.desc in
  begin match (a1, a2) with
    (None,    Some l2) ->
      update_level t1.level t2; t1.desc <- Tlink l2
  | (Some l1, None) ->
      update_level t2.level t1; t2.desc <- Tlink l1
  | (_, _) ->
      update_level t1.level t2; occur env t1 t2; t1.desc <- Tlink t2
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
        unify_fields env fi1 fi2;
        begin match !nm2 with
          Some (_, va::_) when (repr va).desc = Tvar -> ()
        | _                                          -> nm2 := !nm1
        end
    | (Tfield _, Tfield _) ->
        unify_fields env t1 t2
    | (Tnil, Tnil) ->
        ()
    | (_, _) ->
        raise (Unify [])
  with
    Unify l ->
      t1.desc <- d1;
      t2.desc <- d2;
      raise (Unify ((t1, t2)::l))
  | exn ->
      t1.desc <- d1;
      t2.desc <- d2;
      raise exn

and unify_list env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (unify_rec env None None) tl1 tl2

and unify_fields env ty1 ty2 =          (* Optimization *)
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let va = newvar () in
    begin match rest1.desc with
      Tvar ->
        unify_rec env None None rest1 (build_fields miss2 va)
    | Tnil ->
        if miss2 <> [] then raise (Unify []);
        va.desc <- Tlink {desc = Tnil; level = va.level}
    | _ ->
        fatal_error "Ctype.unify_fields (1)"
    end;
    begin match rest2.desc with
      Tvar ->
        unify_rec env None None (build_fields miss1 va) rest2
    | Tnil ->
        if miss1 <> [] then raise (Unify []);
        va.desc <- Tlink {desc = Tnil; level = va.level}
    | _ ->
        fatal_error "Ctype.unify_fields (2)"
    end;
    List.iter (fun (t1, t2) -> unify_rec env None None t1 t2) pairs

let unify env ty1 ty2 =
  try
    unify_rec env None None ty1 ty2
  with Unify trace ->
    let trace = expand_trace env trace in
    match trace with
      t1::t2::rem ->
        raise (Unify (t1::t2::filter_trace rem))
    | _ ->
        fatal_error "Ctype.unify"

(**** Special cases of unification ****)

(* Unify [t] and ['a -> 'b]. Return ['a] and ['b]. *)
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
        raise (Unify [])
      end
  | _ ->
      raise (Unify [])

(* Used by [filter_method]. *)
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
      if n = name then
        ty1
      else
        filter_method_field name ty2
  | _ ->
      raise (Unify [])

(* Unify [ty] and [< name : 'a; .. >]. Return ['a]. *)
let rec filter_method env name ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      let ty1 = newvar () in
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
        raise (Unify [])
      end
  | _ ->
      raise (Unify [])


                        (***********************************)
                        (*  Matching between type schemes  *)
                        (***********************************)

(* XXX A voir... *)


(* XXX This is not really an occur check !!! *)
let moregen_occur env ty0 ty =
  let visited = ref [] in
  let rec occur_rec ty =
    let ty = repr ty in
    if not (List.memq ty !visited) then begin
      visited := ty::!visited;
      begin match ty.desc with
        Tvar when ty.level = generic_level & ty0.level < !current_level ->
          (* ty0 has level = !current_level iff it is generic
             in the original type scheme. In this case, it can be freely
             instantiated. Otherwise, ty0 is not generic
             and cannot be instantiated by a type that contains
             generic variables. *)
          raise (Unify [])
      | Tconstr(p, tl, abbrev) ->
          (* XXX Pourquoi expanser ? *)
          begin try
            List.iter occur_rec tl
          with Unify lst ->
            let ty' =
              try expand_abbrev env p tl abbrev ty.level
              with Cannot_expand -> raise (Unify lst) in
            occur_rec ty'
          end
      | _ ->
          iter_type_expr occur_rec ty
      end
    end
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
        if t1.level = generic_level then raise (Unify []);
        moregen_occur env t1 t2;
        t1.desc <- Tlink t2
    | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
        moregen env t1 t2; moregen env u1 u2
    | (Ttuple tl1, Ttuple tl2) ->
        moregen_list env tl1 tl2
    | (Tconstr(p1, tl1, abbrev1), Tconstr(p2, tl2, abbrev2)) ->
        if Path.same p1 p2 then begin
          try
            t1.desc <- Tlink t2;
            moregen_list env tl1 tl2;
            t1.desc <- d1
          with Unify lst ->
          t1.desc <- d1;
          try
            moregen env (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
          with Cannot_expand ->
          try
            moregen env t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
          with Cannot_expand ->
            raise (Unify lst)
        end else begin
          try
            moregen env (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
          with Cannot_expand ->
          try
            moregen env t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
          with Cannot_expand ->
            raise (Unify [])
        end
    | (Tobject(f1, _), Tobject(f2, _)) ->
        t1.desc <- Tlink t2;
        moregen_fields env f1 f2
    | (Tconstr(p1, tl1, abbrev1), _) ->
        begin try
          moregen env (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
        with Cannot_expand ->
          raise (Unify [])
        end
    | (_, Tconstr(p2, tl2, abbrev2)) ->
        begin try
          moregen env t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
        with Cannot_expand ->
          raise (Unify [])
        end
    | (_, _) ->
        raise (Unify [])
    end
  with exn ->
    t1.desc <- d1;
    raise exn

and moregen_list env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (moregen env) tl1 tl2

and moregen_fields env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  if miss1 <> [] then raise (Unify []);
  begin match rest1.desc with
    Tvar ->
      if rest1.level = generic_level then raise (Unify []);
      let fi = build_fields miss2 rest2 in
      moregen_occur env rest1 fi
  | Tnil ->
      if miss2 <> [] then raise (Unify []);
      if rest2.desc <> Tnil then raise (Unify [])
  | _ ->
      fatal_error "moregen_fields"
  end;
  List.iter (fun (t1, t2) -> moregen env t1 t2) pairs

let moregeneral env sch1 sch2 =
  begin_def();
  try
    moregen env (instance sch1) sch2;
(*     remove_abbrev sch2; *)
    end_def();
    true
  with Unify _ ->
(*     remove_abbrev sch2; *)
    end_def();
    false


                 (*********************************************)
                 (*  Equivalence between parameterized types  *)
                 (*********************************************)


(* XXX A voir... *)

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
           (function (label', t') -> (label = label') & (eqtype t t'))
           fields2)
      fields1
  in
    let eq = eqtype ty1 ty2 in
(*     remove_abbrev ty1; remove_abbrev ty2; *)
    eq


                              (***************)
                              (*  Subtyping  *)
                              (***************)


(**** Build a subtype of a given type. ****)

let subtypes = ref []

(* XXX Types récursifs ? *)
let rec build_subtype env t =
  let t = repr t in
  match t.desc with
    Tlink t' ->                         (* Redundant ! *)
      build_subtype env t'
  | Tvar ->
      (t, false)
  | Tarrow(t1, t2) ->
      let (t1', c1) = (t1, false) in
      let (t2', c2) = build_subtype env t2 in
      if c1 or c2 then (new_global_ty (Tarrow(t1', t2')), true)
      else (t, false)
  | Ttuple tlist ->
      let (tlist', clist) =
        List.split (List.map (build_subtype env) tlist)
      in
      if List.exists (function c -> c) clist then
        (new_global_ty (Ttuple tlist'), true)
      else (t, false)
  | Tconstr(p, tl, abbrev) ->
      if generic_abbrev env p then begin
        let t' = expand_abbrev env p tl abbrev t.level in
        let (t'', c) = build_subtype env t' in
        if c then (t'', true)
        else (t, false)
      end else
        (t, false)
  | Tobject (t1, _) ->
      if opened_object t1 then
        (t, false)
      else
        (begin try
           List.assq t !subtypes
         with Not_found ->
           let t' = new_global_var () in
           subtypes := (t, t')::!subtypes;
           let (t1', _) = build_subtype env t1 in
           t'.desc <- Tobject (t1', ref None);
           t'
         end,
         true)
  | Tfield(s, t1, t2) ->
      let (t1', _) = build_subtype env t1 in
      let (t2', _) = build_subtype env t2 in
      (new_global_ty (Tfield(s, t1', t2')), true)
  | Tnil ->
      let v = new_global_var () in
      (v, true)

let enlarge_type env ty =
  subtypes := [];
  let (ty', _) = build_subtype env ty in
  subtypes := [];
  ty'

(**** Check whether a type is a subtype of another type. ****)

let subtypes = ref [];;

let subtype_error env trace =
  raise (Subtype (expand_trace env (List.rev trace), []))

let rec subtype_rec env trace t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then [] else
  if List.exists (fun (t1', t2') -> t1 == t1' & t2 == t2') !subtypes then
    []
  else begin
    subtypes := (t1, t2) :: !subtypes;
    match (t1.desc, t2.desc) with
      (Tvar, _) | (_, Tvar) ->
        [(trace, t1, t2)]
    | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
        (subtype_rec env ((t2, t1)::trace) t2 t1) @
        (subtype_rec env ((u1, u2)::trace) u1 u2)
    | (Ttuple tl1, Ttuple tl2) ->
        subtype_list env trace tl1 tl2
    | (Tconstr(p1, tl1, abbrev1), Tconstr(p2, tl2, abbrev2)) ->
        if generic_abbrev env p1 then
          subtype_rec env trace
            (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
        else if generic_abbrev env p2 then
          subtype_rec env trace t1
            (expand_abbrev env p2 tl2 abbrev2 t2.level)
        else
          [(trace, t1, t2)]
    | (Tconstr(p1, tl1, abbrev1), _) ->
        if generic_abbrev env p1 then
          subtype_rec env trace
            (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
        else
          [(trace, t1, t2)]
    | (_, Tconstr(p2, tl2, abbrev2)) ->
        if generic_abbrev env p2 then
          subtype_rec env trace t1
            (expand_abbrev env p2 tl2 abbrev2 t2.level)
        else
          [(trace, t1, t2)]
    | (Tobject (f1, _), Tobject (f2, _)) ->
        if opened_object f1 & opened_object f2 then
          [(trace, t1, t2)]
        else
          subtype_fields env trace f1 f2
    | (_, _) ->
        subtype_error env trace
  end

and subtype_list env trace tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    subtype_error env trace;
  List.fold_left2
    (fun cstrs t1 t2 -> cstrs @ (subtype_rec env ((t1, t2)::trace) t1 t2))
    [] tl1 tl2

and subtype_fields env trace ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1 in
  let (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  begin match rest1.desc with
    Tvar ->
      [(trace, rest1, build_fields miss2 (newvar ()))]
  | Tnil   -> if miss2 = [] then [] else subtype_error env trace
  | _      -> fatal_error "Ctype.subtype_fields (1)"
  end
    @
  begin match rest2.desc with
    Tvar ->
      [(trace, build_fields miss1 (rest1), rest2)]
  | Tnil   -> []
  | _      -> fatal_error "Ctype.subtype_fields (2)"
  end
    @
  (List.fold_left
     (fun cstrs (t1, t2) ->
        cstrs @ (subtype_rec env ((t1, t2)::trace) t1 t2))
     [] pairs)

let subtype env ty1 ty2 =
  subtypes := [];
  (* Build constraint set. *)
  let cstrs = subtype_rec env [(ty1, ty2)] ty1 ty2 in
  (* Enforce constraints. *)
  function () ->
    List.iter
      (function (trace0, t1, t2) ->
         try unify env t1 t2 with Unify trace ->
           raise (Subtype (expand_trace env (List.rev trace0),
                           List.tl (List.tl trace))))
      cstrs;
    subtypes := []


                              (*************************)
                              (*  Remove dependencies  *)
                              (*************************)


let inst_subst = ref ([] : (type_expr * type_expr) list)

(* XXX A voir... *)
let rec nondep_type_rec env id ty =
  let ty = repr ty in
  if ty.desc = Tvar then ty else
  try List.assq ty !inst_subst with Not_found ->
    let ty' = newgenvar () in
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


                              (******************)
                              (*  Type pruning  *)
                              (******************)


let inst_subst = ref ([] : (type_expr * type_expr) list)

(* XXX A voir... *)
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


                              (******************************)
                              (*  Abbreviation correctness  *)
                              (******************************)


exception Nonlinear_abbrev
exception Recursive_abbrev

let rec non_recursive_abbrev env path constrs ty =
  let ty = repr ty in
  match ty.desc with
    Tarrow (ty1, ty2) ->
      non_recursive_abbrev env path constrs ty1;
      non_recursive_abbrev env path constrs ty2
  | Ttuple tl ->
      List.iter (non_recursive_abbrev env path constrs) tl
  | Tconstr(p, args, abbrev) ->
      if Path.same path p then
        raise Recursive_abbrev
      else begin
        begin try
          let ty' = expand_abbrev env p args abbrev ty.level in
          if List.memq ty' constrs then () else
            non_recursive_abbrev env path (ty'::constrs) ty'
        with Cannot_expand ->
          ()
        end
      end
  | _ (* Tvar | Tobject (_, _) | Tfield (_, _, _) | Tnil *) ->
      ()

let rec path_assoc x =
  function
    [] -> raise Not_found
  | (a,b)::l -> if Path.same a x then b else path_assoc x l

let visited_abbrevs = ref []

let visited_abbrev p args =
  try
    let slot = path_assoc p !visited_abbrevs in
    if
      List.exists
        (function args' ->
           List.for_all2 (fun ty ty' -> repr ty == ty') args args')
        !slot
    then
      true
    else begin
      slot := (List.map repr args)::!slot;
      false
    end
  with Not_found ->
    visited_abbrevs := (p, ref [args])::!visited_abbrevs;
    false

let rec linear_abbrev env path params visited ty =
  let ty = repr ty in
  match ty.desc with
    Tarrow (ty1, ty2) ->
      linear_abbrev env path params visited ty1;
      linear_abbrev env path params visited ty2
  | Ttuple tl ->
      List.iter (linear_abbrev env path params visited) tl
  | Tconstr(p, args, abbrev) ->
      if Path.same p path then begin
        if
          List.exists (fun (ty1, ty2) -> repr ty1 != repr ty2)
            (List.combine params args)
        then
          raise Nonlinear_abbrev
      end else begin
        try
          let ty' = expand_abbrev env p args abbrev ty.level in
          if not (visited_abbrev p args) then
            linear_abbrev env path params visited ty'
        with Cannot_expand ->
        if not (List.memq ty visited) then begin
          List.iter
            (linear_abbrev env path params (ty::visited))
            args
        end
      end
  | Tobject (ty', _) ->
      if not (List.memq ty visited) then
        linear_abbrev env path params (ty::visited) ty'
  | Tfield(_, ty1, ty2) ->
      linear_abbrev env path params visited ty1;
      linear_abbrev env path params visited ty2
  | _ (* Tvar | Tnil *) ->
      ()

let correct_abbrev env ident params ty =
  let path = Path.Pident ident in
  non_recursive_abbrev env path [] ty;
  if params <> [] then begin
    visited_abbrevs := [];
    linear_abbrev env path params [] ty;
    visited_abbrevs := []
  end;
(*   remove_abbrev ty *)
()
                              (*******************)
                              (*  Miscellaneous  *)
                              (*******************)

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

type closed_schema_result = Var of type_expr | Row_var of type_expr
exception Failed of closed_schema_result

let visited = ref []

let rec closed_schema_rec ty =
  let ty = repr ty in
  if not (List.memq ty !visited) then begin
    visited := ty::!visited;
    match ty.desc with
      Tvar when ty.level != generic_level -> raise (Failed (Var ty))
    | Tobject(f, {contents = Some (_, p)}) ->
        begin try closed_schema_rec f with
          Failed (Row_var v) -> raise (Failed (Var v))
        | Failed (Var v) -> raise (Failed (Row_var v))
        end;
        List.iter closed_schema_rec p
    | Tobject(f, _) ->
        begin try closed_schema_rec f with
          Failed (Row_var v) -> raise (Failed (Var v))
        | Failed (Var v) -> raise (Failed (Row_var v))
        end
    | Tfield(_, t1, t2) ->
        begin try
          closed_schema_rec t1
        with
          Failed (Row_var v) -> raise (Failed (Var v))
        | Failed (Var v) -> raise (Failed (Row_var v))
        end;
        closed_schema_rec t2
    | _ ->
        iter_type_expr closed_schema_rec ty
  end

let closed_schema ty =
  visited := [];
  try
    closed_schema_rec ty;
    visited := [];
    true
  with Failed _ ->
    visited := [];
    false

let closed_schema_verbose ty =
  visited := [];
  try
    closed_schema_rec ty;
    visited := [];
    None
  with Failed status ->
    visited := [];
    Some status

let is_generic ty =
  let ty = repr ty in
  match ty.desc with
    Tvar -> ty.level = generic_level
  | _ -> fatal_error "Ctype.is_generic"

let rec arity ty =
  match (repr ty).desc with
    Tarrow(t1, t2) -> 1 + arity t2
  | _ -> 0
