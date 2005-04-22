(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* typetexp.ml,v 1.34.4.9 2002/01/07 08:39:16 garrigue Exp *)

(* Typechecking of type expressions for the core language *)

open Misc
open Parsetree
open Types
open Ctype

exception Already_bound

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Longident.t
  | Unbound_type_constructor_2 of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_class of Longident.t
  | Unbound_row_variable of Longident.t
  | Type_mismatch of (type_expr * type_expr) list
  | Alias_type_mismatch of (type_expr * type_expr) list
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * type_expr
  | Cannot_have_etype
  | Cannot_have_lident

exception Error of Location.t * error

type variable_context = int * (string, type_expr) Tbl.t

(* Translation of type expressions *)

let type_variables = ref (Tbl.empty : (string, type_expr) Tbl.t)
let univars        = ref ([] : (string * type_expr) list)
let pre_univars    = ref ([] : type_expr list)
let local_aliases  = ref ([] : string list)
let generic_loop_vars = ref ([] : (string * type_expr) list)

let used_variables = ref (Tbl.empty : (string, type_expr) Tbl.t)
let bindings       = ref ([] : (Location.t * type_expr * type_expr) list)
  (* These two variables are used for the "delayed" policy. *)

let reset_pre_univars () =
  pre_univars := [];
  local_aliases := []

let reset_type_variables () =
  reset_global_level ();
  type_variables := Tbl.empty

let narrow () =
  (increase_global_level (), !type_variables)

let widen (gl, tv) =
  restore_global_level gl;
  type_variables := tv

let enter_type_variable strict loc name =
  try
    if name <> "" && name.[0] = '_' then
      raise (Error (loc, Invalid_variable_name ("'" ^ name)));
    let v = Tbl.find name !type_variables in
    if strict then raise Already_bound;
    v
  with Not_found ->
    let v = new_global_var() in
    type_variables := Tbl.add name v !type_variables;
    v

let type_variable loc name =
  try
    Tbl.find name !type_variables
  with Not_found ->
    raise(Error(loc, Unbound_type_variable ("'" ^ name)))

let wrap_method ty =
  match (Ctype.repr ty).desc with
    Tpoly _ -> ty
  | _ -> Ctype.newty (Tpoly (ty, []))

let new_pre_univar () =
  let v = newvar () in pre_univars := v :: !pre_univars; v

let rec swap_list = function
    x :: y :: l -> y :: x :: swap_list l
  | l -> l

type policy = Fixed | Extensible | Delayed | Univars

let reject_lident loc lid = raise (Error(loc, Cannot_have_lident))

let rec transl_type env ?(lident=reject_lident) policy styp =
  match styp.ptyp_desc with
    Ptyp_any ->
      if policy = Univars then new_pre_univar () else newvar ()
  | Ptyp_var name ->
      if name <> "" && name.[0] = '_' then
        raise (Error (styp.ptyp_loc, Invalid_variable_name ("'" ^ name)));
      begin try
	List.assoc name !generic_loop_vars
      with Not_found -> try
        instance (List.assoc name !univars)
      with Not_found ->
        match policy with
          Fixed ->
            begin try
              instance (Tbl.find name !type_variables)
            with Not_found ->
              raise(Error(styp.ptyp_loc, Unbound_type_variable ("'" ^ name)))
            end
        | Extensible ->
            begin try
              instance (Tbl.find name !type_variables)
            with Not_found ->
              let v = new_global_var () in
              type_variables := Tbl.add name v !type_variables;
              v
            end
        | Univars ->
            begin try
              instance (Tbl.find name !type_variables)
            with Not_found ->
              let v = new_pre_univar () in
              type_variables := Tbl.add name v !type_variables;
              local_aliases := name :: !local_aliases;
              v
            end
        | Delayed ->
            begin try
              instance (Tbl.find name !used_variables)
            with Not_found -> try
              let v1 = instance (Tbl.find name !type_variables) in
              let v2 = new_global_var () in
              used_variables := Tbl.add name v2 !used_variables;
              bindings := (styp.ptyp_loc, v1, v2)::!bindings;
              v2
            with Not_found ->
              let v = new_global_var () in
              type_variables := Tbl.add name v !type_variables;
              used_variables := Tbl.add name v !used_variables;
              v
            end
      end
  | Ptyp_arrow(l, st1, st2) ->
      let ty1 = transl_type env ~lident policy st1 in
      let ty2 = transl_type env ~lident policy st2 in
      newty (Tarrow(l, ty1, ty2, Cok))
  | Ptyp_tuple stl ->
      newty (Ttuple(List.map (transl_type env ~lident policy) stl))
  | Ptyp_constr(lid, stl) ->
      let (path, decl) =
        try
          Env.lookup_type lid env
        with Not_found ->
          raise(Error(styp.ptyp_loc, Unbound_type_constructor lid)) in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, Type_arity_mismatch(lid, decl.type_arity,
                                                           List.length stl)));
      let args = List.map (transl_type env ~lident policy) stl in
      let params = Ctype.instance_list decl.type_params in
      let unify_param =
        match decl.type_manifest with
          None -> unify_var
        | Some ty ->
            if (repr ty).level = Btype.generic_level then unify_var else unify
      in
      List.iter2
        (fun (sty, ty) ty' ->
           try unify_param env ty' ty with Unify trace ->
             raise (Error(sty.ptyp_loc, Type_mismatch (swap_list trace))))
        (List.combine stl args) params;
      let constr = newconstr path args in
      begin try
        Ctype.enforce_constraints env constr
      with Unify trace ->
        raise (Error(styp.ptyp_loc, Type_mismatch trace))
      end;
      constr
  | Ptyp_object fields ->
      newobj (transl_fields env ~lident policy fields)
  | Ptyp_class(lid, stl, present) ->
      let (path, decl, is_variant) =
        try
          let (path, decl) = Env.lookup_type lid env in
          let rec check decl =
            match decl.type_manifest with
              None -> raise Not_found
            | Some ty ->
                match (repr ty).desc with
                  Tvariant row when Btype.static_row row -> ()
                | Tconstr (path, _, _) ->
                    check (Env.find_type path env)
                | _ -> raise Not_found
          in check decl;
          Location.prerr_warning styp.ptyp_loc Warnings.Deprecated;
          (path, decl,true)
        with Not_found -> try
          if present <> [] then raise Not_found;
          let lid2 =
            match lid with
              Longident.Lident s     -> Longident.Lident ("#" ^ s)
            | Longident.Ldot(r, s)   -> Longident.Ldot (r, "#" ^ s)
            | Longident.Lapply(_, _) -> fatal_error "Typetexp.transl_type"
          in
          let (path, decl) = Env.lookup_type lid2 env in
          (path, decl, false)
        with Not_found ->
          raise(Error(styp.ptyp_loc, Unbound_class lid))
      in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, Type_arity_mismatch(lid, decl.type_arity,
                                                       List.length stl)));
      let args = List.map (transl_type env ~lident policy) stl in
      let params = Ctype.instance_list decl.type_params in
      List.iter2
        (fun (sty, ty) ty' ->
           try unify_var env ty' ty with Unify trace ->
             raise (Error(sty.ptyp_loc, Type_mismatch (swap_list trace))))
        (List.combine stl args) params;
      let ty =
        try Ctype.expand_head env (newconstr path args)
        with Unify trace ->
          raise (Error(styp.ptyp_loc, Type_mismatch trace))
      in
      begin match ty.desc with
        Tvariant row ->
          let row = Btype.row_repr row in
          List.iter
            (fun l -> if not (List.mem_assoc l row.row_fields) then
              raise(Error(styp.ptyp_loc, Present_has_no_type l)))
            present;
          let bound = ref row.row_bound in
          let single = List.length row.row_fields = 1 in
          let fields =
            if single then row.row_fields else
            List.map
              (fun (l,f) -> l,
                if List.mem l present then f else
                match Btype.row_field_repr f with
                | Rpresent (Some ty) ->
                    bound := ty :: !bound;
                    Reither(false, [ty], false, ref None)
                | Rpresent None ->
                    Reither (true, [], false, ref None)
                | _ -> f)
              row.row_fields
          in
          let row = { row_closed = true; row_fields = fields;
                      row_bound = !bound; row_name = Some (path, args);
                      row_fixed = false; row_more = newvar () } in
          let static = Btype.static_row row in
          let row =
            if static then row else
            { row with row_more =
                if policy = Univars then new_pre_univar () else newvar () }
          in
          newty (Tvariant row)
      | Tobject (fi, _) ->
          let _, tv = flatten_fields fi in
          if policy = Univars then pre_univars := tv :: !pre_univars;
          ty
      | _ ->
          assert false
      end
  | Ptyp_alias(st, alias) ->
      begin
        try
          let t =
            try List.assoc alias !univars
            with Not_found ->
              let v1 = instance ( Tbl.find alias !type_variables) in
              (* Special case if using indirect variable bindings *)
              if policy = Delayed then
                try instance (Tbl.find alias !used_variables)
                with Not_found ->
                  let v2 = new_global_var () in
                  used_variables := Tbl.add alias v2 !used_variables;
                  bindings := (styp.ptyp_loc, v1, v2)::!bindings;
                  v2
              else v1
          in
          let ty = transl_type env ~lident policy st in
          begin try unify_var env t ty with Unify trace ->
            let trace = swap_list trace in
            raise(Error(styp.ptyp_loc, Alias_type_mismatch trace))
          end;
          ty
        with Not_found ->
          begin_def ();
          let t = newvar () in
          type_variables := Tbl.add alias t !type_variables;
          let local = (policy = Univars || !univars <> []) in
          if local then local_aliases := alias :: !local_aliases;
          if policy = Delayed then
            used_variables := Tbl.add alias t !used_variables;
          let ty = transl_type env ~lident policy st in
          begin try unify_var env t ty with Unify trace ->
            let trace = swap_list trace in
            raise(Error(styp.ptyp_loc, Alias_type_mismatch trace))
          end;
          end_def ();
          if local then generalize_structure t
          else generalize_global t;
          instance t
      end
  | Ptyp_variant(fields, closed, present) ->
      let bound = ref [] and name = ref None in
      let mkfield l f =
        newty (Tvariant {row_fields=[l,f]; row_more=newvar();
                         row_bound=[]; row_closed=true;
                         row_fixed=false; row_name=None}) in
      let add_typed_field loc l f fields =
        try
          let f' = List.assoc l fields in
          let ty = mkfield l f and ty' = mkfield l f' in
          if equal env false [ty] [ty'] then fields
          else raise(Error(loc, Constructor_mismatch (ty,ty')))
        with Not_found ->
          (l, f) :: fields
      in
      (* closed and only one field: make it present anyway *)
      let single = closed && List.length fields = 1 in
      let rec add_field fields = function
          Rtag (l, c, stl) ->
            name := None;
            let f = match present with
              Some present when not (single || List.mem l present) ->
                let tl = List.map (transl_type env ~lident policy) stl in
                bound := tl @ !bound;
                Reither(c, tl, false, ref None)
            | _ ->
                if List.length stl > 1 || c && stl <> [] then
                  raise(Error(styp.ptyp_loc, Present_has_conjunction l));
                match stl with [] -> Rpresent None
                | st :: _ -> Rpresent (Some(transl_type env ~lident policy st))
            in
            add_typed_field styp.ptyp_loc l f fields
        | Rinherit sty ->
            let ty = transl_type env ~lident policy sty in
            let nm =
              match repr ty with
                {desc=Tconstr(p, tl, _)} -> Some(p, tl)
              | _                        -> None
            in
            name := if fields = [] then nm else None;
            let fl = match expand_head env ty, nm with
              {desc=Tvariant row}, _ when Btype.static_row row ->
                let row = Btype.row_repr row in
                row.row_fields
            | {desc=Tvar}, Some(p, _) ->
                raise(Error(sty.ptyp_loc, Unbound_type_constructor_2 p)) 
            | _ ->
                raise(Error(sty.ptyp_loc, Not_a_variant ty))
            in
            let single = single && List.length fl = 1 in
            List.fold_left
              (fun fields (l, f) ->
                let f = match present with
                  Some present when not (single || List.mem l present) ->
                    begin match f with
                      Rpresent(Some ty) ->
                        bound := ty :: !bound;
                        Reither(false, [ty], false, ref None)
                    | Rpresent None ->
                        Reither(true, [], false, ref None)
                    | _ ->
                        assert false
                    end
                | _ -> f
                in
                add_typed_field sty.ptyp_loc l f fields)
              fields fl
      in
      let fields = List.fold_left add_field [] fields in
      begin match present with None -> ()
      | Some present ->
          List.iter
            (fun l -> if not (List.mem_assoc l fields) then
              raise(Error(styp.ptyp_loc, Present_has_no_type l)))
            present
      end;
      ignore begin
        List.fold_left
          (fun hl (l,_) ->
            let h = Btype.hash_variant l in
            try
              let l' = List.assoc h hl in
              if l <> l' then raise(Error(styp.ptyp_loc, Variant_tags(l, l')));
              hl
            with Not_found -> (h,l) :: hl)
          []
          fields
      end;
      let row =
        { row_fields = List.rev fields; row_more = newvar ();
          row_bound = !bound; row_closed = closed;
          row_fixed = false; row_name = !name } in
      let static = Btype.static_row row in
      let row =
        if static then row else
        { row with row_more =
            if policy = Univars then new_pre_univar () else
            if policy = Fixed && not static then
              raise(Error(styp.ptyp_loc, Unbound_type_variable "[..]"))
            else row.row_more
        } in
      newty (Tvariant row)
  | Ptyp_poly(vars, st) ->
      begin_def();
      let new_univars = List.map (fun name -> name, newvar()) vars in
      let old_univars = !univars in
      univars := new_univars @ !univars;
      let ty = transl_type env ~lident policy st in
      univars := old_univars;
      end_def();
      generalize ty;
      let ty_list =
        List.fold_left
          (fun tyl (name, ty1) ->
            let v = Btype.proxy ty1 in
            if deep_occur v ty then begin
              if v.level <> Btype.generic_level || v.desc <> Tvar then
                raise (Error (styp.ptyp_loc, Cannot_quantify (name, v)));
              v.desc <- Tunivar;
              v :: tyl
            end else tyl)
          [] new_univars
      in
      let ty' = Btype.newgenty (Tpoly(ty, List.rev ty_list)) in
      unify_var env (newvar()) ty';
      ty'
  | Ptyp_konst(_,_) (* not allowed *)
  | Ptyp_overload(_) (* not allowed *) ->
      raise (Error(styp.ptyp_loc, Cannot_have_etype))
  | Ptyp_lident li -> lident styp.ptyp_loc li

and transl_fields env ?(lident=reject_lident) policy =
  function
    [] ->
      newty Tnil
  | {pfield_desc = Pfield_var}::_ ->
      if policy = Univars then new_pre_univar () else newvar ()
  | {pfield_desc = Pfield(s, e)}::l ->
      let ty1 = transl_type env ~lident policy e in
      let ty2 = transl_fields env ~lident policy l in
        newty (Tfield (s, Fpresent, ty1, ty2))


(* Make the rows "fixed" in this type, to make universal check easier *)
let rec make_fixed_univars ty =
  let ty = repr ty in
  if ty.level >= Btype.lowest_level then begin
    Btype.mark_type_node ty;
    match ty.desc with
    | Tvariant row ->
        let row = Btype.row_repr row in
        if (Btype.row_more row).desc = Tunivar then
          ty.desc <- Tvariant
              {row with row_fixed=true;
               row_fields = List.map
                 (fun (s,f as p) -> match Btype.row_field_repr f with
                   Reither (c, tl, m, r) -> s, Reither (c, tl, true, r)
                 | _ -> p)
                 row.row_fields};
        Btype.iter_row make_fixed_univars row
    | _ ->
        Btype.iter_type_expr make_fixed_univars ty
  end

let make_fixed_univars ty =
  make_fixed_univars ty;
  Btype.unmark_type ty

let transl_simple_type env fixed styp =
  univars := []; local_aliases := [];
  let typ = transl_type env (if fixed then Fixed else Extensible) styp in
  type_variables := List.fold_right Tbl.remove !local_aliases !type_variables;
  make_fixed_univars typ;
  typ

let transl_simple_type_univars env styp =
  univars := [];
  reset_pre_univars ();
  begin_def ();
  let typ = transl_type env Univars styp in
  end_def ();
  generalize typ;
  let univs =
    List.fold_left
      (fun acc v ->
        let v = repr v in
        if v.level <> Btype.generic_level || v.desc <> Tvar then acc
        else (v.desc <- Tunivar ; v :: acc))
      [] !pre_univars
  in
  type_variables := List.fold_right Tbl.remove !local_aliases !type_variables;
  reset_pre_univars ();
  make_fixed_univars typ;
  instance (Btype.newgenty (Tpoly (typ, univs)))

let transl_simple_type_delayed env styp =
  univars := [];
  used_variables := Tbl.empty;
  bindings := [];
  let typ = transl_type env Delayed styp in
  let b = !bindings in
  used_variables := Tbl.empty;
  bindings := [];
  (typ,
   function () ->
     List.iter
       (function (loc, t1, t2) ->
          try unify env t1 t2 with Unify trace ->
            raise (Error(loc, Type_mismatch trace)))
       b)

let transl_type_scheme env sktyp =
  let rec transl_type_scheme env sktyp =
    let type_variables_escaped = !type_variables in
    reset_type_variables ();
    let res = transl_type_scheme_sub env sktyp in
    type_variables := type_variables_escaped;
    res

  and transl_type_scheme_sub env sktyp =
    begin_def();
    let t =
      match sktyp.ptyp_desc with
      | Ptyp_konst (k, t) ->
  	  let konst = List.map (fun (kt,kd) -> 
  	    {ktype= transl_simple_type env false kt;
  	     kdepend= begin
  	       match kd with
  	       | Some t -> Some (transl_type_scheme env t)
  	       | None -> None
  	     end }) k 
  	  in
  	  let ty = transl_simple_type env false t in
  	  let konst = Gtype.normalize_konst_type konst ty in
  	  newty (Tkonst(konst, ty))
      | Ptyp_overload typs ->
  	  (* FIXME: vars are reset... *)
  	  Gtype.make_toverload env (List.map (transl_type_scheme env) typs) 
      | Ptyp_alias(({ptyp_desc= Ptyp_overload typs} as st), alias) ->
  	  begin
            try
  	    (* we have already a binding! *)
  	    let v1 = 
	      try List.assoc alias !generic_loop_vars with Not_found ->
		Tbl.find alias !type_variables 
	    in
  	    let ty = transl_type_scheme env st in
  	    if Etype.compare_types v1 ty <> 0 then
  	      raise(Error(sktyp.ptyp_loc, Alias_type_mismatch [v1,ty]));
              ty
            with Not_found ->
  	      (* first time of  binding *)
  	      let t = Btype.newgenvar () in
(*
Format.eprintf "Added alias %s@." alias;
*)
	      generic_loop_vars := (alias,t) :: !generic_loop_vars;
              let ty = transl_type_scheme env st in
  	      t.desc <- Tlink ty;
  	      ty
  	  end
      | _ -> 
  	transl_simple_type env false sktyp
    in
    end_def();
    generalize t;
    t
  in
  generic_loop_vars := [];
  transl_type_scheme env sktyp


let escape_states f v =
  (* push *)
  let tv = !type_variables
  and pu = !pre_univars
  and uv = !used_variables
  and bs = !bindings
  in
  type_variables := Tbl.empty;
  pre_univars := [];
  used_variables := Tbl.empty;
  bindings := [];
  let pop () =
    type_variables := tv;
    pre_univars := pu;
    used_variables := uv;
    bindings := bs
  in
  try 
    let r = f v in pop (); r
  with
  | e -> pop (); raise e

let transl_run_time_type f env styp =
  escape_states (fun () ->
    let typ = transl_type env ~lident: f Extensible styp in
    type_variables := 
      List.fold_right Tbl.remove !local_aliases !type_variables;
    make_fixed_univars typ;
    typ) ()

(* Error report *)

open Format
open Printtyp

let report_error ppf = function
  | Unbound_type_variable name ->
      fprintf ppf "Unbound type parameter %s" name
  | Unbound_type_constructor lid ->
      fprintf ppf "Unbound type constructor %a" longident lid
  | Unbound_type_constructor_2 p ->
      fprintf ppf "The type constructor@ %a@ is not yet completely defined"
        path p
  | Type_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
       "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
       longident lid expected provided
  | Bound_type_variable name ->
      fprintf ppf "Already bound type parameter '%s" name
  | Recursive_type ->
      fprintf ppf "This type is recursive"
  | Unbound_class lid ->
      fprintf ppf "Unbound class %a" longident lid
  | Unbound_row_variable lid ->
      fprintf ppf "Unbound row variable in #%a" longident lid
  | Type_mismatch trace ->
      Printtyp.unification_error true trace
        (function ppf ->
           fprintf ppf "This type")
        ppf
        (function ppf ->
           fprintf ppf "should be an instance of type")
  | Alias_type_mismatch trace ->
      Printtyp.unification_error true trace
        (function ppf ->
           fprintf ppf "This alias is bound to type")
        ppf
        (function ppf ->
           fprintf ppf "but is used as an instance of type")
  | Present_has_conjunction l ->
      fprintf ppf "The present constructor %s has a conjunctive type" l
  | Present_has_no_type l ->
      fprintf ppf "The present constructor %s has no type" l
  | Constructor_mismatch (ty, ty') ->
      Printtyp.reset_and_mark_loops_list [ty; ty'];
      fprintf ppf "@[<hov>%s %a@ %s@ %a@]"
        "This variant type contains a constructor"
        Printtyp.type_expr ty
        "which should be"
        Printtyp.type_expr ty'
  | Not_a_variant ty ->
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf "@[The type %a@ is not a polymorphic variant type@]"
        Printtyp.type_expr ty
  | Variant_tags (lab1, lab2) ->
      fprintf ppf
        "Variant tags `%s@ and `%s have same hash value.@ Change one of them."
        lab1 lab2
  | Invalid_variable_name name ->
      fprintf ppf "The type variable name %s is not allowed in programs" name
  | Cannot_quantify (name, v) ->
      fprintf ppf "This type scheme cannot quantify '%s :@ %s." name
        (if v.desc = Tvar then "it escapes this scope" else
         if v.desc = Tunivar then "it is aliased to another variable"
         else "it is not a variable")
  | Cannot_have_etype ->
      fprintf ppf "This overloaded type is not allowed as a type constraint."
  | Cannot_have_lident ->
      fprintf ppf "This expression is only permitted inside run time types"
