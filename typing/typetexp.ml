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

(* $Id$ *)

(* Typechecking of type expressions for the core language *)

open Misc
open Parsetree
open Types
open Ctype

exception Already_bound

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Longident.t
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

exception Error of Location.t * error

(* Translation of type expressions *)

let type_variables = ref (Tbl.empty : (string, type_expr) Tbl.t)
let saved_type_variables = ref ([] : (string, type_expr) Tbl.t list)

let used_variables = ref (Tbl.empty : (string, type_expr) Tbl.t)
let bindings       = ref ([] : (Location.t * type_expr * type_expr) list)
        (* These two variables are used for the "delayed" policy. *)

let reset_type_variables () =
  reset_global_level ();
  type_variables := Tbl.empty;
  saved_type_variables := []

let narrow () =
  increase_global_level ();
  saved_type_variables := !type_variables :: !saved_type_variables

let widen () =
  restore_global_level ();
  match !saved_type_variables with
    tv :: rem -> type_variables := tv; saved_type_variables := rem
  | []        -> assert false

let enter_type_variable strict name =
  try
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

type policy = Fixed | Extensible | Delayed

let rec transl_type env policy styp =
  match styp.ptyp_desc with
    Ptyp_any -> Ctype.newvar ()
  | Ptyp_var name ->
      begin
        match policy with
          Fixed ->
            begin try
              Tbl.find name !type_variables
            with Not_found ->
              raise(Error(styp.ptyp_loc, Unbound_type_variable ("'" ^ name)))
            end
        | Extensible ->
            begin try
              Tbl.find name !type_variables
            with Not_found ->
              let v = new_global_var () in
              type_variables := Tbl.add name v !type_variables;
              v
            end
        | Delayed ->
            begin try
              Tbl.find name !used_variables
            with Not_found -> try
              let v1 = Tbl.find name !type_variables in
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
      let ty1 = transl_type env policy st1 in
      let ty2 = transl_type env policy st2 in
      newty (Tarrow(l, ty1, ty2, Cok))
  | Ptyp_tuple stl ->
      newty (Ttuple(List.map (transl_type env policy) stl))
  | Ptyp_constr(lid, stl) ->
      let (path, decl) =
        try
          Env.lookup_type lid env
        with Not_found ->
          raise(Error(styp.ptyp_loc, Unbound_type_constructor lid)) in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, Type_arity_mismatch(lid, decl.type_arity,
                                                           List.length stl)));
      let args = List.map (transl_type env policy) stl in
      let params = List.map (fun _ -> Ctype.newvar ()) args in
      let cstr = newty (Tconstr(path, params, ref Mnil)) in
      begin try
        Ctype.enforce_constraints env cstr
      with Unify trace ->
        raise (Error(styp.ptyp_loc, Type_mismatch trace))
      end;
      List.iter2
        (fun (sty, ty) ty' ->
           try unify env ty ty' with Unify trace ->
             raise (Error(sty.ptyp_loc, Type_mismatch trace)))
        (List.combine stl args) params;
      cstr
  | Ptyp_object fields ->
      newobj (transl_fields env policy fields)
  | Ptyp_class(lid, stl, present) ->
      if policy = Fixed then
        raise(Error(styp.ptyp_loc, Unbound_row_variable lid));
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
      let args = List.map (transl_type env policy) stl in
      let cstr = newty (Tconstr(path, args, ref Mnil)) in
      let ty =
        try Ctype.expand_head env cstr
        with Unify trace ->
          raise (Error(styp.ptyp_loc, Type_mismatch trace))
      in
      let params = Ctype.instance_list decl.type_params in
      List.iter2
        (fun (sty, ty') ty ->
           try unify env ty' ty with Unify trace ->
             raise (Error(sty.ptyp_loc, Type_mismatch trace)))
        (List.combine stl args) params;
      begin match ty.desc with
        Tvariant row ->
          let row = Btype.row_repr row in
          List.iter
            (fun l -> if not (List.mem_assoc l row.row_fields) then
              raise(Error(styp.ptyp_loc, Present_has_no_type l)))
            present;
          let bound = ref row.row_bound in
          let fields =
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
          let row = { row with row_fields = fields; row_bound = !bound;
                      row_name = Some (path, args) } in
          newty (Tvariant row)
      | _ ->
          ty
      end
  | Ptyp_alias(st, alias) ->
      if Tbl.mem alias !type_variables then
        raise(Error(styp.ptyp_loc, Bound_type_variable alias))
      else
        let ty' = new_global_var () in
        type_variables := Tbl.add alias ty' !type_variables;
        let ty = transl_type env policy st in
        begin try unify env ty ty' with Unify trace ->
          raise(Error(styp.ptyp_loc, Alias_type_mismatch trace))
        end;
        ty
  | Ptyp_variant(fields, closed, present) ->
      let bound = ref [] and name = ref None in
      let mkfield l f =
        newty (Tvariant {row_fields=[l,f]; row_more=newty Tnil;
                         row_bound=[]; row_closed=true; row_name=None}) in
      let add_typed_field loc l f fields =
        try
          let f' = List.assoc l fields in
          let ty = mkfield l f and ty' = mkfield l f' in
          if equal env false [ty] [ty'] then fields
          else raise(Error(loc, Constructor_mismatch (ty,ty')))
        with Not_found ->
          (l, f) :: fields
      in
      let rec add_field fields = function
          Rtag (l, c, stl) ->
            name := None;
            let f = match present with
              Some present when not (List.mem l present) ->
                let tl = List.map (transl_type env policy) stl in
                bound := tl @ !bound;
                Reither(c, tl, false, ref None)
            | _ ->
                if List.length stl > 1 || c && stl <> [] then
                  raise(Error(styp.ptyp_loc, Present_has_conjunction l));
                match stl with [] -> Rpresent None
                | st :: _ -> Rpresent (Some(transl_type env policy st))
            in
            add_typed_field styp.ptyp_loc l f fields
        | Rinherit sty ->
            let ty = transl_type env policy sty in
            if fields <> [] then name := None else begin
              match repr ty with
                {desc=Tconstr(p, tl, _)} -> name := Some(p, tl)
              | _                        -> ()
            end;
            let fl = match expand_head env ty with
              {desc=Tvariant row} when Btype.static_row row ->
                let row = Btype.row_repr row in
                row.row_fields
            | _ -> raise(Error(sty.ptyp_loc, Not_a_variant ty))
            in
            List.fold_left
              (fun fields (l, f) ->
                let f = match present with
                  Some present when not (List.mem l present) ->
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
              if l <> l' then raise(Ctype.Tags(l, l'));
              hl
            with Not_found -> (h,l) :: hl)
          []
          fields
      end;
      let row =
        { row_fields = List.rev fields; row_more = newvar ();
          row_bound = !bound; row_closed = closed; row_name = !name } in
      if policy = Fixed && not (Btype.static_row row) then
        raise(Error(styp.ptyp_loc, Unbound_type_variable "[..]"));
      newty (Tvariant row)

and transl_fields env policy =
  function
    [] ->
      newty Tnil
  | {pfield_desc = Pfield_var} as field::_ ->
      if policy = Fixed then
        raise(Error(field.pfield_loc, Unbound_type_variable "<..>"));
      newvar ()
  | {pfield_desc = Pfield(s, e)}::l ->
      let ty1 = transl_type env policy e in
      let ty2 = transl_fields env policy l in
        newty (Tfield (s, Fpresent, ty1, ty2))

let transl_simple_type env fixed styp =
  let typ = transl_type env (if fixed then Fixed else Extensible) styp in
  typ

let transl_simple_type_delayed env styp =
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

let transl_type_scheme env styp =
  reset_type_variables();
  begin_def();
  let typ = transl_simple_type env false styp in
  end_def();
  generalize typ;
  typ

(* Error report *)

open Format
open Printtyp

let report_error ppf = function
  | Unbound_type_variable name ->
      fprintf ppf "Unbound type parameter %s" name
  | Unbound_type_constructor lid ->
      fprintf ppf "Unbound type constructor %a" longident lid
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
