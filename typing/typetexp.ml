(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
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
  | Type_mismatch of type_expr * type_expr

exception Error of Location.t * error

(* Translation of type expressions *)

let type_variables = ref (Tbl.empty : (string, type_expr) Tbl.t)
let aliases        = ref (Tbl.empty : (string, type_expr) Tbl.t)

let used_variables = ref (Tbl.empty : (string, type_expr) Tbl.t)
let bindings       = ref ([] : (type_expr * type_expr) list)
        (* These two variables are used for the "delayed" policy. *)

let reset_type_variables () =
  reset_global_level ();
  type_variables := Tbl.empty

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
    raise(Error(loc, Unbound_type_variable name))

type policy = Fixed | Extensible | Delayed

let rec transl_type env policy styp =
  match styp.ptyp_desc with
    Ptyp_var name ->
      begin try Tbl.find name !aliases with Not_found ->
        match policy with
          Fixed ->
            begin try
              Tbl.find name !type_variables
            with Not_found ->
              raise(Error(styp.ptyp_loc, Unbound_type_variable name))
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
              bindings := (v1, v2)::!bindings;
              v2
            with Not_found ->
              let v = new_global_var () in
              type_variables := Tbl.add name v !type_variables;
              used_variables := Tbl.add name v !used_variables;
              v
            end
      end
  | Ptyp_arrow(st1, st2) ->
      let ty1 = transl_type env policy st1 in
      let ty2 = transl_type env policy st2 in
        Ctype.newty (Tarrow(ty1, ty2))
  | Ptyp_tuple stl ->
      Ctype.newty (Ttuple(List.map (transl_type env policy) stl))
  | Ptyp_constr(lid, stl, alias) ->
      let (path, decl) =
        try
          Env.lookup_type lid env
        with Not_found ->
          raise(Error(styp.ptyp_loc, Unbound_type_constructor lid)) in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, Type_arity_mismatch(lid, decl.type_arity,
                                                           List.length stl)));
      let (cstr, params) =
        begin match alias with
      	  None ->
	    let tl = List.map (transl_type env policy) stl in
              (newty (Tconstr(path, tl, ref Mnil)), tl)
        | Some alias ->
            let cstr = newvar () in
            begin try
              Tbl.find alias !type_variables;
              raise(Error(styp.ptyp_loc, Bound_type_variable alias))
            with Not_found -> try
              Tbl.find alias !aliases;
              raise(Error(styp.ptyp_loc, Bound_type_variable alias))
            with Not_found ->
      	      aliases := Tbl.add alias cstr !aliases
	    end;
	    let tl = List.map (transl_type env policy) stl in
	    begin try
              occur env cstr
      	       	(Ctype.expand_abbrev env path tl (ref Mnil) cstr.level)
            with
	      Unify _       -> raise(Error(styp.ptyp_loc, Recursive_type))
	    | Cannot_expand -> ()
	    end;
	    cstr.desc <- Tconstr(path, tl, ref Mnil);
	    (cstr, tl)
        end
      in
      begin match decl.type_manifest with
      	Some _ ->
	  List.iter2
           (fun ty (sty, ty') ->
	      try Ctype.unify env (Ctype.instance ty) ty' with
		Unify _ ->
		  raise (Error(sty.ptyp_loc, Type_mismatch(ty, ty'))))
           decl.type_params (List.combine stl params)
      | _ ->
      	  ()
      end;
      cstr
  | Ptyp_object(fields, None) ->
      newobj (transl_fields env policy fields)
  | Ptyp_object(fields, Some alias) ->
      begin try
        Tbl.find alias !type_variables;
	raise(Error(styp.ptyp_loc, Bound_type_variable alias))
      with Not_found -> try
        Tbl.find alias !aliases;
	raise(Error(styp.ptyp_loc, Bound_type_variable alias))
      with Not_found ->
        let obj = newvar () in
      	  aliases := Tbl.add alias obj !aliases;
	  obj.desc <- Tobject (transl_fields env policy fields, ref None);
	  obj
      end
  | Ptyp_class(lid, stl, alias) ->
      if policy = Fixed then
        raise(Error(styp.ptyp_loc, Unbound_row_variable lid));
      let lid2 =
        match lid with
          Longident.Lident s     -> Longident.Lident ("#" ^ s)
	| Longident.Ldot(r, s)   -> Longident.Ldot (r, "#" ^ s)
	| Longident.Lapply(_, _) -> fatal_error "Typetexp.transl_type"
      in
      let (path, decl) =
        try
          Env.lookup_type lid2 env
        with Not_found ->
          raise(Error(styp.ptyp_loc, Unbound_class lid)) in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, Type_arity_mismatch(lid, decl.type_arity,
                                                           List.length stl)));
      let v = new_global_var () in
      let (ty, params) =
        begin match alias with
      	  None ->
	    let tl = List.map (transl_type env policy) stl in
	    (expand_abbrev env path tl (ref Mnil) v.level, tl)
        | Some alias ->
            begin try
              Tbl.find alias !type_variables;
	      raise(Error(styp.ptyp_loc, Bound_type_variable alias))
            with Not_found -> try
              Tbl.find alias !aliases;
              raise(Error(styp.ptyp_loc, Bound_type_variable alias))
            with Not_found ->
      	      aliases := Tbl.add alias v !aliases
	    end;
	    let tl = List.map (transl_type env policy) stl in
	    let cstr = expand_abbrev env path tl (ref Mnil) v.level in
	    v.desc <- Tlink cstr;
	    (v, tl)
        end
      in
      begin match decl.type_manifest with
      	Some _ ->
	  List.iter2
           (fun ty (sty, ty') ->
	      try Ctype.unify env (Ctype.instance ty) ty' with
		Unify _ ->
		  raise (Error(sty.ptyp_loc, Type_mismatch(ty, ty'))))
           decl.type_params (List.combine stl params)
      | _ ->
      	  ()
      end;
      ty

and transl_fields env policy =
  function
    [] ->
      newty Tnil
  | {pfield_desc = Pfield_var} as field::_ ->
      if policy = Fixed then
        raise(Error(field.pfield_loc, Unbound_type_variable ".."));
      newvar ()
  | {pfield_desc = Pfield(s, e)}::l ->
      let ty1 = transl_type env policy e in
      let ty2 = transl_fields env policy l in
	newty (Tfield (s, ty1, ty2))

let transl_simple_type env fixed styp =
  aliases := Tbl.empty;
  let typ = transl_type env (if fixed then Fixed else Extensible) styp in
  aliases := Tbl.empty;
  typ

let transl_simple_type_delayed env styp =
  aliases := Tbl.empty;
  used_variables := Tbl.empty;
  bindings := [];
  let typ = transl_type env Delayed styp in
  let b = !bindings in
  aliases := Tbl.empty;
  used_variables := Tbl.empty;
  bindings := [];
  (typ,
   function () -> List.iter (function (t1, t2) -> Ctype.unify env t1 t2) b)

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

let report_error = function
    Unbound_type_variable name ->
      print_string "Unbound type parameter "; print_string name
  | Unbound_type_constructor lid ->
      print_string "Unbound type constructor "; longident lid
  | Type_arity_mismatch(lid, expected, provided) ->
      open_hovbox 0;
      print_string "The type constructor "; longident lid;
      print_space(); print_string "expects "; print_int expected;
      print_string " argument(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " argument(s)";
      close_box()
  | Bound_type_variable name ->
      print_string "Already bound type parameter "; print_string name
  | Recursive_type ->
      print_string "This type is recursive"
  | Unbound_class lid ->
      print_string "Unbound class "; longident lid
  | Unbound_row_variable lid ->
      print_string "Unbound row variable in #"; longident lid
  | Type_mismatch (ty, ty') ->
      Printtyp.reset ();
      Printtyp.mark_loops ty; Printtyp.mark_loops ty';
      open_hovbox 0;
      print_string "This parameter ";
      Printtyp.type_expr ty';
      print_string " should be an instance of ";
      Printtyp.type_expr ty
