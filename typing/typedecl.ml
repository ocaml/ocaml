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

(**** Typing of type definitions ****)

open Misc
open Parsetree
open Types
open Typedtree
open Typetexp

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string
  | Definition_mismatch of type_expr
  | Unconsistent_constraint
  | Type_clash of (type_expr * type_expr) list

exception Error of Location.t * error

(* Enter all declared types in the environment as abstract types *)

let rec enter_types env = function
    ([], []) ->
      (env, [])
  | ((name, sdecl) :: srem, id :: irem) ->
      let decl =
        { type_params = List.map (fun _ -> Ctype.newvar ()) sdecl.ptype_params;
          type_arity = List.length sdecl.ptype_params;
          type_kind = Type_abstract;
          type_manifest =
            match sdecl.ptype_manifest with
              None -> None
            | Some _ -> Some (Ctype.newvar ()) }
      in
      let extenv = Env.add_type id decl env in
      let (ext_env, decl_rem) = enter_types extenv (srem, irem) in
      (ext_env, (id, decl) :: decl_rem)
  | _ ->
      fatal_error "Typedecl.enter_types"

(* Translate one type declaration *)

module StringSet =
  Set.Make(struct
    type t = string
    let compare = compare
  end)

(* Check whether an abbreviation expands to itself. *)
let rec cyclic_abbrev env id ty =
  let ty = Ctype.repr ty in
  match ty.desc with
    Tconstr (Path.Pident id', _, _) when Ident.same id id' ->
      true
  | Tconstr (p, tl, abbrev) ->
      begin try
        cyclic_abbrev env id (Ctype.expand_abbrev env p tl abbrev ty.level)
      with Ctype.Cannot_expand ->
        false
      end
  | _ ->
      false

let transl_declaration env (name, sdecl) (id, decl) =
  reset_type_variables();
  begin try
    List.iter2
      (fun ty sty -> Ctype.unify env ty (enter_type_variable true sty))
      decl.type_params sdecl.ptype_params
  with Already_bound ->
    raise(Error(sdecl.ptype_loc, Repeated_parameter))
  end;

  List.iter
    (function (v, ty, loc) ->
       try
         Ctype.unify env
           (type_variable loc v) (transl_simple_type env false ty)
       with Ctype.Unify _ ->
         raise(Error(loc, Unconsistent_constraint)))
    sdecl.ptype_cstrs;

  let decl' =
    { type_params = decl.type_params;
      type_arity = decl.type_arity;
      type_kind =
        begin match sdecl.ptype_kind with
          Ptype_abstract ->
            Type_abstract
        | Ptype_variant cstrs ->
            let all_constrs = ref StringSet.empty in
            List.iter
              (fun (name, args) ->
                if StringSet.mem name !all_constrs then
                  raise(Error(sdecl.ptype_loc, Duplicate_constructor name));
                all_constrs := StringSet.add name !all_constrs)
              cstrs;
            if List.length cstrs > Config.max_tag then
              raise(Error(sdecl.ptype_loc, Too_many_constructors));
            Type_variant(List.map
              (fun (name, args) ->
                      (name, List.map (transl_simple_type env true) args))
              cstrs)
        | Ptype_record lbls ->
            let all_labels = ref StringSet.empty in
            List.iter
              (fun (name, mut, arg) ->
                if StringSet.mem name !all_labels then
                  raise(Error(sdecl.ptype_loc, Duplicate_label name));
                all_labels := StringSet.add name !all_labels)
              lbls;
            Type_record(List.map
              (fun (name, mut, arg) ->
                      (name, mut, transl_simple_type env true arg))
              lbls)
        end;
      type_manifest =
        begin match (decl.type_manifest, sdecl.ptype_manifest) with
          (None, None) -> None
        | (Some ty, Some sty) ->
            let ty' =
              Ctype.unroll_abbrev id decl.type_params
                (transl_simple_type env true sty)
            in
            if cyclic_abbrev env id ty' then
              raise(Error(sdecl.ptype_loc, Recursive_abbrev name));
            begin try Ctype.unify env ty' ty with Ctype.Unify trace ->
              raise(Error(sdecl.ptype_loc, Type_clash trace))
            end;
            Some ty
        | _ ->
            fatal_error "Typedecl.transl_declaration"
        end } in
  (id, decl')

(* Generalize a type declaration *)

let generalize_decl decl =
  List.iter Ctype.generalize decl.type_params;
  begin match decl.type_kind with
    Type_abstract ->
      ()
  | Type_variant v ->
      List.iter (fun (_, tyl) -> List.iter Ctype.generalize tyl) v
  | Type_record r ->
      List.iter (fun (_, _, ty) -> Ctype.generalize ty) r
  end;
  begin match decl.type_manifest with
    None    -> ()
  | Some ty -> Ctype.generalize ty
  end

(*
   If both a variant/record definition and a type equation are given,
   need to check that the equation refers to a type of the same kind
   with the same constructors and labels.
*)
let check_abbrev env (_, sdecl) (id, decl) =
  match decl with
    {type_kind = (Type_variant _ | Type_record _); type_manifest = Some ty} ->
      begin match (Ctype.repr ty).desc with
        Tconstr(path, args, _) ->
          begin try
            let decl' = Env.find_type path env in
            if
              List.length args = List.length decl.type_params
                      &&
              Ctype.equal env false args decl.type_params
                      &&
              Includecore.type_declarations env id
                (Subst.type_declaration (Subst.add_type id path Subst.identity)
                                        decl)
                decl'
            then ()
            else raise(Error(sdecl.ptype_loc, Definition_mismatch ty))
          with Not_found ->
            raise(Error(sdecl.ptype_loc, Definition_mismatch ty))
          end
      | _ -> raise(Error(sdecl.ptype_loc, Definition_mismatch ty))
      end
  | _ -> ()

(* Check for ill-defined abbrevs *)

(* XXX Obsolete... (occur-check) *)
let check_recursive_abbrev env (name, sdecl) (id, decl) =
  match decl.type_manifest with
    Some ty ->
      begin try Ctype.correct_abbrev env id decl.type_params ty with
        Ctype.Recursive_abbrev ->
          raise(Error(sdecl.ptype_loc, Recursive_abbrev name))
      end
  | _ ->
      ()

(* Translate a set of mutually recursive type declarations *)
let transl_type_decl env name_sdecl_list =
  (* Create identifiers. *)
  let id_list =
    List.map (fun (name, _) -> Ident.create name) name_sdecl_list
  in
  (*
     Since we've introduced fresh idents, make sure the definition
     level is at least the binding time of these events. Otherwise,
     passing one of the recursively-defined type constrs as argument
     to an abbreviation may fail.
  *)
  Ctype.init_def(Ident.current_time());
  Ctype.begin_def();
  (* Enter types. *)
  let (temp_env, temp_decl) = enter_types env (name_sdecl_list, id_list) in
  (* Translate each declaration. *)
  let decls =
    List.map2 (transl_declaration temp_env) name_sdecl_list temp_decl in
  (* Generalize types. *)
  Ctype.end_def();
  List.iter (function (_, decl) -> generalize_decl decl) decls;
  (* Build the final env. *)
  let newenv =
    List.fold_right
      (fun (id, decl) env -> Env.add_type id decl env)
      decls env
  in
  (* Check re-exportation *)
  List.iter2 (check_abbrev newenv) name_sdecl_list decls;
  (* Check for recursive abbrevs *)
  List.iter2 (check_recursive_abbrev newenv) name_sdecl_list decls;
  (* Done *)
  (decls, newenv)

(* Translate an exception declaration *)
let transl_exception env excdecl =
  reset_type_variables();
  Ctype.begin_def();
  let types = List.map (transl_simple_type env true) excdecl in
  Ctype.end_def();
  List.iter Ctype.generalize types;
  types

(* Translate a value declaration *)
let transl_value_decl env valdecl =
  let ty = Typetexp.transl_type_scheme env valdecl.pval_type in
  let prim = Primitive.parse_declaration (Ctype.arity ty) valdecl.pval_prim in
  { val_type = ty;
    val_kind = match prim with Some p -> Val_prim p | None -> Val_reg }

(* Translate a "with" constraint -- much simplified version of
    transl_type_decl. *)
let transl_with_constraint env sdecl =
  reset_type_variables();
  Ctype.begin_def();
  let params =
    try
      List.map (enter_type_variable true) sdecl.ptype_params
    with Already_bound ->
      raise(Error(sdecl.ptype_loc, Repeated_parameter)) in
  List.iter
    (function (v, ty, loc) ->
       try
         Ctype.unify env
           (type_variable loc v) (transl_simple_type env false ty)
       with Ctype.Unify _ ->
         raise(Error(loc, Unconsistent_constraint)))
    sdecl.ptype_cstrs;
  let decl =
    { type_params = params;
      type_arity = List.length params;
      type_kind = Type_abstract;
      type_manifest =
        begin match sdecl.ptype_manifest with
          None -> None
        | Some sty -> Some(transl_simple_type env true sty)
        end }
  in
  Ctype.end_def();
  generalize_decl decl;
  decl

(**** Error report ****)

open Format

let report_error = function
    Repeated_parameter ->
      print_string "A type parameter occurs several times"
  | Duplicate_constructor s ->
      print_string "Two constructors are named "; print_string s
  | Too_many_constructors ->
      print_string "Too many constructors -- maximum is ";
      print_int Config.max_tag; print_string " constructors"
  | Duplicate_label s ->
      print_string "Two labels are named "; print_string s
  | Recursive_abbrev s ->
      print_string "The type abbreviation "; print_string s;
      print_string " is cyclic" (* " expands to itself" *)
  | Definition_mismatch ty ->
      Printtyp.reset ();
      Printtyp.mark_loops ty;
      print_string
        "The variant or record definition does not match that of type";
      print_space(); Printtyp.type_expr ty
  | Unconsistent_constraint ->
      print_string "The type constraints are not consistent"
  | Type_clash trace ->
      Printtyp.unification_error trace
        (function () ->
           print_string "This type constructor expands to type")
        (function () ->
           print_string "but is here used with type")
