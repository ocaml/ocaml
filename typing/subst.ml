(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Substitutions *)

open Path
open Typedtree


type t = 
  { types: Path.t Ident.tbl;
    modules: Path.t Ident.tbl;
    modtypes: module_type Ident.tbl }

let identity =
  { types = Ident.empty; modules = Ident.empty; modtypes = Ident.empty }

let add_type id p s =
  { types = Ident.add id p s.types;
    modules = s.modules;
    modtypes = s.modtypes }

let add_module id p s =
  { types = s.types;
    modules = Ident.add id p s.modules;
    modtypes = s.modtypes }

let add_modtype id ty s =
  { types = s.types;
    modules = s.modules;
    modtypes = Ident.add id ty s.modtypes }

let rec module_path s = function
    Pident id as p ->
      begin try Ident.find_same id s.modules with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)

let type_path s = function
    Pident id as p ->
      begin try Ident.find_same id s.types with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)

let rec type_expr s = function
    Tvar{tvar_link = None} as ty -> ty
  | Tvar{tvar_link = Some ty} -> type_expr s ty
  | Tarrow(t1, t2) -> Tarrow(type_expr s t1, type_expr s t2)
  | Ttuple tl -> Ttuple(List.map (type_expr s) tl)
  | Tconstr(p, []) -> Tconstr(type_path s p, [])
  | Tconstr(p, tl) -> Tconstr(type_path s p, List.map (type_expr s) tl)

let value_description s descr =
  { val_type = type_expr s descr.val_type;
    val_prim = descr.val_prim }

let type_declaration s decl =
  { type_params = decl.type_params;
    type_arity = decl.type_arity;
    type_kind =
      match decl.type_kind with
        Type_abstract -> Type_abstract
      | Type_manifest ty -> Type_manifest(type_expr s ty)
      | Type_variant cstrs ->
          Type_variant(List.map (fun (n, args) -> (n, List.map (type_expr s) args))
                           cstrs)
      | Type_record lbls ->
          Type_record(List.map (fun (n, mut, arg) -> (n, mut, type_expr s arg))
                          lbls)
  }

let exception_declaration s tyl =
  List.map (type_expr s) tyl

let rec modtype s = function
    Tmty_ident p as mty ->
      begin match p with
        Pident id ->
          begin try Ident.find_same id s.modtypes with Not_found -> mty end
      | Pdot(p, n, pos) ->
          Tmty_ident(Pdot(module_path s p, n, pos))
      end
  | Tmty_signature sg ->
      Tmty_signature(signature s sg)
  | Tmty_functor(id, arg, res) ->
      Tmty_functor(id, modtype s arg, modtype s res)

and signature s sg = List.map (signature_item s) sg

and signature_item s = function
    Tsig_value(id, d) -> Tsig_value(id, value_description s d)
  | Tsig_type(id, d) -> Tsig_type(id, type_declaration s d)
  | Tsig_exception(id, d) -> Tsig_exception(id, exception_declaration s d)
  | Tsig_module(id, mty) -> Tsig_module(id, modtype s mty)
  | Tsig_modtype(id, d) -> Tsig_modtype(id, modtype_declaration s d)

and modtype_declaration s = function
    Tmodtype_abstract -> Tmodtype_abstract
  | Tmodtype_manifest mty -> Tmodtype_manifest(modtype s mty)
