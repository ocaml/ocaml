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

(* Substitutions *)

open Misc
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
  | Papply(p1, p2) ->
      Papply(module_path s p1, module_path s p2)

let type_path s = function
    Pident id as p ->
      begin try Ident.find_same id s.types with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      fatal_error "Subst.type_path"

(* From Ctype *)
let rec repr = function
    {desc = Tlink ty} as t ->
      let r = repr ty in
      if r != ty then t.desc <- Tlink r;
      r
  | t -> t

(* From Ctype *)
let rec opened ty =
  match (repr ty).desc with
    Tfield(_, _, t) -> opened t
  | Tvar            -> true
  | Tnil            -> false
  | _               -> fatal_error "Subst.opened"

let generic_level = -1

let newgenty desc =
  {desc = desc; level = generic_level}

let new_val = ref []

type 'a visited = Zero | One | Many of 'a

let rec typexp visited s ty =
  let ty = repr ty in
  if ty.desc = Tvar then ty else
  try
    match List.assq ty visited with
      {contents = Zero} as v ->
      	let t = newgenty Tvar in
	v := Many t;
	let ty' = typexp_2 visited s ty v in
	t.desc <- ty'.desc;
	t
    | {contents = One} as v ->
        let t = newgenty Tvar in
        v := Many t;
        t
    | {contents = Many t} ->
        t
  with Not_found ->
    let v = ref One in
    let ty' = typexp_2 ((ty, v)::visited) s ty v in
    match v with
      {contents = Many t} ->
      	t.desc <- ty'.desc;
	t
    | _ ->
      	ty'

and typexp_2 visited s ty v =
  match ty.desc with
    Tvar ->
      ty
  | Tarrow(t1, t2) ->
      newgenty(Tarrow(typexp visited s t1, typexp visited s t2))
  | Ttuple tl ->
      newgenty(Ttuple(List.map (typexp visited s) tl))
  | Tconstr(p, [], _) ->
      newgenty(Tconstr(type_path s p, [], ref []))
  | Tconstr(p, tl, _) ->
      newgenty(Tconstr(type_path s p, List.map (typexp visited s) tl, ref []))
  | Tobject (t1, name) ->
      let ty' () =
      	let name' =
          match !name with
            None -> None
          | Some (p, tl) ->
              Some (type_path s p, List.map (typexp visited s) tl)
	in
        newgenty(Tobject (typexp visited s t1, ref name'))
      in
      if opened t1 then
        try
          List.assq ty !new_val
        with Not_found ->
          if v = ref One then begin
            let t = newgenty Tvar in
    	    v := Many t;
            new_val := (ty, t):: !new_val
          end;
          ty' ()
      else
        ty' ()
  | Tfield(n, t1, t2) ->
      newgenty(Tfield(n, typexp visited s t1, typexp visited s t2))
  | Tnil ->
      newgenty Tnil
  | Tlink _ ->
      fatal_error "Subst.typexp"

let type_expr s ty =
  new_val := [];
  let ty = typexp [] s ty in
  new_val := [];
  ty

let value_description s descr =
  { val_type = type_expr s descr.val_type;
    val_kind = descr.val_kind }

let type_declaration s decl =
  { type_params = decl.type_params;
    type_arity = decl.type_arity;
    type_kind =
      begin match decl.type_kind with
        Type_abstract -> Type_abstract
      | Type_variant cstrs ->
          Type_variant(List.map (fun (n, args) -> (n, List.map (type_expr s) args))
                           cstrs)
      | Type_record lbls ->
          Type_record(List.map (fun (n, mut, arg) -> (n, mut, type_expr s arg))
                          lbls)
      end;
    type_manifest =
      begin match decl.type_manifest with
        None -> None
      | Some ty -> Some(type_expr s ty)
      end
  }

let exception_declaration s tyl =
  List.map (type_expr s) tyl

let class_type s decl =
  new_val := [];
  let params = List.map (function p -> (repr p, ref Zero)) decl.cty_params in
  let decl =
    { cty_params = List.map (typexp params s) decl.cty_params;
      cty_args = List.map (typexp params s) decl.cty_args;
      cty_vars =
        Vars.fold (fun l (m, t) -> Vars.add l (m, typexp params s t))
          decl.cty_vars Vars.empty;
      cty_self = typexp params s decl.cty_self;
      cty_concr = decl.cty_concr;
      cty_new =
        begin match decl.cty_new with
      	  None    -> None
        | Some ty -> Some (typexp params s ty)
      	end }
  in
    new_val := [];
    decl

let rec modtype s = function
    Tmty_ident p as mty ->
      begin match p with
        Pident id ->
          begin try Ident.find_same id s.modtypes with Not_found -> mty end
      | Pdot(p, n, pos) ->
          Tmty_ident(Pdot(module_path s p, n, pos))
      | Papply(p1, p2) ->
          fatal_error "Subst.modtype"
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
  | Tsig_class(id, d) -> Tsig_class(id, class_type s d)

and modtype_declaration s = function
    Tmodtype_abstract -> Tmodtype_abstract
  | Tmodtype_manifest mty -> Tmodtype_manifest(modtype s mty)
