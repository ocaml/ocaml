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

(* Operations on module types *)

open Path
open Types


let rec scrape env mty =
  match mty with
    Tmty_ident p ->
      begin try
        match Env.find_modtype p env with
          Tmodtype_abstract -> mty
        | Tmodtype_manifest mty' -> scrape env mty'
      with Not_found ->
        mty
      end
  | _ -> mty

let rec strengthen env mty p =
  match scrape env mty with
    Tmty_signature sg ->
      Tmty_signature(strengthen_sig env sg p)
  | Tmty_functor(param, arg, res) ->
      Tmty_functor(param, arg, strengthen env res (Papply(p, Pident param)))
  | mty ->
      mty

and strengthen_sig env sg p =
  match sg with
    [] -> []
  | (Tsig_value(id, desc) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Tsig_type(id, decl) :: rem ->
      let newdecl =
        match decl.type_manifest with
          None ->
            { type_params = decl.type_params;
              type_arity = decl.type_arity;
              type_kind = decl.type_kind;
              type_manifest = Some(Ctype.newgenty(
	       		           Tconstr(Pdot(p, Ident.name id, nopos),
                                           decl.type_params,
					   ref []))) }
        | _ -> decl in
      Tsig_type(id, newdecl) :: strengthen_sig env rem p
  | (Tsig_exception(id, d) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Tsig_module(id, mty) :: rem ->
      Tsig_module(id, strengthen env mty (Pdot(p, Ident.name id, nopos))) ::
      strengthen_sig (Env.add_module id mty env) rem p
      (* Need to add the module in case it defines manifest module types *)
  | Tsig_modtype(id, decl) :: rem ->
      let newdecl =
        match decl with
          Tmodtype_abstract ->
            Tmodtype_manifest(Tmty_ident(Pdot(p, Ident.name id, nopos)))
        | Tmodtype_manifest _ ->
            decl in
      Tsig_modtype(id, newdecl) ::
      strengthen_sig (Env.add_modtype id decl env) rem p
      (* Need to add the module type in case it is manifest *)
  | (Tsig_class(id, decl) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p

(* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. *)

type variance = Co | Contra | Strict

let nondep_supertype env mid mty =

  let rec nondep_mty va mty =
    match mty with
      Tmty_ident p ->
        if Path.isfree mid p then begin
          match Env.find_modtype p env with
            Tmodtype_abstract -> raise Not_found
          | Tmodtype_manifest mty -> nondep_mty va mty      
        end else mty
    | Tmty_signature sg ->
        Tmty_signature(nondep_sig va sg)
    | Tmty_functor(param, arg, res) ->
        let var_inv =
          match va with Co -> Contra | Contra -> Co | Strict -> Strict in
        Tmty_functor(param, nondep_mty var_inv arg, nondep_mty va res)

  and nondep_sig va = function
    [] -> []
  | item :: rem ->
      let rem' = nondep_sig va rem in
      match item with
        Tsig_value(id, d) ->
          Tsig_value(id, {val_type = Ctype.nondep_type env mid d.val_type;
                          val_kind = d.val_kind}) :: rem'
      | Tsig_type(id, d) ->
          Tsig_type(id, nondep_type_decl va d) :: rem'
      | Tsig_exception(id, d) ->
          Tsig_exception(id, List.map (Ctype.nondep_type env mid) d) :: rem'
      | Tsig_module(id, mty) ->
          Tsig_module(id, nondep_mty va mty) :: rem'
      | Tsig_modtype(id, d) ->
          begin try
            Tsig_modtype(id, nondep_modtype_decl d) :: rem'
          with Not_found ->
            match va with
              Co -> Tsig_modtype(id, Tmodtype_abstract) :: rem'
            | _  -> raise Not_found
          end
      | Tsig_class(id, d) ->
          Tsig_class(id, Ctype.nondep_class_type env mid d) :: rem'

  and nondep_type_decl va d =
    {type_params = d.type_params;
     type_arity = d.type_arity;
     type_kind =
       begin try
         match d.type_kind with
           Type_abstract ->
             Type_abstract
         | Type_variant cstrs ->
             Type_variant(List.map
               (fun (c, tl) -> (c, List.map (Ctype.nondep_type env mid) tl))
               cstrs)
         | Type_record lbls ->
             Type_record(List.map
               (fun (c, mut, t) -> (c, mut, Ctype.nondep_type env mid t))
               lbls)
       with Not_found ->
         match va with Co -> Type_abstract | _ -> raise Not_found
       end;
     type_manifest =
       begin try
         match d.type_manifest with
           None -> None
         | Some ty -> Some(Ctype.nondep_type env mid ty)
       with Not_found ->
         match va with Co -> None | _ -> raise Not_found
       end}

  and nondep_modtype_decl = function
      Tmodtype_abstract -> Tmodtype_abstract
    | Tmodtype_manifest mty -> Tmodtype_manifest(nondep_mty Strict mty)

  in
    nondep_mty Co mty
