(* Inclusion checks for the module language *)

open Misc
open Path
open Typedtree


type error =
    Missing_field of Ident.t
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of Ident.t * type_declaration * type_declaration
  | Exception_declarations of
      Ident.t * exception_declaration * exception_declaration
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration

exception Error of error list

(* All functions "blah env x1 x2" check that x1 is included in x2,
   i.e. that x1 is the type of an implementation that fulfills the
   specification x2. If not, Error is raised with a backtrace of the error. *)

(* Inclusion between value descriptions *)

let value_descriptions env id vd1 vd2 =
  if Includecore.value_descriptions env vd1 vd2
  then ()
  else raise(Error[Value_descriptions(id, vd1, vd2)])

(* Inclusion between type declarations *)

let type_declarations env id decl1 decl2 =
  if Includecore.type_declarations env id decl1 decl2
  then ()
  else raise(Error[Type_declarations(id, decl1, decl2)])

(* Inclusion between exception declarations *)

let exception_declarations env id decl1 decl2 =
  if Includecore.exception_declarations env decl1 decl2
  then ()
  else raise(Error[Exception_declarations(id, decl1, decl2)])

(* Expand a module type identifier when possible *)

exception Dont_match

let expand_module_path env path =
  match Env.find_modtype path env with
    Tmodtype_abstract -> raise Dont_match
  | Tmodtype_manifest mty -> mty

(* Extract name, kind and ident from a signature item *)

type field_desc =
    Field_value of string
  | Field_type of string
  | Field_exception of string
  | Field_module of string
  | Field_modtype of string

let item_ident_name = function
    Tsig_value(id, _) -> (id, Field_value(Ident.name id))
  | Tsig_type(id, _) -> (id, Field_type(Ident.name id))
  | Tsig_exception(id, _) -> (id, Field_exception(Ident.name id))
  | Tsig_module(id, _) -> (id, Field_module(Ident.name id))
  | Tsig_modtype(id, _) -> (id, Field_modtype(Ident.name id))

(* Simplify a structure coercion *)

let simplify_structure_coercion cc =
  let pos = ref 0 in
  try
    List.iter
      (fun (n, c) ->
        if n <> !pos or c <> Tcoerce_none then raise Exit;
        incr pos)
      cc;
    Tcoerce_none
  with Exit ->
    Tcoerce_structure cc

(* Inclusion between module types. 
   Return the restriction that transforms a value of the smaller type
   into a value of the bigger type. *)

let rec modtypes env mty1 mty2 =
  try
    try_modtypes env mty1 mty2
  with 
    Dont_match ->
      raise(Error[Module_types(mty1, mty2)])
  | Error reasons ->
      raise(Error(Module_types(mty1, mty2) :: reasons))

and try_modtypes env mty1 mty2 =
  match (mty1, mty2) with
    (Tmty_ident p1, Tmty_ident p2) when Path.same p1 p2 ->
      Tcoerce_none
  | (Tmty_ident p1, _) ->
      try_modtypes env (expand_module_path env p1) mty2
  | (_, Tmty_ident p2) ->
      try_modtypes env mty1 (expand_module_path env p2)
  | (Tmty_signature sig1, Tmty_signature sig2) ->
      signatures env sig1 sig2
  | (Tmty_functor(param1, arg1, res1), Tmty_functor(param2, arg2, res2)) ->
      let cc_arg =
        modtypes env arg2 arg1 in
      let cc_res =
        Ident.identify param2 param1
          (fun () -> modtypes (Env.add_module param1 arg1 env) res1 res2) in
      begin match (cc_arg, cc_res) with
          (Tcoerce_none, Tcoerce_none) -> Tcoerce_none
        | _ -> Tcoerce_functor(cc_arg, cc_res)
      end
  | (_, _) ->
      raise Dont_match

(* Inclusion between signatures *)

and signatures env sig1 sig2 =
  (* Environment used to check inclusion of components *)
  let new_env =
    Env.add_signature sig1 env in
  (* Build a table of the components of sig1, along with their positions.
     The table is indexed by kind and name of component *)
  let rec build_component_table pos tbl = function
      [] -> tbl
    | item :: rem ->
        let (id, name) = item_ident_name item in
        let nextpos =
          match item with
            Tsig_value(_,_) | Tsig_exception(_,_) | Tsig_module(_,_) -> pos+1
          | Tsig_type(_,_) | Tsig_modtype(_,_) -> pos in
        build_component_table nextpos
                              (Tbl.add name (id, item, pos) tbl) rem in
  let comps1 =
    build_component_table 0 Tbl.empty sig1 in
  (* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. *)
  let rec pair_components paired unpaired = function
      [] ->
        begin match unpaired with
            [] -> signature_components new_env (List.rev paired)
          | _  -> raise(Error unpaired)
        end
    | item2 :: rem ->
        let (id2, name2) = item_ident_name item2 in
        begin try
          let (id1, item1, pos1) = Tbl.find name2 comps1 in
          Ident.identify id1 id2
            (fun () ->
              pair_components ((item1, item2, pos1) :: paired) unpaired rem)
        with Not_found ->
          pair_components paired (Missing_field id2 :: unpaired) rem
        end in
  (* Do the pairing and checking, and return the final coercion *)
  simplify_structure_coercion(pair_components [] [] sig2)

(* Inclusion between signature components *)

and signature_components env = function
    [] -> []
  | (Tsig_value(id1, valdecl1), Tsig_value(id2, valdecl2), pos) :: rem ->
      value_descriptions env id1 valdecl1 valdecl2;
      (pos, Tcoerce_none) :: signature_components env rem
  | (Tsig_type(id1, tydecl1), Tsig_type(id2, tydecl2), pos) :: rem ->
      type_declarations env id1 tydecl1 tydecl2;
      signature_components env rem
  | (Tsig_exception(id1, excdecl1), Tsig_exception(id2, excdecl2), pos)
    :: rem ->
      exception_declarations env id1 excdecl1 excdecl2;
      (pos, Tcoerce_none) :: signature_components env rem
  | (Tsig_module(id1, mty1), Tsig_module(id2, mty2), pos) :: rem ->
      let cc = modtypes env mty1 mty2 in
      (pos, cc) :: signature_components env rem
  | (Tsig_modtype(id1, info1), Tsig_modtype(id2, info2), pos) :: rem ->
      modtype_infos env id1 info1 info2;
      signature_components env rem
  | _ ->
      fatal_error "Includemod.signature_components"

(* Inclusion between module type specifications *)

and modtype_infos env id info1 info2 =
  try
    match (info1, info2) with
      (Tmodtype_abstract, Tmodtype_abstract) -> ()
    | (Tmodtype_manifest mty1, Tmodtype_abstract) -> ()
    | (Tmodtype_manifest mty1, Tmodtype_manifest mty2) ->
        modtypes env mty1 mty2; modtypes env mty2 mty1; ()
    | (_, Tmodtype_manifest mty2) ->
        let mty1 = Tmty_ident(Pident id) in
        modtypes env mty1 mty2; modtypes env mty2 mty1; ()
  with Error reasons ->
    raise(Error(Modtype_infos(id, info1, info2) :: reasons))

(* Error report *)

open Format
open Printtyp

let include_err = function
    Missing_field id ->
      print_string "Missing field "; ident id
  | Value_descriptions(id, d1, d2) ->
      open_hvbox 2;
      print_string "Values do not match:"; print_space();
      value_description id d1; 
      print_break(1, -2);
      print_string "is not included in"; print_space();
      value_description id d2;
      close_box()
  | Type_declarations(id, d1, d2) ->
      open_hvbox 2;
      print_string "Type declarations do not match:"; print_space();
      type_declaration id d1; 
      print_break(1, -2);
      print_string "is not included in"; print_space();
      type_declaration id d2;
      close_box()
  | Exception_declarations(id, d1, d2) ->
      open_hvbox 2;
      print_string "Exception declarations do not match:"; print_space();
      exception_declaration id d1; 
      print_break(1, -2);
      print_string "is not included in"; print_space();
      exception_declaration id d2;
      close_box()
  | Module_types(mty1, mty2)->
      open_hvbox 2;
      print_string "Modules do not match:"; print_space();
      modtype mty1;
      print_break(1, -2);
      print_string "is not included in"; print_space();
      modtype mty2;
      close_box()
  | Modtype_infos(id, d1, d2) ->
      open_hvbox 2;
      print_string "Module type declarations do not match:"; print_space();
      modtype_declaration id d1; 
      print_break(1, -2);
      print_string "is not included in"; print_space();
      modtype_declaration id d2;
      close_box()

let report_error errlist =
  match List.rev errlist with
    [] -> ()
  | err :: rem ->
      include_err err;
      List.iter (fun err -> print_space(); include_err err) rem

