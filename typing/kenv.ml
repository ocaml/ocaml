(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                  Jun Furuse, University of Tokyo                    *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Types
open Typedtree

(* konstraint env *)

let debug = try ignore (Sys.getenv "GCAML_DEBUG_KENV"); true with _ -> false

type t = 
    { recursives: value_description list;
      mutable kset: (Types.konst_elem * Typedtree.instance_info ref) list }

let add kenv kelems = kenv.kset <- kelems @ kenv.kset

let print ppf kenv =
  let rec print_list pr sep ppf =
    function
	[] -> ()
      | [a] -> pr ppf a
      | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l
  in
  let print_kelem ppf (kelem,_) = 
    match kelem.kdepend with
    | None -> Printtyp.type_scheme ppf kelem.ktype
    | Some kdepend -> 
	Format.fprintf ppf "@[<2>%a <@ %a@]" 
	  Printtyp.type_scheme kelem.ktype
	  Printtyp.type_scheme kdepend
  in
  Format.fprintf ppf "{ @[recursives= %a;@ kset= %a@] }" 
    (print_list (fun ppf v -> Printtyp.type_scheme v.val_type)
       (fun ppf -> Format.fprintf ppf ",@ ")) kenv.recursives
    (print_list print_kelem (fun ppf -> Format.fprintf ppf ",@ ")) kenv.kset

let instance kenv desc =
  let t = Ctype.repr (desc.val_type) in
  let let_rec_non_generic = List.memq desc kenv.recursives in
  (* Instantiations of Toverload and Tkonst are special! *)
  match t.desc with
  | Toverload {over_aunif= aunif; over_cases= cases} ->
      let t' = Ctype.instance aunif in 
      let inst_info = ref FA_none in
      add kenv [{ ktype=t'; kdepend= Some t }, inst_info];
      t', inst_info
  | Tkonst (konst, t') -> 
      let t'' = Ctype.instance t' in
      let inst_info = ref FA_none in
      add kenv [{ ktype=t''; kdepend= Some t}, inst_info];
      t'', inst_info
  | _ -> 
      if let_rec_non_generic then
	(* Even for non overloaded types, inst_info must be kept its track,
	   for a derived generic defined by let rec; when the definition is
	   type-checked, the derived generic is not yet polymorphic! *)
	let t' = Ctype.instance t in
	let inst_info = ref FA_none in
	add kenv [{ ktype=t'; kdepend= Some t}, inst_info];
	t', inst_info
      else
	Ctype.instance t, ref FA_none

let filter_kset kenv =
  List.filter (fun (k,_) -> 
    match k.kdepend with
    | Some t ->
	begin match t.desc with
	| Toverload _ -> true
	| Tkonst _ -> true
	| _ -> false
	end
    | _ -> false) kenv.kset

let make_tkonst kenv typ =
  let typ = Ctype.repr typ in (* trace all the links *)
  (* step1: Find internal generic use *)
  let internals = filter_kset kenv in
  (* step2: If any internal generic use exists, then let rec non generic
     defined value becomes also generic. We modify the type of the pattern
     type. *)
  if internals <> [] then begin
    let typ' = Ctype.newty typ.desc in (* copy the desc *)
    typ.desc <- Tkonst ([], typ') (* plug Tkonst *)
  end;
Format.eprintf "KKK: (%a) %a@." Printtyp.type_scheme typ print kiset;
  (* step3: filter again with the new pattern type *)
  kiset := filter_kiset !kiset;
  if !kiset <> [] then begin 
    (* !kiset may contain non-generic stuffs *)
    let konsts = List.map fst !kiset in
    let t =
      match typ.desc with
      | Tkonst([], typ') -> (* when Tkonst was plugged in *)
	  Btype.newgenty (Tkonst (konsts, typ'))
      | _ -> 
	  Btype.newgenty (Tkonst (List.map fst !kiset, typ))
    in
    (* Type variable leaves inside genk are generalized, 
       but parent nodes are not. Here, we fix them. *)
(* WE CANNOT DO THIS HERE, SINCE GENERIC RECURSIVE LOOPS MAY BE LOST.
    let t = Ctype.correct_levels t in
*)
Format.eprintf "KKK: (%a ====> %a) %a@." 
      Printtyp.type_scheme typ 
      Printtyp.type_scheme t
      print kiset;
    Etype.normalize_type t;
    t
  end else typ

let resolve_kenv env kiset =
  kiset := filter_kiset !kiset;
if debug && !kiset <> [] then Format.eprintf "@[<2>resolve:@, %a@]@." print kiset;
  let flow_record = (Gtype.resolve_konstraint env (List.map fst !kiset)) 
  in
  List.iter2 (fun (kelem1,instinforef) (kelem2,flow) ->
    if kelem1 != kelem2 then assert false;
    instinforef := FA_flow flow) !kiset flow_record;
if debug then if flow_record <> [] then 
  Format.eprintf "FLOW: %a@." Gtype.print_flow_record flow_record

