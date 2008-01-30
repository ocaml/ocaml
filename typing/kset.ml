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

(* konstraint set *)

let debug = Gdebug.defined "GCAML_DEBUG_KSET"

type elem = { kelem_type : type_expr;
	      kelem_vdesc : value_description;
	      kelem_instinfo : instance_info ref } 
type t = elem list ref

let empty () = ref []
let add kset k = kset := k :: !kset
let get kset = !kset
let create kset = ref kset

let print ppf kset =
  let rec print_list pr sep ppf =
    function
	[] -> ()
      | [a] -> pr ppf a
      | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l
  in
  let print_kelem ppf { kelem_type= kt; kelem_vdesc= kvdesc } = 
    Format.fprintf ppf "@[<2>%a <@ %a@]" 
      Printtyp.type_scheme kt
      Printtyp.type_scheme kvdesc.val_type
  in
  Format.fprintf ppf "{ @[%a@] }" 
    (print_list print_kelem (fun ppf -> Format.fprintf ppf ",@ ")) !kset

(* New type instantiation of [vdesc]
   When an overloaded type is instantiated, we add its konstraint part
   to the konstraint set [kset]. The gathered constraints in [kset] 
   must be solved later. 
*)
let instance kset vdesc =
  let t = Ctype.repr vdesc.val_type in
  (* Instantiations of Toverload and Tkonst are special! *)
  match t.desc with
  | Toverload {over_aunif= aunif; over_cases= cases} ->
      let t' = Ctype.instance aunif in 
      let inst_info = ref FA_none in
      add kset { kelem_type= t'; 
		 kelem_vdesc= vdesc; 
		 kelem_instinfo= inst_info };
      t', inst_info
  | Tkonst (konst, t') -> 
      let t'' = Ctype.instance t' in
      let inst_info = ref FA_none in
      add kset { kelem_type= t'';
		 kelem_vdesc= vdesc;
		 kelem_instinfo= inst_info };
      t'', inst_info
  | _ -> 
      (* Even for non overloaded types, inst_info must be kept its track,
	 for a derived generic defined by let rec; when the definition is
	 type-checked, the derived generic is not yet polymorphic! *)
      let t' = Ctype.instance t in
      let inst_info = ref FA_none in
      add kset { kelem_type= t';
		 kelem_vdesc= vdesc;
		 kelem_instinfo= inst_info };
      t', inst_info

let filter_konst { kelem_vdesc= kvdesc } =
  match (Ctype.repr kvdesc.val_type).desc with
  | Tkonst _ | Toverload _ -> true
  | _ -> false

let resolve_kset env kset =
  let size0 = List.length !kset in
  kset := List.filter filter_konst !kset;
  let size1 = List.length !kset in
  if debug then Format.eprintf "kset length= %d => %d@." size0 size1;
  if debug && !kset <> [] then Format.eprintf "@[<2>resolve:@, %a@]@." print kset;
  let flow_record = 
    Gtype.resolve_konstraint env (List.map (fun kelem ->
      { ktype= kelem.kelem_type; 
	kdepend= Some kelem.kelem_vdesc.val_type }) !kset)
  in
  List.iter2 (fun kelem (kelem2,flow) ->
    if kelem.kelem_type != kelem2.ktype then assert false;
    kelem.kelem_instinfo := FA_flow flow) !kset flow_record;
  if debug then if flow_record <> [] then 
    Format.eprintf "FLOW: %a@." Gtype.print_flow_record flow_record

