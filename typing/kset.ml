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

let debug = try ignore (Sys.getenv "GCAML_DEBUG_KSET"); true with _ -> false

type elem = type_expr * value_description * instance_info ref 
type t = elem list ref

let empty () = ref []
let add kset k = kset := k @ !kset
let get kset = !kset
let create kset = ref kset

let print ppf kset =
  let rec print_list pr sep ppf =
    function
	[] -> ()
      | [a] -> pr ppf a
      | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l
  in
  let print_kelem ppf (kt,kvdesc,_) = 
    Format.fprintf ppf "@[<2>%a <@ %a@]" 
      Printtyp.type_scheme kt
      Printtyp.type_scheme kvdesc.val_type
  in
  Format.fprintf ppf "{ @[%a@] }" 
    (print_list print_kelem (fun ppf -> Format.fprintf ppf ",@ ")) !kset

let instance kset vdesc =
  let t = Ctype.repr (vdesc.val_type) in
  (* Instantiations of Toverload and Tkonst are special! *)
  match t.desc with
  | Toverload {over_aunif= aunif; over_cases= cases} ->
      let t' = Ctype.instance aunif in 
      let inst_info = ref FA_none in
      add kset [t', vdesc, inst_info];
      t', inst_info
  | Tkonst (konst, t') -> 
      let t'' = Ctype.instance t' in
      let inst_info = ref FA_none in
      add kset [t'', vdesc, inst_info];
      t'', inst_info
(* TOO ADVANCED
      begin match 
	Ctype.instance_list (t::(List.map (fun ke -> ke.ktype) konst)) 
      with
      | t'::ktypes' ->
	  let konst' = 
	    List.map2 (fun ke ktype' -> { ke with ktype= ktype' }) 
	      konst ktypes'
	  in
Format.eprintf "kinst=%a@." Printtyp.type_scheme t;
	  add kset konst'; 
	  t'
      | _ -> assert false
      end
*)
  | _ -> 
	(* Even for non overloaded types, inst_info must be kept its track,
	   for a derived generic defined by let rec; when the definition is
	   type-checked, the derived generic is not yet polymorphic! *)
	let t' = Ctype.instance t in
	let inst_info = ref FA_none in
	add kset [t', vdesc, inst_info];
	t', inst_info
(*
	Ctype.instance t, ref FA_none
*)

(*
let make_tkonst konsts typ =
  let typ = Ctype.repr typ in
  let konsts = List.filter filter_konst konsts in
  if konsts <> [] then begin 
    (* Type variable leaves inside genk are generalized, 
       but parent nodes are not. Here, we fix them. *)
(* WE CANNOT DO THIS HERE, SINCE GENERIC RECURSIVE LOOPS MAY BE LOST.
    let t = Ctype.correct_levels t in
*)
    let t = Btype.newgenty (Tkonst (konsts, typ)) in
Format.eprintf "make_tkonst=> %a@." Printtyp.type_scheme t;
    Etype.normalize_type t;
    t
  end else typ
*)

let filter_konst (_,kvdesc,_) =
  match (Ctype.repr kvdesc.val_type).desc with
  | Tkonst _ | Toverload _ -> true
  | _ -> false

let resolve_kset env kset =
  kset := List.filter filter_konst !kset;
  if debug && !kset <> [] then Format.eprintf "@[<2>resolve:@, %a@]@." print kset;
  let flow_record = 
    Gtype.resolve_konstraint env (List.map (fun (kt,kvdesc,_) ->
      {ktype= kt; kdepend= Some kvdesc.val_type}) !kset)
  in
  List.iter2 (fun (kt,kvdesc,instinforef) (kelem2,flow) ->
    if kt != kelem2.ktype then assert false;
    instinforef := FA_flow flow) !kset flow_record;
if debug then if flow_record <> [] then 
  Format.eprintf "FLOW: %a@." Gtype.print_flow_record flow_record

