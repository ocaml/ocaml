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

open Misc
open Path
open Types
open Typedtree
open Translcore

let find_recursively_defined_types idecls =
  let defined = List.map fst idecls in
(*
Format.eprintf "find_recursively_defined_types ";
List.iter (fun i -> Format.eprintf "%s " (Ident.name i)) defined;
Format.eprintf "@.";
*)
  let found = ref [] in
  let find_datatypes_of_type ty =
    let visited = ref [] in
    let rec f ty = 
      if List.memq ty !visited then ()
      else begin
	visited := ty :: !visited;
	match ty.desc with
	| Tconstr (Pident id, _, _) -> 
	    (* we are only interested in ids in defined *)
	    if List.memq id defined && 
	       not (List.memq id !found) then found := id :: !found;
	    Btype.iter_type_expr f ty
	| _ -> Btype.iter_type_expr f ty
      end
    in
    f ty
  in
  let used_datatypes_of_typedecl decl =
    found := [];
    List.iter find_datatypes_of_type decl.type_params;
    begin match decl.type_kind with
    | Type_abstract -> ()
    | Type_variant (cnsts,_) ->
	List.iter (fun (_,ts) -> List.iter find_datatypes_of_type ts) cnsts
    | Type_record (lbls, _, _) ->
	List.iter (fun (_,_,ty) -> find_datatypes_of_type ty) lbls
    end;
    begin match decl.type_manifest with 
    | Some ty -> find_datatypes_of_type ty
    | None -> ()
    end;
    !found
  in
  let graph =
    List.map (fun (id,decl) -> id, used_datatypes_of_typedecl decl) idecls
  in
  (* not quite efficient algorithm *)
(*
Format.eprintf "graph done@.";
List.iter (fun (id, g) -> 
  Format.eprintf "%s => " (Ident.name id);
  List.iter (fun id -> Format.eprintf "%s " (Ident.name id)) g;
  Format.eprintf "@.") graph;
*)
  let find_loop start =
    let uniq src = 
      List.fold_left (fun set id -> 
	if List.memq id set then set else id :: set) [] src
    in
    let rec find_loop_aux path cnx =
      List.flatten (List.map (fun id -> 
	if id == start then id :: path
	else if List.memq id path then []
	else 
	  let path = id :: path in
	  let cnx = List.assq id graph in
	  find_loop_aux path cnx) cnx)
    in
    uniq (start :: find_loop_aux [] (List.assq start graph))
  in
  let in_groups = ref [] in
  let groups =
    List.fold_left (fun groups id ->
      if List.memq id !in_groups then groups
      else
	let group = find_loop id in
	in_groups := group @ !in_groups;
	group :: groups) [] defined
  in
(*
Format.eprintf "groups: ";
List.iter (fun g -> 
  Format.eprintf "(%a) "
    (fun ppf -> List.iter (fun id -> Format.fprintf ppf "%s, " (Ident.name id))) g) groups;
Format.eprintf "@.";
*)
  let rec find_group id = function
    | [] -> assert false
    | g::_ when List.memq id g -> g
    | _::gs -> find_group id gs
  in
  let group_table = List.map (fun id -> id, find_group id groups) defined in
  let rec traverse found id =
    (* traverse the graph and create node list related only to 
       recursive defs. this provides sorted recursively defined ids *)
    let recdefs = List.assq id group_table in
    let nodes = 
      List.fold_right (fun node st ->
	if List.memq node recdefs && not (List.memq node found) then 
	  node :: st 
	else st) 
	(List.assq id graph) []
    in
    List.fold_left (fun found node ->
      traverse found node) (found @ nodes) nodes 
  in
  List.map (fun id -> id, traverse [] id) defined
 
let transl_type_declarations decls =
  let group_assoc = find_recursively_defined_types decls in
  List.map (fun (id, decl) -> 
    let group = List.assq id group_assoc in
    let recdefs = 
      List.fold_right (fun i ids -> if id == i then ids else i :: ids) 
	group []
    in
    id, transl_type_declaration Env.initial (Ident.name id) recdefs decl) decls
