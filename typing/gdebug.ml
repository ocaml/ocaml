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

let print_type_scheme_ref = ref (fun ppf scm -> assert false)
let print_type_scheme ppf scm = !print_type_scheme_ref ppf scm

let print_konst_elem ppf kelem =
  match kelem.kdepend with
  | None -> Format.fprintf ppf "(%a)" print_type_scheme kelem.ktype
  | Some scm -> 
      Format.fprintf ppf "(%a < %a)" 
	print_type_scheme kelem.ktype
	print_type_scheme scm

let print_list p sep ppf l =
  let rec f = function
    | [] -> ()
    | [x] -> 
	Format.fprintf ppf "@[%a@]" p x
    | x::xs ->
	Format.fprintf ppf "@[%a@]" p x;
	sep ppf;
	f xs
  in
  f l

let print_raw_type_expr_ref = ref (fun ppf scm -> assert false)
let print_raw_type_expr ppf scm = !print_raw_type_expr_ref ppf scm

let defined s = try ignore (Sys.getenv s); true with _ -> false
let defined = 
  let tbl = Hashtbl.create 107 in
  fun s -> try Hashtbl.find tbl s with Not_found -> 
    let v = defined s in
    Hashtbl.add tbl s v;
    v
