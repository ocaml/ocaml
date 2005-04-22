(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                   Jun Furuse, University of Tokyo                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format
open Rtype

let rec print_list sep f ppf = function
  | [] -> ()
  | [x] -> f ppf x
  | x::xs -> 
      fprintf ppf "%a%a%a" 
	f x
	sep ()
	(print_list sep f) xs

let print_tuple f ppf v = 
  print_list (fun ppf () -> fprintf ppf ",@ ") f ppf v

type base_printer = formatter -> Obj.t -> unit

let printers =
  [ Rtype.int, ((fun _(*[]*) ppf v -> fprintf ppf "%i" (Obj.obj v)) :
			 base_printer list -> base_printer);
    Rtype.char, (fun _(*[]*) ppf v -> fprintf ppf "%C" (Obj.obj v));
    Rtype.string, (fun _(*[]*) ppf v -> fprintf ppf "%S" (Obj.obj v));
    Rtype.float, (fun _(*[]*) ppf v -> fprintf ppf "%F" (Obj.obj v));
    Rtype.bool, (fun _(*[]*) ppf v -> fprintf ppf "%B" (Obj.obj v));
    Rtype.unit, (fun _(*[]*) ppf v -> fprintf ppf "()");
    Rtype.exn, (fun _(*[]*) ppf v -> fprintf ppf "<exn>");
    Rtype.nativeint, (fun _(*[]*) ppf v -> fprintf ppf "%nd" (Obj.obj v));
    Rtype.int32, (fun _(*[]*) ppf v -> fprintf ppf "%ld" (Obj.obj v));
    Rtype.int64, (fun _(*[]*) ppf v -> fprintf ppf "%Ld" (Obj.obj v));
    
    Rtype.list, (fun printers ppf v ->
      match printers with
      | [printer] ->
	  begin match Obj.obj v with
	  | [] -> fprintf ppf "[]"
	  | v -> 
	      fprintf ppf "@[<2>[ %a ]@]" 
		(print_list (fun ppf () -> fprintf ppf ";@ ") printer) v
	  end
      | _ -> assert false);

    Rtype.array, (fun printers ppf v ->
      match printers with
      | [printer] ->
	  begin match Array.to_list (Obj.obj v) with
	  | [] -> fprintf ppf "[||]"
	  | v -> 
	      fprintf ppf "@[<3>[| %a |]@]" 
		(print_list (fun ppf () -> fprintf ppf ";@ ") printer) v
	  end
      | _ -> assert false)
      
  ]

type printer = Rtype.type_expr -> Format.formatter -> Obj.t -> unit

let rec gen_print self ty ppf v =
  match ty.desc with
  | Tvar -> fprintf ppf "<poly>"
  | Tarrow (_,_,_) -> fprintf ppf "<fun>"
  | Ttuple ts -> 
      (* bind types and values *)
      let rec bind pos = function
  	| [] -> []
  	| t::ts -> (t, Obj.field v pos) :: bind (pos+1) ts
      in
      fprintf ppf "(@[%a@])" 
  	(print_tuple (fun ppf (t,v) -> self t ppf v)) (bind 0 ts)
  | Tconstr ((_, decl), args) -> 
      try
	(* call printers of base types *)
	print_base self ppf decl args v
      with
      | Not_found ->
	  match decl.type_kind with
	  | Type_abstract -> 
	      begin match decl.type_manifest with
	      | Some t ->
		  let s = List.combine decl.type_params args in
		  self (Rtype.subst s t) ppf v
	      | None -> fprintf ppf "<abstr>"
	      end
	  | Type_variant (cnstrs, _) -> 
	      print_variant self ppf decl args cnstrs v
	  | Type_record (labels, Record_regular, _) ->
	      print_regular_record self ppf decl args labels v
	  | Type_record (labels, Record_float, _) ->
	      print_float_record self ppf decl args labels v

and print_base self ppf decl args v =
  let printer = List.assq decl printers in
  let sub_printers = List.map self args in
  printer sub_printers ppf v

and print_variant self ppf decl args cnstrs v =
  if Obj.is_int v then
    let rec find_constr_as_int p = function
      | [] -> assert false
      | (cnst,[]) :: cs -> 
	  if p = 0 then cnst else find_constr_as_int (p-1) cs
      | _::cs -> find_constr_as_int p cs
    in
    let cnst = find_constr_as_int (Obj.obj v : int) cnstrs in
    fprintf ppf "%s" cnst
  else begin
    let sub = List.combine decl.type_params args in
    let rec find_constr_as_block p = function
      | [] -> assert false
      | (cnst,[]) :: cs -> find_constr_as_block p cs
      | (cnst,args)::cs -> 
	  if p = 0 then cnst,args
	  else find_constr_as_block (p-1) cs
    in
    let cnst, args = find_constr_as_block (Obj.tag v) cnstrs in
    let subprinters = 
      List.map self (List.map (Rtype.subst sub) args)
    in
    let fields =
      let rec get_fields p = function
	| [] -> []
	| _::xs -> (Obj.field v p) :: get_fields (p+1) xs
      in
      get_fields 0 args
    in
    match List.combine subprinters fields with
    | [sprt, field] -> 
	fprintf ppf "@[%s (%a)@]" cnst sprt field
    | sprtfields -> 
	let rec print_args ppf = function
	  | [] -> ()
	  | [p,v] -> p ppf v
	  | (p,v)::pvs -> 
	      fprintf ppf "%a,@,%a" 
		p v 
		print_args pvs
	in
	fprintf ppf "@[%s (@[%a@])@]" cnst
	  print_args sprtfields
  end

and print_regular_record self ppf decl args labels v =
  let sub = List.combine decl.type_params args in
  (* sub printer prints label and content *)
  let labels_and_subprinters = 
    List.map (fun (l,_,t) -> 
      l, self (Rtype.subst sub t)) labels
  in
  let fields =
    let rec get_fields p = function
      | [] -> []
      | _::xs -> (Obj.field v p) :: get_fields (p+1) xs
    in
    get_fields 0 labels
  in
  fprintf ppf "@[<2>{ %a }@]" 
    (print_list (fun ppf () -> fprintf ppf ";@ ") 
       (fun ppf ((l,p),v) ->
	 fprintf ppf "%s = %a" l p (Obj.repr v)))
    (List.combine labels_and_subprinters fields)

and print_float_record self ppf decl args labels v =
  let sub = List.combine decl.type_params args in
  (* sub printer prints label and content *)
  let labels_and_subprinters = 
    List.map (fun (l,_,t) -> 
      l, self (Rtype.subst sub t)) labels
  in
  let values_as_float = 
    Array.to_list (Obj.obj v : float array) 
  in
  fprintf ppf "@[<2>{ %a }@]" 
    (print_list (fun ppf () -> fprintf ppf ";@ ") 
       (fun ppf ((l,p),v) ->
	 fprintf ppf "%s = %a" l p (Obj.repr v)))
    (List.combine labels_and_subprinters values_as_float)

let rec printer ty ppf v = gen_print printer ty ppf v

generic val print : {'a} => formatter -> 'a -> unit = printer

let eprint v = Format.eprintf "%a@." print v 
