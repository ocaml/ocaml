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

type depth_info = 
    { max_depth : int;
      cur_depth : int }

let incr_depth depth = { depth with cur_depth= depth.cur_depth + 1 }
let over_depth depth = depth.max_depth < depth.cur_depth

let print_dots ppf = fprintf ppf "..."

type base_printer = depth_info -> formatter -> Obj.t -> unit

let rec print_list sep f ppf = function
  | [] -> ()
  | [x] -> f ppf x
  | x::xs -> 
      fprintf ppf "%a%a%a" 
	f x
	sep ()
	(print_list sep f) xs

let rec print_list_with_depth depth sep (f : base_printer) ppf = function
  | [] -> ()
  | _ when over_depth depth -> print_dots ppf
  | [x] -> f depth ppf x
  | x::xs -> 
      fprintf ppf "%a%a%a" 
	(f depth) x
	sep ()
	(print_list_with_depth (incr_depth depth) sep f) xs

let print_tuple f ppf v = 
  print_list (fun ppf () -> fprintf ppf ",@ ") f ppf v

let base_printers : (Rtype.type_declaration * (base_printer list -> base_printer)) list =
  [ Rtype.int, (fun _ _ ppf v -> fprintf ppf "%i" (Obj.obj v));
    Rtype.char, (fun _ _ ppf v -> fprintf ppf "%C" (Obj.obj v));
    Rtype.string, (fun _ _ ppf v -> fprintf ppf "%S" (Obj.obj v));
    Rtype.float, (fun _ _ ppf v -> fprintf ppf "%F" (Obj.obj v));
    Rtype.bool, (fun _ _ ppf v -> fprintf ppf "%B" (Obj.obj v));
    Rtype.unit, (fun _ _ ppf v -> fprintf ppf "()");
    Rtype.exn, (fun _ _ ppf v -> fprintf ppf "<exn>");
    Rtype.nativeint, (fun _ _ ppf v -> fprintf ppf "%nd" (Obj.obj v));
    Rtype.int32, (fun _ _ ppf v -> fprintf ppf "%ld" (Obj.obj v));
    Rtype.int64, (fun _ _ ppf v -> fprintf ppf "%Ld" (Obj.obj v));
    
    Rtype.list, (fun printers depth ppf v ->
      if over_depth depth then fprintf ppf "..." else
      match printers with
      | [printer] ->
	  begin match Obj.obj v with
	  | [] -> fprintf ppf "[]"
	  | v -> 
	      fprintf ppf "@[<2>[ %a ]@]" 
		(print_list_with_depth 
		   depth (fun ppf () -> fprintf ppf ";@ ") printer) v
	  end
      | _ -> assert false);

    Rtype.array, (fun printers depth ppf v ->
      if over_depth depth then fprintf ppf "..." else
      match printers with
      | [printer] ->
	  begin match Array.to_list (Obj.obj v) with
	  | [] -> fprintf ppf "[||]"
	  | v -> 
	      fprintf ppf "@[<3>[| %a |]@]" 
		(print_list_with_depth
		   depth (fun ppf () -> fprintf ppf ";@ ") printer) v
	  end
      | _ -> assert false)
      
  ]

type printer = Rtype.type_expr -> depth_info -> Format.formatter -> Obj.t -> unit

let gen_print (self : printer) ty depth ppf v =

  if over_depth depth then print_dots ppf else
  let depth' = incr_depth depth in

  let rec gen_print ty ppf v =
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
  	  (print_tuple (fun ppf (t,v) -> self t depth' ppf v)) (bind 0 ts)
    | Tconstr ((_, Box decl), args) -> 
	try
	  (* call printers of base types *)
	  print_base ppf decl args v
	with
	| Not_found ->
	    match decl.type_kind with
	    | Type_abstract -> 
		begin match decl.type_manifest with
		| Some t ->
		    let s = List.combine decl.type_params args in
		    self (Rtype.subst s t) depth ppf v
		| None -> fprintf ppf "<abstr>"
		end
	    | Type_variant ([], _) -> 
		(* FIXME: exception lacks runtime definition currently *)
		pp_print_string ppf (Printexc.to_string (Obj.obj v : exn))
	    | Type_variant (cnstrs, _) -> 
		print_variant ppf decl args cnstrs v
	    | Type_record (labels, Record_regular, _) ->
		print_regular_record ppf decl args labels v
	    | Type_record (labels, Record_float, _) ->
		print_float_record ppf decl args labels v

  and print_base ppf decl args v =
    let printer = List.assq decl base_printers in
    let sub_printers = List.map self args in
    printer sub_printers depth ppf v

  and print_variant ppf decl args cnstrs v =
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
      | [sprt, field] -> fprintf ppf "@[%s (%a)@]" cnst (sprt depth') field
      | sprtfields -> 
	  let rec print_args ppf = function
	    | [] -> ()
	    | [p,v] -> p depth' ppf v
	    | (p,v)::pvs -> 
		fprintf ppf "%a,@,%a" 
		  (p depth') v 
		  print_args pvs
	  in
	  fprintf ppf "@[<2>%s@ (@[%a@])@]" cnst print_args sprtfields
    end

  and print_regular_record ppf decl args labels v =
    let sub = List.combine decl.type_params args in
    (* sub printer prints label and content *)
    let labels_and_subprinters = 
      List.map (fun (l,_,t) -> l, self (Rtype.subst sub t) depth') labels
    in
    let fields =
      let rec get_fields p = function
	| [] -> []
	| _::xs -> (Obj.field v p) :: get_fields (p+1) xs
      in
      get_fields 0 labels
    in
    fprintf ppf "@[<hov2>{ %a }@]" 
      (print_list (fun ppf () -> fprintf ppf ";@ ") 
	 (fun ppf ((l,p),v) ->
	   fprintf ppf "@[<2>%s=@ %a@]" l p (Obj.repr v)))
      (List.combine labels_and_subprinters fields)

  and print_float_record ppf decl args labels v =
    let sub = List.combine decl.type_params args in
    (* sub printer prints label and content *)
    let labels_and_subprinters = 
      List.map (fun (l,_,t) -> 
	l, self (Rtype.subst sub t) depth') labels
    in
    let values_as_float = 
      Array.to_list (Obj.obj v : float array) 
    in
    fprintf ppf "@[<hov2>{ %a }@]" 
      (print_list (fun ppf () -> fprintf ppf ";@ ") 
	 (fun ppf ((l,p),v) ->
	   fprintf ppf "@[<2>%s=@ %a@]" l p (Obj.repr v)))
      (List.combine labels_and_subprinters values_as_float)
  in

  gen_print ty ppf v 
    
let rec printer ty depth ppf v = gen_print printer ty depth ppf v

let printer ty ?(max_depth=20) ppf v = 
  printer ty { max_depth=max_depth; cur_depth=0 } ppf v

let eprinter ty ?max_depth v = Format.eprintf "%a@." (printer ty ?max_depth) v 

generic val print : {'a} => ?max_depth: int -> formatter -> 'a -> unit = printer
generic val eprint : {'a} => ?max_depth: int -> 'a -> unit = eprinter

