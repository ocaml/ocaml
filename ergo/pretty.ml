(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2010                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*     Alain Mebsout                                                      *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

open Format
open Why_ptree

(* not quite ''pretty''-printing, but helps for quick debugging *)

let rec print_term fmt t = 
  print_term_desc fmt t.tt_desc
and print_term_desc fmt =  function
  | TTconst Ttrue -> fprintf fmt "true"
  | TTconst Tfalse -> fprintf fmt "false"
  | TTconst Tint s -> fprintf fmt "%s" s
  | TTconst Treal s -> fprintf fmt "%s" (Num.string_of_num s)
  | TTvar x -> Symbols.print fmt x
  | TTinfix(t1,op,t2) -> 
      fprintf fmt "%a%a%a" print_term t1 Symbols.print op print_term t2
  | TTapp(f,l) -> 
      fprintf fmt "%a(%a)" Symbols.print f print_term_list l
  | _ -> failwith "not implemented"
and print_term_list fmt = List.iter (fprintf fmt "%a," print_term)

let rec print_atom fmt = function
  | TAtrue -> fprintf fmt "@true"
  | TAfalse -> fprintf fmt "@false"
  | TAeq tl -> fprintf fmt "=(%a)" print_term_list tl
  | TAneq tl -> fprintf fmt "<>(%a)" print_term_list tl
  | TAle tl -> fprintf fmt "<=(%a)" print_term_list tl
  | TAlt tl -> fprintf fmt "<(%a)" print_term_list tl
  | TApred t -> fprintf fmt "%a" print_term t 
  | TAbuilt(s, tl) -> fprintf fmt "%s(%a)" (Hstring.view s) print_term_list tl

let print_op fmt = function
  | OPand -> fprintf fmt "/\\"
  | OPor -> fprintf fmt "\\/"
  | OPimp -> fprintf fmt "=>"
  | OPnot -> fprintf fmt "~"
  | OPif t -> fprintf fmt "if (%a) " print_term t
  | OPiff -> fprintf fmt "<=>"

let rec print_form fmt = function
  | TFatom a -> fprintf fmt "%a" print_atom a
  | TFop(op,tl) -> fprintf fmt "%a(%a)" print_op op print_form_list tl
  | TFforall qf -> fprintf fmt "forall ..., %a" print_form qf.qf_form
  | TFexists qf -> fprintf fmt "exists ..., %a" print_form qf.qf_form
  | TFlet (up,var,t,f) -> fprintf fmt "let ..., %a" print_form f
  | TFnamed(lbl, f) -> fprintf fmt "%s:%a" (Hstring.view lbl) print_form f
and print_form_list fmt = List.iter (fprintf fmt "%a," print_form)
