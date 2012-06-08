(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*                  Projet Cristal, INRIA Rocquencourt                 *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format
open Outcometree
open Types
open Asttypes
open Printast

exception Ellipsis

let cautious f ppf arg =
  try f ppf arg with
    Ellipsis -> fprintf ppf "..."

let rec print_ident ppf =
  function
    Oide_ident s -> fprintf ppf "%s" s
  | Oide_dot (id, s) -> fprintf ppf "%a.%s" print_ident id s
  | Oide_apply (id1, id2) ->
      fprintf ppf "%a(%a)" print_ident id1 print_ident id2

let parenthesized_ident name =
  (List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"])
  ||
  (match name.[0] with
      'a'..'z' | 'A'..'Z' | '\223'..'\246' | '\248'..'\255' | '_' ->
        false
    | _ -> true)

let value_ident ppf name =
  if parenthesized_ident name then
    fprintf ppf "( %s )" name
  else
    fprintf ppf "%s" name

(* Values *)

let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i+1)
    | _ -> s
  in loop 0

let float_repres f =
  match classify_float f with
    FP_nan -> "nan"
  | FP_infinite ->
      if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f in
        if f = float_of_string s1 then s1 else
        let s2 = Printf.sprintf "%.15g" f in
        if f = float_of_string s2 then s2 else
        Printf.sprintf "%.18g" f
      in valid_float_lexeme float_val

let parenthesize_if_neg ppf fmt v isneg =
  if isneg then pp_print_char ppf '(';
  fprintf ppf fmt v;
  if isneg then pp_print_char ppf ')'

let print_out_value ppf tree =
  let rec print_tree_1 ppf =
    function
    | Oval_constr (name, [param]) ->
        fprintf ppf "@[<1>%a@ %a@]" print_ident name print_constr_param param
    | Oval_constr (name, (_ :: _ as params)) ->
        fprintf ppf "@[<1>%a@ (%a)@]" print_ident name
          (print_tree_list print_tree_1 ",") params
    | Oval_variant (name, Some param) ->
        fprintf ppf "@[<2>`%s@ %a@]" name print_constr_param param
    | tree -> print_simple_tree ppf tree
  and print_constr_param ppf = function
    | Oval_int i -> parenthesize_if_neg ppf "%i" i (i < 0)
    | Oval_int32 i -> parenthesize_if_neg ppf "%lil" i (i < 0l)
    | Oval_int64 i -> parenthesize_if_neg ppf "%LiL" i (i < 0L)
    | Oval_nativeint i -> parenthesize_if_neg ppf "%nin" i (i < 0n)
    | Oval_float f -> parenthesize_if_neg ppf "%s" (float_repres f) (f < 0.0)
    | tree -> print_simple_tree ppf tree
  and print_simple_tree ppf =
    function
      Oval_int i -> fprintf ppf "%i" i
    | Oval_int32 i -> fprintf ppf "%lil" i
    | Oval_int64 i -> fprintf ppf "%LiL" i
    | Oval_nativeint i -> fprintf ppf "%nin" i
    | Oval_float f -> fprintf ppf "%s" (float_repres f)
    | Oval_char c -> fprintf ppf "%C" c
    | Oval_string s ->
        begin try fprintf ppf "%S" s with
          Invalid_argument "String.create" -> fprintf ppf "<huge string>"
        end
    | Oval_list tl ->
        fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_array tl ->
        fprintf ppf "@[<2>[|%a|]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_constr (name, []) -> print_ident ppf name
    | Oval_variant (name, None) -> fprintf ppf "`%s" name
    | Oval_stuff s -> fprintf ppf "%s" s
    | Oval_record fel ->
        fprintf ppf "@[<1>{%a}@]" (cautious (print_fields true)) fel
    | Oval_ellipsis -> raise Ellipsis
    | Oval_printer f -> f ppf
    | Oval_tuple tree_list ->
        fprintf ppf "@[<1>(%a)@]" (print_tree_list print_tree_1 ",") tree_list
    | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree_1) tree
  and print_fields first ppf =
    function
      [] -> ()
    | (name, tree) :: fields ->
        if not first then fprintf ppf ";@ ";
        fprintf ppf "@[<1>%a@ =@ %a@]" print_ident name (cautious print_tree_1)
          tree;
        print_fields false ppf fields
  and print_tree_list print_item sep ppf tree_list =
    let rec print_list first ppf =
      function
        [] -> ()
      | tree :: tree_list ->
          if not first then fprintf ppf "%s@ " sep;
          print_item ppf tree;
          print_list false ppf tree_list
    in
    cautious (print_list true) ppf tree_list
  in
  cautious print_tree_1 ppf tree

let out_value = ref print_out_value

(* Types *)

let rec print_list_init pr sep ppf =
  function
    [] -> ()
  | a :: l -> sep ppf; pr ppf a; print_list_init pr sep ppf l

let rec print_list pr sep ppf =
  function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

let pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let pr_vars =
  print_list (fun ppf s -> fprintf ppf "'%s" s) (fun ppf -> fprintf ppf "@ ")

let rec print_out_type ppf =
  function
  | Otyp_alias (ty, s) ->
      fprintf ppf "@[%a@ as '%s@]" print_out_type ty s
  | Otyp_poly (sl, ty) ->
      fprintf ppf "@[<hov 2>%a.@ %a@]"
        pr_vars sl
        print_out_type ty
  | ty ->
      print_out_type_1 ppf ty

and print_out_type_1 ppf =
  function
    Otyp_arrow (lab, ty1, ty2) ->
      fprintf ppf "@[%s%a ->@ %a@]" (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty1 print_out_type_1 ty2
  | ty -> print_out_type_2 ppf ty
and print_out_type_2 ppf =
  function
    Otyp_tuple tyl ->
      fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type " *") tyl
  | ty -> print_simple_out_type ppf ty
and print_simple_out_type ppf =
  function
    Otyp_class (ng, id, tyl) ->
      fprintf ppf "@[%a%s#%a@]" print_typargs tyl (if ng then "_" else "")
        print_ident id
  | Otyp_constr (id, tyl) ->
      fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
  | Otyp_object (fields, rest) ->
      fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
  | Otyp_stuff s -> fprintf ppf "%s" s
  | Otyp_var (ng, s) -> fprintf ppf "'%s%s" (if ng then "_" else "") s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let print_present ppf =
        function
          None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      let print_fields ppf =
        function
          Ovar_fields fields ->
            print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_name (id, tyl) ->
            fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
      in
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a ]@]" (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "< "
         else if tags = None then "> " else "? ")
        print_fields row_fields
        print_present tags
  | Otyp_alias _ | Otyp_poly _ | Otyp_arrow _ | Otyp_tuple _ as ty ->
      fprintf ppf "@[<1>(%a)@]" print_out_type ty
  | Otyp_abstract | Otyp_sum _ | Otyp_record _ | Otyp_manifest (_, _) -> ()
  | Otyp_module (p, n, tyl) ->
      fprintf ppf "@[<1>(module %s" p;
      let first = ref true in
      List.iter2
        (fun s t ->
          let sep = if !first then (first := false; "with") else "and" in
          fprintf ppf " %s type %s = %a" sep s print_out_type t
        )
        n tyl;
      fprintf ppf ")@]"
and print_fields rest ppf =
  function
    [] ->
      begin match rest with
        Some non_gen -> fprintf ppf "%s.." (if non_gen then "_" else "")
      | None -> ()
      end
  | [s, t] ->
      fprintf ppf "%s : %a" s print_out_type t;
      begin match rest with
        Some _ -> fprintf ppf ";@ "
      | None -> ()
      end;
      print_fields rest ppf []
  | (s, t) :: l ->
      fprintf ppf "%s : %a;@ %a" s print_out_type t (print_fields rest) l
and print_row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " of@ &@ "
    else if tyl <> [] then fprintf ppf " of@ "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of (print_typlist print_out_type " &")
    tyl
and print_typlist print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      fprintf ppf "%a%s@ %a" print_elem ty sep (print_typlist print_elem sep)
        tyl
and print_typargs ppf =
  function
    [] -> ()
  | [ty1] -> fprintf ppf "%a@ " print_simple_out_type ty1
  | tyl -> fprintf ppf "@[<1>(%a)@]@ " (print_typlist print_out_type ",") tyl

let out_type = ref print_out_type

(* Class types *)

let type_parameter ppf (ty, (co, cn)) =
  fprintf ppf "%s'%s" (if not cn then "+" else if not co then "-" else "")
    (*if co then if cn then "!" else "+" else if cn then "-" else "?"*)
    ty

let print_out_class_params ppf =
  function
    [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (print_list type_parameter (fun ppf -> fprintf ppf ", "))
        tyl

let rec print_out_class_type ppf =
  function
    Octy_constr (id, tyl) ->
      let pr_tyl ppf =
        function
          [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ " (print_typlist !out_type ",") tyl
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl print_ident id
  | Octy_fun (lab, ty, cty) ->
      fprintf ppf "@[%s%a ->@ %a@]" (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty print_out_class_type cty
  | Octy_signature (self_ty, csil) ->
      let pr_param ppf =
        function
          Some ty -> fprintf ppf "@ @[(%a)@]" !out_type ty
        | None -> ()
      in
      fprintf ppf "@[<hv 2>@[<2>object%a@]@ %a@;<1 -2>end@]" pr_param self_ty
        (print_list print_out_class_sig_item (fun ppf -> fprintf ppf "@ "))
        csil
and print_out_class_sig_item ppf =
  function
    Ocsg_constraint (ty1, ty2) ->
      fprintf ppf "@[<2>constraint %a =@ %a@]" !out_type ty1
        !out_type ty2
  | Ocsg_method (name, priv, virt, ty) ->
      fprintf ppf "@[<2>method %s%s%s :@ %a@]"
        (if priv then "private " else "") (if virt then "virtual " else "")
        name !out_type ty
  | Ocsg_value (name, mut, vr, ty) ->
      fprintf ppf "@[<2>val %s%s%s :@ %a@]"
        (if mut then "mutable " else "")
        (if vr then "virtual " else "")
        name !out_type ty

let out_class_type = ref print_out_class_type


(* Contract *)

let rec print_out_core_contract_desc ppf = 
  function
    Tctr_pred  (x, e, exnop) -> 
    begin match exnop with 
    | None -> fprintf ppf "@[{@ %a@ |@ %a@ }@]" 
                         Ident.print x
                         print_out_expression e
    | Some exns ->  fprintf ppf "@[{@ %a@ |@ %a@ }@ with@  exception@ %a @]" 
                         Ident.print x
                         print_out_expression e                    
                         bindings exns
    end
  | Tctr_arrow (xop, c1, c2) -> 
    begin
      match xop with
      | None -> fprintf ppf "@[%a@ -> @ %a@]" 
                print_out_core_contract c1
                print_out_core_contract c2
      | Some x -> fprintf ppf "@[%a:(%a)@ -> @ %a@]" 
                Ident.print x
                print_out_core_contract c1
                print_out_core_contract c2
    end
  | Tctr_tuple (cs) -> fprintf ppf "@[%a@]" 
                       (print_list_init print_out_dep_core_contract
                                  (fun ppf -> fprintf ppf "*@ "))
                                   cs
  | Tctr_constr (i, cdesc, cs) -> fprintf ppf "@[%a@ of@%a@]" 
                       Path.print i
                       (print_list_init print_out_dep_core_contract
                                  (fun ppf -> fprintf ppf "*@ "))
                                   cs
  | Tctr_and (c1, c2) -> fprintf ppf "@[%a@ \n and \n %a@]"
                        print_out_core_contract c1
                        print_out_core_contract c2
  | Tctr_or (c1, c2) -> fprintf ppf "@[%a@ \n or \n %a@]"
                        print_out_core_contract c1
                        print_out_core_contract c2
  | Tctr_typconstr (i, cs) -> fprintf ppf "@[%a@ of@%a@]" 
                       Path.print i
                       (print_list_init print_out_core_contract
                                  (fun ppf -> fprintf ppf "*@ "))
                                   cs
  | Tctr_var (v) ->  fprintf ppf "@[[%a]@]" Ident.print v
  | Tctr_poly (vs, c) -> fprintf ppf "@[%a.%a@]" 
                         (print_list_init Ident.print
                                  (fun ppf -> fprintf ppf ",@ "))
                                   vs
                         print_out_core_contract c

and print_out_core_contract ppf c = 
      print_out_core_contract_desc ppf c.contract_desc

and print_out_dep_core_contract ppf = function (vo, c) ->
      match vo with
      | None -> fprintf ppf "@[%a@]" 
                print_out_core_contract c
      | Some x -> fprintf ppf "@[%s:(%a)@]" (Ident.name x)
                  print_out_core_contract c

and print_out_contract_declaration ppf cdecl = 
      print_out_core_contract_desc ppf cdecl.ttopctr_desc.contract_desc

and top_bindings ppf p_e_list = 
	let spc = ref false in
        List.iter
	  (fun (pi, ei) -> 
	    if !spc then fprintf ppf "@ " else spc := true;
	    fprintf ppf "@[<3>%a@ =@ %a@]" print_out_pattern pi 
	      print_out_expression ei)
        p_e_list

and bindings ppf p_e_list = 
	let spc = ref false in
        List.iter
	  (fun (pi, ei) -> 
	    if !spc then fprintf ppf "@ " else spc := true;
	    fprintf ppf "@[<hov>| %a ->@ @[<hv>%a@]@]" print_out_pattern pi 
	      print_out_expression ei)
        p_e_list


and print_out_expression_desc ppf = 
  function
    Texp_ident (path, vd) -> Path.print ppf path
  | Texp_constant (c) -> fmt_constant ppf c
  | Texp_let (rflag, pat_exp_list, e) -> 
       begin 
         match rflag with
             | Recursive -> 
		 fprintf ppf "@[<hv>let rec@ %a@ in %a@]"
		   top_bindings pat_exp_list
		   print_out_expression e
             | Nonrecursive -> 
		 fprintf ppf "@[<hv>let %a@ in %a@]"
		   top_bindings pat_exp_list
		   print_out_expression e
             | Default -> 
		 fprintf ppf "@[<hv>let %a@]"
		   top_bindings pat_exp_list
       end 
  | Texp_function (pat_exp_list, pl) -> 
       fprintf ppf "@[<v>function@ @[ %a @] @]" bindings pat_exp_list
  | Texp_apply (e1, eop_opl_list) -> 
       let rec picke xs = match xs with 
                      | [] -> []
                      | (x::ss) -> begin match x with
                                   | (Some e, opl) -> e::(picke ss)
			           | (None, _) -> picke ss 
                                   end
       in
       let es = picke eop_opl_list in
       fprintf ppf "@[<hov>Texp_apply((%a)@ (%a)) @]" 
       print_out_expression e1
       (print_list_init print_out_expression  
	               (fun ppf -> fprintf ppf "@ "))
                       es
  | Texp_match (e, pat_exp_list, pl) -> 
       fprintf ppf "@[<hv>match %a with@ %a @]" 
       print_out_expression e
       bindings pat_exp_list
  | Texp_tuple (es) -> 
       fprintf ppf "@[<hv>(%a)@]"
       (print_list print_out_expression
	               (fun ppf -> fprintf ppf ",@ "))
                       es
  | Texp_construct (path, constr_desc, es) -> 
       begin match constr_desc.cstr_tag with
       | Cstr_constant (i) -> 
	   let c = if i>0 
	   then begin if path = Predef.path_bool 
	              then "true"
	              else Path.name path
                end
	   else begin if path = Predef.path_bool then "false"
	              else if path = Predef.path_list then "[]"
	              else Path.name path
                end
           in fprintf ppf "@[%s@]" c
       | Cstr_block (i) -> 
	   let c = if path = Predef.path_list
	           then "Cons"
                   else Path.name path
           in
	   fprintf ppf "@[%s@ (%a)@]" c (print_list print_out_expression  
	               (fun ppf -> fprintf ppf ",@ ")) es
       | Cstr_exception (p)->
         fprintf ppf "@[exception@ %s@ (%a)@]" (Path.name p) 
           (print_list print_out_expression (fun ppf -> fprintf ppf ",@ ")) es
      end
  | Texp_ifthenelse (e, then_exp, else_expop) -> 
     begin
       match else_expop with
       |  None -> 
	   fprintf ppf "@[<hv>if %a@ then %a @]" 
             print_out_expression e
             print_out_expression then_exp
       | Some else_exp -> 
	   fprintf ppf "@[<hv>if %a@ then %a@ else %a@]" 
             print_out_expression e
             print_out_expression then_exp
	     print_out_expression else_exp
     end
  | Texp_try (e, pat_exp_list) -> 
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@)@]"
	print_out_expression e bindings pat_exp_list
  | Texp_sequence(e1, e2) ->
      fprintf ppf "@[<2>(%a@ ; %a)@]" 
        print_out_expression e1
        print_out_expression e2
  | Texp_while (e1, e2) ->
      fprintf ppf "@[<2>(while@ (%a)@ @[{%a@]})@]" 
        print_out_expression e1
        print_out_expression e2
  | Texp_for (id, e1, e2, dir_flag, e3) -> 
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
	Ident.print id
	print_out_expression e1
	(match dir_flag with Upto -> "to" | Downto -> "downto")
	print_out_expression e2
	print_out_expression e3
  | Texp_array (e_list) -> 
      fprintf ppf "@[ %a @]"
        (print_list_init print_out_expression 
	   (fun ppf -> fprintf ppf ","))
        e_list
  | Texp_when (e1, e2) -> 
      fprintf ppf "@[<2>(when@ (%a)@ @[{%a@]})@]" 
        print_out_expression e1
        print_out_expression e2
  | Texp_send (e, m) -> fprintf ppf "@[%a@]" print_out_expression e
  | Texp_assert (e) -> fprintf ppf "@[%a@]" print_out_expression e
  | Texp_assertfalse -> fprintf ppf "%s" "assertfalse"
  | Texp_lazy (e) -> fprintf ppf "@[%a@]" print_out_expression e
  | Texp_contract (ccontract, e, r1, r2) ->
      begin
      match (r1.exp_desc, r2.exp_desc) with
        (Texp_bad _, Texp_unr _) -> 
	  fprintf ppf "@[(%a@ |>@ %a)@]" print_out_expression e
	    print_out_core_contract ccontract
      | (Texp_unr _, Texp_bad _) ->
	  fprintf ppf "@[(%a@ <|@ %a)@]" print_out_expression e
	    print_out_core_contract ccontract
      | (_,_) -> fprintf ppf "%s" "typing/oprint.ml: contract not done yet"
      end
  | Texp_bad (bl) -> fprintf ppf "BAD %a" print_blame bl
  | Texp_unr (bl) -> fprintf ppf "UNR %a" print_blame bl
(*
  | Texp_variant (lbl, expop) -> fprintf ppf "%s" "Texp_variant"
  | Texp_record of (label_description * expression) list * expression option
  | Texp_field of expression * label_description
  | Texp_setfield of expression * label_description * expression
  | Texp_when of expression * expression
  | Texp_send of expression * meth
  | Texp_new of Path.t * class_declaration
  | Texp_instvar of Path.t * Path.t
  | Texp_setinstvar of Path.t * Path.t * expression
  | Texp_override of Path.t * (Path.t * expression) list
  | Texp_letmodule of Ident.t * module_expr * expression 
  | Texp_object of class_structure * class_signature * string list
*)
  | others -> fprintf ppf "%s" "typing/oprint.ml: not done yet"

and print_blame ppf bl = match bl with
    Caller (loc, pathopt, path) -> 
      begin 
         match pathopt with
           None -> fprintf ppf "%s" "(Blame _)"
         | Some p -> fprintf ppf "(Blame %s +> %s)" (Path.name p) (Path.name path)
      end
  | Callee (loc, path) -> 
       fprintf ppf "(Blame callee %s)" (Path.name path)
  | UnknownBlame -> fprintf ppf "%s" "UnknownBlame"

and print_out_pattern ppf pat = print_out_pattern_desc ppf pat.pat_desc

and print_out_expression ppf exp = print_out_expression_desc ppf exp.exp_desc

and print_out_pattern_desc ppf = 
  function 
    Tpat_any -> fprintf ppf "%s" "_"
  | Tpat_var (id) -> fprintf ppf "%s" (Ident.unique_toplevel_name id)
  | Tpat_alias (pat, id) -> 
      fprintf ppf "%a, @%s" print_out_pattern pat (Ident.unique_toplevel_name id)
  | Tpat_constant (c) -> fprintf ppf "%a" fmt_constant c
  | Tpat_tuple (pat_list) -> 
      fprintf ppf "@[(%a) @]"
        (print_list_init print_out_pattern
	   (fun ppf -> fprintf ppf ","))
        pat_list
  | Tpat_construct (path, constr_desc, pat_list) ->
      begin match constr_desc.cstr_tag with
       | Cstr_constant (i) -> 
	   let c = if i>0 
	   then begin if path = Predef.path_bool 
	              then "true"
	              else Path.name path
                end
	   else begin if path = Predef.path_bool then "false"
	              else if path = Predef.path_list then "[]"
	              else Path.name path
                end
           in fprintf ppf "@[%s@]" c
       | Cstr_block (i) -> 
	   let c = if path = Predef.path_list
	           then "Cons"
                   else Path.name path
           in
           fprintf ppf "@[%s@ (%a)@]" c (print_list print_out_pattern
	               (fun ppf -> fprintf ppf ",@ ")) pat_list
       | Cstr_exception (p)->
         fprintf ppf "@[exception@ %s@ (%a)@]" (Path.name p) 
           (print_list print_out_pattern (fun ppf -> fprintf ppf ",@ ")) pat_list
      end
  | Tpat_variant (lbl, patop, rdescref) ->
      fprintf ppf "%s" "typing/oprint.ml: not done yet"
  | Tpat_record (lbldesc_pat_list) -> 
      fprintf ppf "@[ %a @]"
        (print_list_init print_out_pattern
	   (fun ppf -> fprintf ppf ","))
        (List.map (fun (b,c) -> c) lbldesc_pat_list)
  | Tpat_array (pat_list) -> 
      fprintf ppf "@[ %a @]"
        (print_list_init print_out_pattern
	   (fun ppf -> fprintf ppf ","))
	pat_list
  | Tpat_or (pat1, pat2, rdescop) -> fprintf ppf "%a, @%a" 
	                      print_out_pattern pat1
	                      print_out_pattern pat2
  | Tpat_lazy (pat) -> fprintf ppf "%a" print_out_pattern pat

                                             
let out_contract_declaration = ref print_out_contract_declaration
let out_core_contract = ref print_out_core_contract
let out_expression = ref print_out_expression
let out_expression_desc = ref print_out_expression_desc
let out_pattern_desc = ref print_out_pattern_desc


(* Signature *)

let out_module_type = ref (fun _ -> failwith "Oprint.out_module_type")
let out_sig_item = ref (fun _ -> failwith "Oprint.out_sig_item")
let out_signature = ref (fun _ -> failwith "Oprint.out_signature")

let rec print_out_module_type ppf =
  function
    Omty_abstract -> ()
  | Omty_functor (name, mty_arg, mty_res) ->
      fprintf ppf "@[<2>functor@ (%s : %a) ->@ %a@]" name
        print_out_module_type mty_arg print_out_module_type mty_res
  | Omty_ident id -> fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
      fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" !out_signature sg
and print_out_signature ppf =
  function
    [] -> ()
  | [item] -> !out_sig_item ppf item
  | item :: items ->
      fprintf ppf "%a@ %a" !out_sig_item item print_out_signature items
and print_out_sig_item ppf =
  function
    Osig_class (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %a%s@ :@ %a@]"
        (if rs = Orec_next then "and" else "class")
        (if vir_flag then " virtual" else "") print_out_class_params params
        name !out_class_type clt
  | Osig_class_type (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %a%s@ =@ %a@]"
        (if rs = Orec_next then "and" else "class type")
        (if vir_flag then " virtual" else "") print_out_class_params params
        name !out_class_type clt
  | Osig_exception (id, tyl) ->
      fprintf ppf "@[<2>exception %a@]" print_out_constr (id, tyl)
  | Osig_modtype (name, Omty_abstract) ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype (name, mty) ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name !out_module_type mty
  | Osig_module (name, mty, rs) ->
      fprintf ppf "@[<2>%s %s :@ %a@]"
        (match rs with Orec_not -> "module"
                     | Orec_first -> "module rec"
                     | Orec_next -> "and")
        name !out_module_type mty
  | Osig_type(td, rs) ->
        print_out_type_decl
          (if rs = Orec_next then "and" else "type")
          ppf td
  | Osig_value (name, ty, prims) ->
      let kwd = if prims = [] then "val" else "external" in
      let pr_prims ppf =
        function
          [] -> ()
        | s :: sl ->
            fprintf ppf "@ = \"%s\"" s;
            List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
      in
      fprintf ppf "@[<2>%s %a :@ %a%a@]" kwd value_ident name !out_type
        ty pr_prims prims
  | Osig_contract (id, decl, rs) -> 
      let kwd = "contract" in
      fprintf ppf "@[<2>%s %a =@ %a@]" kwd value_ident id 
              print_out_core_contract_desc (decl.ttopctr_desc).contract_desc


and print_out_type_decl kwd ppf (name, args, ty, priv, constraints) =
  let print_constraints ppf params =
    List.iter
      (fun (ty1, ty2) ->
         fprintf ppf "@ @[<2>constraint %a =@ %a@]" !out_type ty1
           !out_type ty2)
      params
  in
  let type_defined ppf =
    match args with
      [] -> fprintf ppf "%s" name
    | [arg] -> fprintf ppf "@[%a@ %s@]" type_parameter arg name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list type_parameter (fun ppf -> fprintf ppf ",@ ")) args name
  in
  let print_manifest ppf =
    function
      Otyp_manifest (ty, _) -> fprintf ppf " =@ %a" !out_type ty
    | _ -> ()
  in
  let print_name_args ppf =
    fprintf ppf "%s %t%a" kwd type_defined print_manifest ty
  in
  let ty =
    match ty with
      Otyp_manifest (_, ty) -> ty
    | _ -> ty
  in
  let print_private ppf = function
    Asttypes.Private -> fprintf ppf " private"
  | Asttypes.Public -> () in
  let rec print_out_tkind ppf = function
  | Otyp_abstract -> ()
  | Otyp_record lbls ->
      fprintf ppf " =%a {%a@;<1 -2>}"
        print_private priv
        (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
  | Otyp_sum constrs ->
      fprintf ppf " =%a@;<1 2>%a"
        print_private priv
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
  | ty ->
      fprintf ppf " =%a@;<1 2>%a"
        print_private priv
        !out_type ty
  in
  fprintf ppf "@[<2>@[<hv 2>%t%a@]%a@]"
    print_name_args
    print_out_tkind ty
    print_constraints constraints
and print_out_constr ppf (name, tyl) =
  match tyl with
    [] -> fprintf ppf "%s" name
  | _ ->
      fprintf ppf "@[<2>%s of@ %a@]" name
        (print_typlist print_simple_out_type " *") tyl
and print_out_label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s%s :@ %a@];" (if mut then "mutable " else "") name
    !out_type arg

let _ = out_module_type := print_out_module_type
let _ = out_signature := print_out_signature
let _ = out_sig_item := print_out_sig_item

(* Phrases *)

let print_out_exception ppf exn outv =
  match exn with
    Sys.Break -> fprintf ppf "Interrupted.@."
  | Out_of_memory -> fprintf ppf "Out of memory during evaluation.@."
  | Stack_overflow ->
      fprintf ppf "Stack overflow during evaluation (looping recursion?).@."
  | _ -> fprintf ppf "@[Exception:@ %a.@]@." !out_value outv

let rec print_items ppf =
  function
    [] -> ()
  | (tree, valopt) :: items ->
      begin match valopt with
        Some v ->
          fprintf ppf "@[<2>%a =@ %a@]" !out_sig_item tree
            !out_value v
      | None -> fprintf ppf "@[%a@]" !out_sig_item tree
      end;
      if items <> [] then fprintf ppf "@ %a" print_items items

let print_out_phrase ppf =
  function
    Ophr_eval (outv, ty) ->
      fprintf ppf "@[- : %a@ =@ %a@]@." !out_type ty !out_value outv
  | Ophr_signature [] -> ()
  | Ophr_signature items -> fprintf ppf "@[<v>%a@]@." print_items items
  | Ophr_exception (exn, outv) -> print_out_exception ppf exn outv

let out_phrase = ref print_out_phrase
