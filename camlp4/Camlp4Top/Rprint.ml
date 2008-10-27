(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)



(* There is a few Obj.magic due to the fact that we no longer have compiler
   files like Parsetree, Location, Longident but Camlp4_import that wrap them to
   avoid name clashing. *)
module Toploop : sig
  open Format;
  open Camlp4_import;
  value print_out_value :
    ref (formatter -> Outcometree.out_value -> unit);
  value print_out_type :
    ref (formatter -> Outcometree.out_type -> unit);
  value print_out_class_type :
    ref (formatter -> Outcometree.out_class_type -> unit);
  value print_out_module_type :
    ref (formatter -> Outcometree.out_module_type -> unit);
  value print_out_sig_item :
    ref (formatter -> Outcometree.out_sig_item -> unit);
  value print_out_signature :
    ref (formatter -> list Outcometree.out_sig_item -> unit);
  value print_out_phrase :
    ref (formatter -> Outcometree.out_phrase -> unit);
end = struct
  open Toploop;
  value print_out_value = Obj.magic print_out_value;
  value print_out_type = Obj.magic print_out_type;
  value print_out_class_type = Obj.magic print_out_class_type;
  value print_out_module_type = Obj.magic print_out_module_type;
  value print_out_sig_item = Obj.magic print_out_sig_item;
  value print_out_signature = Obj.magic print_out_signature;
  value print_out_phrase = Obj.magic print_out_phrase;
end;

(* This file originally come from typing/oprint.ml *)

open Format;
open Camlp4_import.Outcometree;
open Camlp4;

exception Ellipsis;
value cautious f ppf arg =
  try f ppf arg with [ Ellipsis -> fprintf ppf "..." ]
;

value rec print_ident ppf =
  fun
  [ Oide_ident s -> fprintf ppf "%s" s
  | Oide_dot id s -> fprintf ppf "%a.%s" print_ident id s
  | Oide_apply id1 id2 ->
      fprintf ppf "%a(%a)" print_ident id1 print_ident id2 ]
;

value value_ident ppf name =
  if List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"]
  then
    fprintf ppf "( %s )" name
  else
    match name.[0] with
    [ 'a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' ->
        fprintf ppf "%s" name
    | _ -> fprintf ppf "( %s )" name ]
;

(* Values *)

value print_out_value ppf tree =
  let rec print_tree ppf =
    fun
    [ Oval_constr name ([_ :: _] as params) ->
        fprintf ppf "@[<1>%a@ %a@]" print_ident name
          (print_tree_list print_simple_tree "") params
    | Oval_variant name (Some param) ->
        fprintf ppf "@[<2>`%s@ %a@]" name print_simple_tree param
    | tree -> print_simple_tree ppf tree ]
  and print_simple_tree ppf =
    fun
    [ Oval_int i -> fprintf ppf "%i" i
    | Oval_int32 i -> fprintf ppf "%ldl" i
    | Oval_int64 i -> fprintf ppf "%LdL" i
    | Oval_nativeint i -> fprintf ppf "%ndn" i
    | Oval_float f -> fprintf ppf "%.12g" f
    | Oval_char c -> fprintf ppf "'%s'" (Char.escaped c)
    | Oval_string s ->
        try fprintf ppf "\"%s\"" (String.escaped s) with
        [ Invalid_argument "String.create" -> fprintf ppf "<huge string>" ]
    | Oval_list tl ->
        fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree ";") tl
    | Oval_array tl ->
        fprintf ppf "@[<2>[|%a|]@]" (print_tree_list print_tree ";") tl
    | Oval_constr (Oide_ident "true") [] -> fprintf ppf "True"
    | Oval_constr (Oide_ident "false") [] -> fprintf ppf "False"
    | Oval_constr name [] -> print_ident ppf name
    | Oval_variant name None -> fprintf ppf "`%s" name
    | Oval_stuff s -> fprintf ppf "%s" s
    | Oval_record fel ->
        fprintf ppf "@[<1>{%a}@]" (cautious (print_fields True)) fel
    | Oval_tuple tree_list ->
        fprintf ppf "@[(%a)@]" (print_tree_list print_tree ",") tree_list
    | Oval_ellipsis -> raise Ellipsis
    | Oval_printer f -> f ppf
    | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree) tree ]
  and print_fields first ppf =
    fun
    [ [] -> ()
    | [(name, tree) :: fields] ->
        let name =
          match name with
          [ Oide_ident "contents" -> Oide_ident "val"
          | x -> x ]
        in
        do {
          if not first then fprintf ppf ";@ " else ();
          fprintf ppf "@[<1>%a=@,%a@]" print_ident name (cautious print_tree)
            tree;
          print_fields False ppf fields
        } ]
  and print_tree_list print_item sep ppf tree_list =
    let rec print_list first ppf =
      fun
      [ [] -> ()
      | [tree :: tree_list] ->
          do {
            if not first then fprintf ppf "%s@ " sep else ();
            print_item ppf tree;
            print_list False ppf tree_list
          } ]
    in
    cautious (print_list True) ppf tree_list
  in
  cautious print_tree ppf tree
;

value rec print_list pr sep ppf =
  fun
  [ [] -> ()
  | [a] -> pr ppf a
  | [a :: l] -> do { pr ppf a; sep ppf; print_list pr sep ppf l } ]
;

value pr_vars =
  print_list (fun ppf s -> fprintf ppf "'%s" s) (fun ppf -> fprintf ppf "@ ")
;

value pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")
;

(* Types *)

value rec print_out_type ppf =
  fun
  [ Otyp_alias ty s -> fprintf ppf "@[%a as '%s@]" print_out_type ty s
  | ty -> print_out_type_1 ppf ty ]
and print_out_type_1 ppf =
  fun
  [ Otyp_arrow lab ty1 ty2 ->
      fprintf ppf "@[%a%a ->@ %a@]" print_ty_label lab
        print_out_type_2 ty1 print_out_type_1 ty2
  | Otyp_poly sl ty ->
      fprintf ppf "@[<hov 2>!%a.@ %a@]"
        pr_vars sl
        print_out_type ty
  | ty -> print_out_type_2 ppf ty ]
and print_out_type_2 ppf =
  fun
  [ Otyp_constr id ([_ :: _] as tyl) ->
      fprintf ppf "@[%a@;<1 2>%a@]" print_ident id
        (print_typlist print_simple_out_type "") tyl
  | ty -> print_simple_out_type ppf ty ]
and print_simple_out_type ppf =
  let rec print_tkind ppf =
  fun
  [ Otyp_var ng s -> fprintf ppf "'%s%s" (if ng then "_" else "") s
  | Otyp_constr id [] -> fprintf ppf "@[%a@]" print_ident id
  | Otyp_tuple tyl ->
      fprintf ppf "@[<1>(%a)@]" (print_typlist print_out_type " *") tyl
  | Otyp_stuff s -> fprintf ppf "%s" s
  | Otyp_variant non_gen row_fields closed tags ->
      let print_present ppf =
        fun
        [ None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l ]
      in
      let print_fields ppf =
        fun
        [ Ovar_fields fields ->
            print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_name id tyl ->
            fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id ]
      in
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a ]@]" (if non_gen then "_" else "")
        (if closed then if tags = None then "= " else "< "
         else if tags = None then "> "
         else "? ")
        print_fields row_fields
        print_present tags
  | Otyp_object fields rest ->
      fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
  | Otyp_class ng id tyl ->
      fprintf ppf "@[%a%s#%a@]" print_typargs tyl (if ng then "_" else "")
        print_ident id
  | Otyp_manifest ty1 ty2 ->
      fprintf ppf "@[<2>%a ==@ %a@]" print_out_type ty1 print_out_type ty2
  | Otyp_sum constrs ->
      fprintf ppf "@[<hv>[ %a ]@]"
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
  | Otyp_record lbls ->
      fprintf ppf "@[<hv 2>{ %a }@]"
        (print_list print_out_label (fun ppf -> fprintf ppf ";@ ")) lbls
  | Otyp_abstract -> fprintf ppf "<abstract>"
  | Otyp_alias _ _ | Otyp_poly _ _
  | Otyp_arrow _ _ _ | Otyp_constr _ [_ :: _] as ty ->
      fprintf ppf "@[<1>(%a)@]" print_out_type ty ]
  in
  print_tkind ppf
and print_out_constr ppf (name, tyl) =
  match tyl with
  [ [] -> fprintf ppf "%s" name
  | _ ->
      fprintf ppf "@[<2>%s of@ %a@]" name
        (print_typlist print_out_type " and") tyl ]
and print_out_label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s :@ %s%a@]" name (if mut then "mutable " else "")
    print_out_type arg
and print_fields rest ppf =
  fun
  [ [] ->
      match rest with
      [ Some non_gen -> fprintf ppf "%s.." (if non_gen then "_" else "")
      | None -> () ]
  | [(s, t)] ->
      do {
        fprintf ppf "%s : %a" s print_out_type t;
        match rest with
        [ Some _ -> fprintf ppf ";@ "
        | None -> () ];
        print_fields rest ppf []
      }
  | [(s, t) :: l] ->
      fprintf ppf "%s : %a;@ %a" s print_out_type t (print_fields rest) l ]
and print_row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " of@ &@ "
    else if tyl <> [] then fprintf ppf " of@ "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of (print_typlist print_out_type " &")
    tyl
and print_typlist print_elem sep ppf =
  fun
  [ [] -> ()
  | [ty] -> print_elem ppf ty
  | [ty :: tyl] ->
      fprintf ppf "%a%s@ %a" print_elem ty sep (print_typlist print_elem sep)
        tyl ]
and print_typargs ppf =
  fun
  [ [] -> ()
  | [ty1] -> fprintf ppf "%a@ " print_simple_out_type ty1
  | tyl ->
      fprintf ppf "@[<1>(%a)@]@ " (print_typlist print_out_type ",") tyl ]
and print_ty_label ppf lab =
  if lab <> "" then fprintf ppf "~%s:" lab else ()
;

value type_parameter ppf (ty, (co, cn)) =
  fprintf ppf "%s'%s" (if not cn then "+" else if not co then "-" else "")
    ty
;

value print_out_class_params ppf =
  fun
  [ [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (print_list type_parameter (fun ppf -> fprintf ppf ", "))
        tyl ]
;

(* Signature items *)

value rec print_out_class_type ppf =
  fun
  [ Octy_constr id tyl ->
      let pr_tyl ppf =
        fun
        [ [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ "
              (print_typlist Toploop.print_out_type.val ",") tyl ]
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl print_ident id
  | Octy_fun lab ty cty ->
      fprintf ppf "@[%a[ %a ] ->@ %a@]" print_ty_label lab
        Toploop.print_out_type.val ty print_out_class_type cty
  | Octy_signature self_ty csil ->
      let pr_param ppf =
        fun
        [ Some ty -> fprintf ppf "@ @[(%a)@]" Toploop.print_out_type.val ty
        | None -> () ]
      in
      fprintf ppf "@[<hv 2>@[<2>object%a@]@ %a@;<1 -2>end@]" pr_param self_ty
        (print_list print_out_class_sig_item (fun ppf -> fprintf ppf "@ "))
        csil ]
and print_out_class_sig_item ppf =
  fun
  [ Ocsg_constraint ty1 ty2 ->
      fprintf ppf "@[<2>type %a =@ %a;@]" Toploop.print_out_type.val ty1
        Toploop.print_out_type.val ty2
  | Ocsg_method name priv virt ty ->
      fprintf ppf "@[<2>method %s%s%s :@ %a;@]"
        (if priv then "private " else "") (if virt then "virtual " else "")
        name Toploop.print_out_type.val ty
  | Ocsg_value name mut virt ty ->
      fprintf ppf "@[<2>value %s%s%s :@ %a;@]"
        (if mut then "mutable " else "") (if virt then "virtual " else "")
        name Toploop.print_out_type.val ty ]
;

value rec print_out_module_type ppf =
  fun
  [ Omty_ident id -> fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
      fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]"
        Toploop.print_out_signature.val sg
  | Omty_functor name mty_arg mty_res ->
      fprintf ppf "@[<2>functor@ (%s : %a) ->@ %a@]" name
        print_out_module_type mty_arg print_out_module_type mty_res
  | Omty_abstract -> () ]
and print_out_signature ppf =
  fun
  [ [] -> ()
  | [item] -> fprintf ppf "%a;" Toploop.print_out_sig_item.val item
  | [item :: items] ->
      fprintf ppf "%a;@ %a" Toploop.print_out_sig_item.val item
        print_out_signature items ]
and print_out_sig_item ppf =
  fun
  [ Osig_class vir_flag name params clt rs ->
      fprintf ppf "@[<2>%s%s@ %a%s@ :@ %a@]"
        (if rs = Orec_next then "and" else "class")
        (if vir_flag then " virtual" else "") print_out_class_params params
        name Toploop.print_out_class_type.val clt
  | Osig_class_type vir_flag name params clt rs ->
      fprintf ppf "@[<2>%s%s@ %a%s@ =@ %a@]"
        (if rs = Orec_next then "and" else "class type")
        (if vir_flag then " virtual" else "") print_out_class_params params
        name Toploop.print_out_class_type.val clt
  | Osig_exception id tyl ->
      fprintf ppf "@[<2>exception %a@]" print_out_constr (id, tyl)
  | Osig_modtype name Omty_abstract ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype name mty ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name
        Toploop.print_out_module_type.val mty
  | Osig_module name mty rs ->
      fprintf ppf "@[<2>%s %s :@ %a@]"
        (match rs with [ Orec_not -> "module"
                       | Orec_first -> "module rec"
                       | Orec_next -> "and" ]) name
        Toploop.print_out_module_type.val mty
  | Osig_type td rs ->
      print_out_type_decl
          (if rs = Orec_next then "and" else "type")
          ppf td
  | Osig_value name ty prims ->
      let kwd = if prims = [] then "value" else "external" in
      let pr_prims ppf =
        fun
        [ [] -> ()
        | [s :: sl] ->
            do {
              fprintf ppf "@ = \"%s\"" s;
              List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
            } ]
      in
      fprintf ppf "@[<2>%s %a :@ %a%a@]" kwd value_ident name
        Toploop.print_out_type.val ty pr_prims prims ]

and print_out_type_decl kwd ppf (name, args, ty, priv, constraints) =
  let constrain ppf (ty, ty') =
    fprintf ppf "@ @[<2>constraint %a =@ %a@]" Toploop.print_out_type.val ty
      Toploop.print_out_type.val ty'
  in
  let print_constraints ppf params = List.iter (constrain ppf) params in
  let type_defined ppf =
    match args with
    [ [] -> fprintf ppf "%s" name
    | [arg] -> fprintf ppf "%s %a" name type_parameter arg
    | _ ->
        fprintf ppf "%s@ %a" name
          (print_list type_parameter (fun ppf -> fprintf ppf "@ ")) args ]
  and print_kind ppf ty =
    fprintf ppf "%s@ %a"
      (if priv = Obj.magic Camlp4_import.Asttypes.Private then " private" else "")
      Toploop.print_out_type.val ty
  in
  let print_types ppf = fun
    [ Otyp_manifest ty1 ty2 ->
        fprintf ppf "@ @[<2>%a ==%a@]"
          Toploop.print_out_type.val ty1
          print_kind ty2
    | ty -> print_kind ppf ty ]
  in
  fprintf ppf "@[<2>@[<hv 2>@[%s %t@] =%a@]%a@]" kwd type_defined
    print_types ty print_constraints constraints
;

(* Phrases *)

value print_out_exception ppf exn outv =
  match exn with
  [ Sys.Break -> fprintf ppf "Interrupted.@."
  | Out_of_memory -> fprintf ppf "Out of memory during evaluation.@."
  | Stack_overflow ->
      fprintf ppf "Stack overflow during evaluation (looping recursion?).@."
  | _ ->
      fprintf ppf "@[Exception:@ %a.@]@." Toploop.print_out_value.val outv ]
;

value rec print_items ppf =
  fun
  [ [] -> ()
  | [(tree, valopt) :: items] ->
      do {
        match valopt with
        [ Some v ->
            fprintf ppf "@[<2>%a =@ %a@]" Toploop.print_out_sig_item.val tree
              Toploop.print_out_value.val v
        | None -> fprintf ppf "@[%a@]" Toploop.print_out_sig_item.val tree ];
        if items <> [] then fprintf ppf "@ %a" print_items items else ()
      } ]
;

value print_out_phrase ppf =
  fun
  [ Ophr_eval outv ty ->
      fprintf ppf "@[- : %a@ =@ %a@]@." Toploop.print_out_type.val ty
        Toploop.print_out_value.val outv
  | Ophr_signature [] -> ()
  | Ophr_signature items -> fprintf ppf "@[<v>%a@]@." print_items items
  | Ophr_exception (exn, outv) -> print_out_exception ppf exn outv ]
;

Toploop.print_out_value.val := print_out_value;
Toploop.print_out_type.val := print_out_type;
Toploop.print_out_class_type.val := print_out_class_type;
Toploop.print_out_module_type.val := print_out_module_type;
Toploop.print_out_sig_item.val := print_out_sig_item;
Toploop.print_out_signature.val := print_out_signature;
Toploop.print_out_phrase.val := print_out_phrase;
