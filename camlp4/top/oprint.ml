(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format;
open Outcometree;

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
    [ Oval_tuple tree_list ->
        fprintf ppf "@[%a@]" (print_tree_list print_tree_1 ",") tree_list
    | tree -> print_tree_1 ppf tree ]
  and print_tree_1 ppf =
    fun
    [ Oval_constr name [param] ->
        fprintf ppf "@[<1>%a@ %a@]" print_ident name print_simple_tree param
    | Oval_constr name ([_ :: _] as params) ->
        fprintf ppf "@[<1>%a@ (%a)@]" print_ident name
          (print_tree_list print_tree_1 ",") params
    | Oval_variant name (Some param) ->
        fprintf ppf "@[<2>`%s@ %a@]" name print_simple_tree param
    | tree -> print_simple_tree ppf tree ]
  and print_simple_tree ppf =
    fun
    [ Oval_int i -> fprintf ppf "%i" i
    | Oval_int32 i -> fprintf ppf "%ldl" i
    | Oval_int64 i -> fprintf ppf "%LdL" i
    | Oval_nativeint i -> fprintf ppf "%ndn" i
    | Oval_float f -> fprintf ppf "%F" f
    | Oval_char c -> fprintf ppf "%C" c
    | Oval_string s ->
        try fprintf ppf "%S" s with
        [ Invalid_argument "String.create" -> fprintf ppf "<huge string>" ]
    | Oval_list tl ->
        fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_array tl ->
        fprintf ppf "@[<2>[|%a|]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_constr name [] -> print_ident ppf name
    | Oval_variant name None -> fprintf ppf "`%s" name
    | Oval_stuff s -> fprintf ppf "%s" s
    | Oval_record fel ->
        fprintf ppf "@[<1>{%a}@]" (cautious (print_fields True)) fel
    | Oval_ellipsis -> raise Ellipsis
    | Oval_printer f -> f ppf
    | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree) tree ]
  and print_fields first ppf =
    fun
    [ [] -> ()
    | [(name, tree) :: fields] ->
        do {
          if not first then fprintf ppf ";@ " else ();
          fprintf ppf "@[<1>%a@ =@ %a@]" print_ident name
            (cautious print_tree) tree;
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

(* Types *)

value rec print_list_init pr sep ppf =
  fun
  [ [] -> ()
  | [a :: l] -> do { sep ppf; pr ppf a; print_list_init pr sep ppf l } ]
;

value pr_vars =
  print_list (fun ppf s -> fprintf ppf "'%s" s) (fun ppf -> fprintf ppf "@ ")
;

value rec print_list pr sep ppf =
  fun
  [ [] -> ()
  | [a] -> pr ppf a
  | [a :: l] -> do { pr ppf a; sep ppf; print_list pr sep ppf l } ]
;

value pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")
;

value rec print_out_type ppf =
  fun
  [ Otyp_alias ty s -> fprintf ppf "@[%a as '%s@]" print_out_type ty s
  | Otyp_poly sl ty ->
      fprintf ppf "@[<hov 2>%a.@ %a@]"
        pr_vars sl
        print_out_type ty
  | ty -> print_out_type_1 ppf ty ]
and print_out_type_1 ppf =
  fun
  [ Otyp_arrow lab ty1 ty2 ->
      fprintf ppf "@[%s%a ->@ %a@]" (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty1 print_out_type_1 ty2
  | ty -> print_out_type_2 ppf ty ]
and print_out_type_2 ppf =
  fun
  [ Otyp_tuple tyl ->
      fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type " *") tyl
  | ty -> print_simple_out_type ppf ty ]
and print_simple_out_type ppf =
  fun
  [ Otyp_class ng id tyl ->
      fprintf ppf "@[%a%s#%a@]" print_typargs tyl (if ng then "_" else "")
        print_ident id
  | Otyp_constr id tyl ->
      fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
  | Otyp_object fields rest ->
      fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
  | Otyp_stuff s -> fprintf ppf "%s" s
  | Otyp_var ng s -> fprintf ppf "'%s%s" (if ng then "_" else "") s
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
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a]@]" (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "< "
         else if tags = None then "> "
         else "? ")
        print_fields row_fields print_present tags
  | Otyp_alias _ _ | Otyp_poly _ | Otyp_arrow _ _ _ | Otyp_tuple _ as ty ->
      fprintf ppf "@[<1>(%a)@]" print_out_type ty
  | Otyp_abstract | Otyp_sum _ | Otyp_record _ | Otyp_private _
  | Otyp_manifest _ _ -> () ]
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
;

(* Signature items *)

value print_out_class_params ppf =
  fun
  [ [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (print_list (fun ppf x -> fprintf ppf "'%s" x)
           (fun ppf -> fprintf ppf ", "))
        tyl ]
;

value rec print_out_class_type ppf =
  fun
  [ Octy_constr id tyl ->
      let pr_tyl ppf =
        fun
        [ [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ " (print_typlist print_out_type ",")
              tyl ]
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl print_ident id
  | Octy_fun lab ty cty ->
      fprintf ppf "@[%s%a ->@ %a@]" (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty print_out_class_type cty
  | Octy_signature self_ty csil ->
      let pr_param ppf =
        fun
        [ Some ty -> fprintf ppf "@ @[(%a)@]" print_out_type ty
        | None -> () ]
      in
      fprintf ppf "@[<hv 2>@[<2>object%a@]@ %a@;<1 -2>end@]" pr_param self_ty
        (print_list print_out_class_sig_item (fun ppf -> fprintf ppf "@ "))
        csil ]
and print_out_class_sig_item ppf =
  fun
  [ Ocsg_constraint ty1 ty2 ->
      fprintf ppf "@[<2>constraint %a =@ %a@]" print_out_type ty1
        print_out_type ty2
  | Ocsg_method name priv virt ty ->
      fprintf ppf "@[<2>method %s%s%s :@ %a@]"
        (if priv then "private " else "") (if virt then "virtual " else "")
        name print_out_type ty
  | Ocsg_value name mut ty ->
      fprintf ppf "@[<2>val %s%s :@ %a@]" (if mut then "mutable " else "")
        name print_out_type ty ]
;

value rec print_out_module_type ppf =
  fun
  [ Omty_abstract -> ()
  | Omty_functor name mty_arg mty_res ->
      fprintf ppf "@[<2>functor@ (%s : %a) ->@ %a@]" name
        print_out_module_type mty_arg print_out_module_type mty_res
  | Omty_ident id -> fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
      fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" print_signature_body sg ]
and print_signature_body ppf =
  fun
  [ [] -> ()
  | [item] -> print_out_sig_item ppf item
  | [item :: items] ->
      fprintf ppf "%a@ %a" print_out_sig_item item
        print_signature_body items ]
and print_out_sig_item ppf =
  fun
  [ Osig_class vir_flag name params clt ->
      fprintf ppf "@[<2>class%s@ %a%s@ :@ %a@]"
        (if vir_flag then " virtual" else "") print_out_class_params params
        name print_out_class_type clt
  | Osig_class_type vir_flag name params clt ->
      fprintf ppf "@[<2>class type%s@ %a%s@ =@ %a@]"
        (if vir_flag then " virtual" else "") print_out_class_params params
        name print_out_class_type clt
  | Osig_exception id tyl ->
      fprintf ppf "@[<2>exception %a@]" print_out_constr (id, tyl)
  | Osig_modtype name Omty_abstract ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype name mty ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name print_out_module_type mty
  | Osig_module name mty ->
      fprintf ppf "@[<2>module %s :@ %a@]" name print_out_module_type mty
  | Osig_type tdl -> print_out_type_decl_list ppf tdl
  | Osig_value name ty prims ->
      let kwd = if prims = [] then "val" else "external" in
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
        print_out_type ty pr_prims prims ]
and print_out_type_decl_list ppf =
  fun
  [ [] -> ()
  | [x] -> print_out_type_decl "type" ppf x
  | [x :: l] ->
      do {
        print_out_type_decl "type" ppf x;
        List.iter (fun x -> fprintf ppf "@ %a" (print_out_type_decl "and") x)
          l
      } ]
and print_out_type_decl kwd ppf (name, args, ty, constraints) =
  let print_constraints ppf params =
    List.iter
      (fun (ty1, ty2) ->
         fprintf ppf "@ @[<2>constraint %a =@ %a@]" print_out_type
           ty1 print_out_type ty2)
      params
  in
  let type_parameter ppf (ty, (co, cn)) =
    fprintf ppf "%s'%s" (if not cn then "+" else if not co then "-" else "")
      ty
  in
  let type_defined ppf =
    match args with
    [ [] -> fprintf ppf "%s" name
    | [arg] -> fprintf ppf "@[%a@ %s@]" type_parameter arg name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list type_parameter (fun ppf -> fprintf ppf ",@ ")) args
          name ]
  in
  let print_manifest ppf =
    fun
    [ Otyp_manifest ty _ -> fprintf ppf " =@ %a" print_out_type ty
    | _ -> () ]
  in
  let print_name_args ppf =
    fprintf ppf "%s %t%a" kwd type_defined print_manifest ty
  in
  let ty =
    match ty with
    [ Otyp_manifest _ ty -> ty
    | _ -> ty ]
  in
  match ty with
  [ Otyp_abstract ->
      fprintf ppf "@[<2>@[<hv 2>%t@]%a@]" print_name_args print_constraints
        constraints
  | Otyp_record lbls ->
      fprintf ppf "@[<2>@[<hv 2>%t = {%a@;<1 -2>}@]@ %a@]" print_name_args
        (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
        print_constraints constraints
  | Otyp_sum constrs ->
      fprintf ppf "@[<2>@[<hv 2>%t =@;<1 2>%a@]%a@]" print_name_args
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
        print_constraints constraints
  | ty ->
      fprintf ppf "@[<2>@[<hv 2>%t =@ %a@]%a@]" print_name_args
        print_out_type ty print_constraints constraints ]
and print_out_constr ppf (name, tyl) =
  match tyl with
  [ [] -> fprintf ppf "%s" name
  | _ ->
      fprintf ppf "@[<2>%s of@ %a@]" name
        (print_typlist print_simple_out_type " *") tyl ]
and print_out_label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s%s :@ %a@];" (if mut then "mutable " else "") name
    print_out_type arg
;

(* Signature items *)

value print_out_class_params ppf =
  fun
  [ [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (print_list (fun ppf x -> fprintf ppf "'%s" x)
           (fun ppf -> fprintf ppf ", "))
        tyl ]
;

value rec print_out_class_type ppf =
  fun
  [ Octy_constr id tyl ->
      let pr_tyl ppf =
        fun
        [ [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ "
              (print_typlist print_out_type ",") tyl ]
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl print_ident id
  | Octy_fun lab ty cty ->
      fprintf ppf "@[%s%a ->@ %a@]" (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty print_out_class_type cty
  | Octy_signature self_ty csil ->
      let pr_param ppf =
        fun
        [ Some ty -> fprintf ppf "@ @[(%a)@]" print_out_type ty
        | None -> () ]
      in
      fprintf ppf "@[<hv 2>@[<2>object%a@]@ %a@;<1 -2>end@]" pr_param self_ty
        (print_list print_out_class_sig_item (fun ppf -> fprintf ppf "@ "))
        csil ]
and print_out_class_sig_item ppf =
  fun
  [ Ocsg_constraint ty1 ty2 ->
      fprintf ppf "@[<2>constraint %a =@ %a@]" print_out_type ty1
        print_out_type ty2
  | Ocsg_method name priv virt ty ->
      fprintf ppf "@[<2>method %s%s%s :@ %a@]"
        (if priv then "private " else "") (if virt then "virtual " else "")
        name print_out_type ty
  | Ocsg_value name mut ty ->
      fprintf ppf "@[<2>val %s%s :@ %a@]" (if mut then "mutable " else "")
        name print_out_type ty ]
;

value rec print_out_module_type ppf =
  fun
  [ Omty_abstract -> ()
  | Omty_functor name mty_arg mty_res ->
      fprintf ppf "@[<2>functor@ (%s : %a) ->@ %a@]" name
        print_out_module_type mty_arg print_out_module_type mty_res
  | Omty_ident id -> fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
      fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" print_signature_body sg ]
and print_signature_body ppf =
  fun
  [ [] -> ()
  | [item] -> print_out_sig_item ppf item
  | [item :: items] ->
      fprintf ppf "%a@ %a" print_out_sig_item item print_signature_body
        items ]
and print_out_sig_item ppf =
  fun
  [ Osig_class vir_flag name params clt ->
      fprintf ppf "@[<2>class%s@ %a%s@ :@ %a@]"
        (if vir_flag then " virtual" else "") print_out_class_params params
        name print_out_class_type clt
  | Osig_class_type vir_flag name params clt ->
      fprintf ppf "@[<2>class type%s@ %a%s@ =@ %a@]"
        (if vir_flag then " virtual" else "") print_out_class_params params
        name print_out_class_type clt
  | Osig_exception id tyl ->
      fprintf ppf "@[<2>exception %a@]" print_out_constr (id, tyl)
  | Osig_modtype name Omty_abstract ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype name mty ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name print_out_module_type mty
  | Osig_module name mty ->
      fprintf ppf "@[<2>module %s :@ %a@]" name print_out_module_type mty
  | Osig_type tdl -> print_out_type_decl_list ppf tdl
  | Osig_value name ty prims ->
      let kwd = if prims = [] then "val" else "external" in
      let pr_prims ppf =
        fun
        [ [] -> ()
        | [s :: sl] ->
            do {
              fprintf ppf "@ = \"%s\"" s;
              List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
            } ]
      in
      fprintf ppf "@[<2>%s %a :@ %a%a@]" kwd value_ident name print_out_type
        ty pr_prims prims ]
and print_out_type_decl_list ppf =
  fun
  [ [] -> ()
  | [x] -> print_out_type_decl "type" ppf x
  | [x :: l] ->
      do {
        print_out_type_decl "type" ppf x;
        List.iter (fun x -> fprintf ppf "@ %a" (print_out_type_decl "and") x)
          l
      } ]
and print_out_type_decl kwd ppf (name, args, ty, constraints) =
  let print_constraints ppf params =
    List.iter
      (fun (ty1, ty2) ->
         fprintf ppf "@ @[<2>constraint %a =@ %a@]" print_out_type ty1
           print_out_type ty2)
      params
  in
  let type_parameter ppf (ty, (co, cn)) =
    fprintf ppf "%s'%s" (if not cn then "+" else if not co then "-" else "")
      ty
  in
  let type_defined ppf =
    match args with
    [ [] -> fprintf ppf "%s" name
    | [arg] -> fprintf ppf "@[%a@ %s@]" type_parameter arg name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list type_parameter (fun ppf -> fprintf ppf ",@ ")) args
          name ]
  in
  let print_manifest ppf =
    fun
    [ Otyp_manifest ty _ -> fprintf ppf " =@ %a" print_out_type ty
    | _ -> () ]
  in
  let print_name_args ppf =
    fprintf ppf "%s %t%a" kwd type_defined print_manifest ty
  in
  let ty =
    match ty with
    [ Otyp_manifest _ ty -> ty
    | _ -> ty ]
  in
  match ty with
  [ Otyp_abstract ->
      fprintf ppf "@[<2>@[<hv 2>%t@]%a@]" print_name_args print_constraints
        constraints
  | Otyp_record lbls ->
      fprintf ppf "@[<2>@[<hv 2>%t = {%a@;<1 -2>}@]@ %a@]" print_name_args
        (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
        print_constraints constraints
  | Otyp_sum constrs ->
      fprintf ppf "@[<2>@[<hv 2>%t =@;<1 2>%a@]%a@]" print_name_args
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
        print_constraints constraints
  | ty ->
      fprintf ppf "@[<2>@[<hv 2>%t =@ %a@]%a@]" print_name_args
        print_out_type ty print_constraints constraints ]
and print_out_constr ppf (name, tyl) =
  match tyl with
  [ [] -> fprintf ppf "%s" name
  | _ ->
      fprintf ppf "@[<2>%s of@ %a@]" name
        (print_typlist print_simple_out_type " *") tyl ]
and print_out_label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s%s :@ %a@];" (if mut then "mutable " else "") name
    print_out_type arg
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
Toploop.print_out_sig_item.val := print_out_sig_item;
Toploop.print_out_phrase.val := print_out_phrase;
