(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Printing functions *)

open Misc
open Ctype
open Format
open Longident
open Path
open Asttypes
open Types
open Btype
open Outcometree

(* Print a long identifier *)

let rec longident ppf = function
  | Lident s -> fprintf ppf "%s" s
  | Ldot(p, s) -> fprintf ppf "%a.%s" longident p s
  | Lapply(p1, p2) -> fprintf ppf "%a(%a)" longident p1 longident p2

(* Print an identifier *)

let ident ppf id = fprintf ppf "%s" (Ident.name id)

(* Print a path *)

let ident_pervasive = Ident.create_persistent "Pervasives"

let rec tree_of_path = function
  | Pident id ->
      Oide_ident (Ident.name id)
  | Pdot(Pident id, s, pos) when Ident.same id ident_pervasive ->
      Oide_ident s
  | Pdot(p, s, pos) ->
      Oide_dot (tree_of_path p, s)
  | Papply(p1, p2) ->
      Oide_apply (tree_of_path p1, tree_of_path p2)

let rec path ppf = function
  | Pident id ->
      ident ppf id
  | Pdot(Pident id, s, pos) when Ident.same id ident_pervasive ->
      fprintf ppf "%s" s
  | Pdot(p, s, pos) ->
      fprintf ppf "%a.%s" path p s
  | Papply(p1, p2) ->
      fprintf ppf "%a(%a)" path p1 path p2

(* Print a type expression *)

let names = ref ([] : (type_expr * string) list)
let name_counter = ref 0

let reset_names () = names := []; name_counter := 0

let new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter)) 
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26) in
  incr name_counter;
  name

let name_of_type t =
  try List.assq t !names with Not_found ->
    let name = new_name () in
    names := (t, name) :: !names;
    name

let check_name_of_type t = ignore(name_of_type t)

let print_name_of_type ppf t = fprintf ppf "%s" (name_of_type t)

let visited_objects = ref ([] : type_expr list)
let aliased = ref ([] : type_expr list)

let is_aliased ty = List.memq ty !aliased
let add_alias ty =
  if not (is_aliased ty) then aliased := ty :: !aliased

let proxy ty =
  let ty = repr ty in
  match ty.desc with
  | Tvariant row -> Btype.row_more row
  | _ -> ty

let namable_row row =
  row.row_name <> None &&
  List.for_all
    (fun (_, f) ->
       match row_field_repr f with
       | Reither(c, l, _, _) ->
           row.row_closed && if c then l = [] else List.length l = 1
       | _ -> true)
    row.row_fields

let rec mark_loops_rec visited ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.memq px visited then add_alias px else
    let visited = px :: visited in
    match ty.desc with
    | Tvar -> ()
    | Tarrow(_, ty1, ty2, _) ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Ttuple tyl -> List.iter (mark_loops_rec visited) tyl
    | Tconstr(_, tyl, _) ->
        List.iter (mark_loops_rec visited) tyl
    | Tvariant row ->
        let row = row_repr row in
        if List.memq px !visited_objects then add_alias px else
         begin
          if not (static_row row) then
            visited_objects := px :: !visited_objects;
          match row.row_name with
          | Some(p, tyl) when namable_row row ->
              List.iter (mark_loops_rec visited) tyl
          | _ ->
              iter_row (mark_loops_rec visited) {row with row_bound = []}
         end
    | Tobject (fi, nm) ->
        if List.memq px !visited_objects then add_alias px else
         begin
          if opened_object ty then
            visited_objects := px :: !visited_objects;
          begin match !nm with
          | None ->
              mark_loops_rec visited fi
          | Some (_, l) ->
              List.iter (mark_loops_rec visited) l
          end
        end
    | Tfield(_, kind, ty1, ty2) when field_kind_repr kind = Fpresent ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Tfield(_, _, _, ty2) ->
        mark_loops_rec visited ty2
    | Tnil -> ()
    | Tsubst ty -> mark_loops_rec visited ty
    | Tlink _ -> fatal_error "Printtyp.mark_loops_rec (2)"

let mark_loops ty =
  normalize_type Env.empty ty;
  mark_loops_rec [] ty;;

let reset_loop_marks () =
  visited_objects := []; aliased := []

let reset () =
  reset_names (); reset_loop_marks ()

let reset_and_mark_loops ty =
  reset (); mark_loops ty;;

let reset_and_mark_loops_list tyl =
 reset (); List.iter mark_loops tyl;;

(* Disabled in classic mode when printing an unification error *)
let print_labels = ref true
let print_label ppf l =
  if !print_labels && l <> "" || is_optional l then fprintf ppf "%s:" l

let rec print_list_init pr sep ppf = function
  | [] -> ()
  | a :: l -> sep ppf; pr ppf a; print_list_init pr sep ppf l;;

let rec print_list pr sep ppf = function
  | [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l;;

let rec tree_of_typexp sch ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.mem_assq px !names then
   let mark = if ty.desc = Tvar then is_non_gen sch px else false in
   Otyp_var (mark, name_of_type px) else

  let pr_typ () =
   (match ty.desc with
    | Tvar ->
        Otyp_var (is_non_gen sch ty, name_of_type ty)
    | Tarrow(l, ty1, ty2, _) ->
        let pr_arrow l ty1 ty2 =
          let lab =
            if !print_labels && l <> "" || is_optional l then l else ""
          in
          let t1 =
            if is_optional l then
              match (repr ty1).desc with
              | Tconstr(path, [ty], _)
                when Path.same path Predef.path_option ->
                  tree_of_typexp sch ty
              | _ -> Otyp_stuff "<hidden>"
            else tree_of_typexp sch ty1 in
          Otyp_arrow (lab, t1, tree_of_typexp sch ty2) in
        pr_arrow l ty1 ty2
    | Ttuple tyl ->
        Otyp_tuple (tree_of_typlist sch tyl)
    | Tconstr(p, tyl, abbrev) ->
        Otyp_constr (tree_of_path p, tree_of_typlist sch tyl)
    | Tvariant row ->
        let row = row_repr row in
        let fields =
          if row.row_closed then
            List.filter (fun (_, f) -> row_field_repr f <> Rabsent)
              row.row_fields
          else row.row_fields in
        let present =
          List.filter
            (fun (_, f) ->
               match row_field_repr f with
               | Rpresent _ -> true
               | _ -> false)
            fields in
        let all_present = List.length present = List.length fields in
        begin match row.row_name with
        | Some(p, tyl) when namable_row row ->
            let id = tree_of_path p in
            let args = tree_of_typlist sch tyl in
            if row.row_closed && all_present then
              Otyp_constr (id, args)
            else
              let non_gen = is_non_gen sch px in
              let tags =
                if all_present then None else Some (List.map fst present) in
              Otyp_variant (non_gen, Ovar_name(tree_of_path p, args),
                            row.row_closed, tags)
        | _ ->
            let non_gen =
              not (row.row_closed && all_present) && is_non_gen sch px in
            let fields = List.map (tree_of_row_field sch) fields in
            let tags =
              if all_present then None else Some (List.map fst present) in
            Otyp_variant (non_gen, Ovar_fields fields, row.row_closed, tags)
        end
    | Tobject (fi, nm) ->
        tree_of_typobject sch ty fi nm
    | Tsubst ty ->
        tree_of_typexp sch ty
    | Tlink _ | Tnil | Tfield _ ->
        fatal_error "Printtyp.tree_of_typexp"
   ) in
  if is_aliased px then begin
    check_name_of_type px;
    Otyp_alias (pr_typ (), name_of_type px) end
  else pr_typ ()

and tree_of_row_field sch (l, f) =
  match row_field_repr f with
  | Rpresent None | Reither(true, [], _, _) -> (l, false, [])
  | Rpresent(Some ty) -> (l, false, [tree_of_typexp sch ty])
  | Reither(c, tyl, _, _) ->
      if c (* contradiction: un constructeur constant qui a un argument *)
      then (l, true, tree_of_typlist sch tyl)
      else (l, false, tree_of_typlist sch tyl)
  | Rabsent -> (l, false, [] (* une erreur, en fait *))

and tree_of_typlist sch = function
  | [] -> []
  | ty :: tyl ->
      let tr = tree_of_typexp sch ty in
      tr :: tree_of_typlist sch tyl

and tree_of_typobject sch ty fi nm =
  begin match !nm with
  | None ->
      let pr_fields fi =
        let (fields, rest) = flatten_fields fi in
        let present_fields =
          List.fold_right
            (fun (n, k, t) l ->
               match field_kind_repr k with
               | Fpresent -> (n, t) :: l
               | _ -> l)
            fields [] in
        let sorted_fields =
          Sort.list (fun (n, _) (n', _) -> n <= n') present_fields in
        tree_of_typfields sch rest sorted_fields in
      let (fields, rest) = pr_fields fi in
      Otyp_object (fields, rest)
  | Some (p, {desc = Tvar} :: tyl) ->
      let non_gen = is_non_gen sch ty in
      let args = tree_of_typlist sch tyl in
      Otyp_class (non_gen, tree_of_path p, args)
  | _ ->
      fatal_error "Printtyp.tree_of_typobject"
  end

and is_non_gen sch ty =
    sch && ty.level <> generic_level

and tree_of_typfields sch rest = function
  | [] ->
      let rest =
        match rest.desc with
        | Tvar -> Some (is_non_gen sch rest)
        | Tnil -> None
        | _ -> fatal_error "typfields (1)"
      in
      ([], rest)
  | (s, t) :: l ->
      let field = (s, tree_of_typexp sch t) in
      let (fields, rest) = tree_of_typfields sch rest l in
      (field :: fields, rest)

let rec print_ident ppf =
  function
  | Oide_ident s -> fprintf ppf "%s" s
  | Oide_dot (id, s) -> fprintf ppf "%a.%s" print_ident id s
  | Oide_apply (id1, id2) ->
      fprintf ppf "%a(%a)" print_ident id1 print_ident id2

let pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let rec print_out_type ppf =
  function
  | Otyp_alias (ty, s) ->
      fprintf ppf "@[%a as '%s@]" print_out_type ty s
  | ty ->
      print_out_type_1 ppf ty

and print_out_type_1 ppf =
  function
  | Otyp_arrow (lab, ty1, ty2) ->
      fprintf ppf "@[%s%a ->@ %a@]"
        (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty1 print_out_type_1 ty2
  | ty ->
      print_out_type_2 ppf ty

and print_out_type_2 ppf =
  function
  | Otyp_tuple tyl ->
      fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type " *") tyl
  | ty ->
      print_simple_out_type ppf ty

and print_simple_out_type ppf =
  function
  | Otyp_class (ng, id, tyl) ->
      fprintf ppf "@[%a%s#%a@]" print_typargs tyl
        (if ng then "_" else "") print_ident id
  | Otyp_constr (id, tyl) ->
      fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
  | Otyp_object (fields, rest) ->
      fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
  | Otyp_stuff s ->
      fprintf ppf "%s" s
  | Otyp_var (ng, s) ->
      fprintf ppf "'%s%s" (if ng then "_" else "") s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let print_present ppf =
        function
        | None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      let print_fields ppf = function
          Ovar_fields fields ->
            print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_name (id, tyl) ->
            fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
      in
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a]@]"
        (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "< "
         else if tags = None then "> " else "? ")
        print_fields row_fields
        print_present tags
  | Otyp_alias (_, _) | Otyp_arrow (_, _, _) | Otyp_tuple _ as ty ->
      fprintf ppf "@[<1>(%a)@]" print_out_type ty
  | Otyp_abstract | Otyp_sum _ | Otyp_record _ | Otyp_manifest (_, _) -> ()

and print_fields rest ppf =
  function
  | [] ->
      begin match rest with
      | Some non_gen -> fprintf ppf "%s.." (if non_gen then "_" else "")
      | None -> ()
      end
  | [(s, t)] ->
      fprintf ppf "%s : %a" s print_out_type t;
      begin match rest with
      | Some _ -> fprintf ppf ";@ "
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
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of
    (print_typlist print_out_type " &") tyl

and print_typlist print_elem sep ppf = function
  | [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      fprintf ppf "%a%s@ %a"
        print_elem ty sep (print_typlist print_elem sep) tyl

and print_typargs ppf =
  function
  | [] -> ()
  | [ty1] -> fprintf ppf "%a@ " print_simple_out_type ty1
  | tyl -> fprintf ppf "@[<1>(%a)@]@ " (print_typlist print_out_type ",") tyl

let outcome_type = ref print_out_type

let typexp sch prio ppf ty =
  !outcome_type ppf (tree_of_typexp sch ty)

let type_expr ppf ty = typexp false 0 ppf ty

and type_sch ppf ty = typexp true 0 ppf ty

and type_scheme ppf ty = reset_and_mark_loops ty; typexp true 0 ppf ty

(* Maxence *)
let type_scheme_max ?(b_reset_names=true) ppf ty = 
  if b_reset_names then reset_names () ;
  typexp true 0 ppf ty
(* Fin Maxence *)

let tree_of_type_scheme ty = reset_and_mark_loops ty; tree_of_typexp true ty

(* Print modules types and signatures *)

let value_ident ppf name =
  if List.mem name
      ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"]
  then fprintf ppf "( %s )" name
  else match name.[0] with
  | 'a' .. 'z' | '\223' .. '\246' | '\248' .. '\255' | '_' ->
      fprintf ppf "%s" name
  | _ -> fprintf ppf "( %s )" name

let print_out_class_params ppf =
  function
  | [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (print_list (fun ppf x -> fprintf ppf "'%s" x)
           (fun ppf -> fprintf ppf ", "))
        tyl

let rec print_out_class_type ppf =
  function
  | Octy_constr (id, tyl) ->
      let pr_tyl ppf = function
        | [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ "
              (print_typlist print_out_type ",") tyl
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl print_ident id
  | Octy_fun (lab, ty, cty) ->
      fprintf ppf "@[%s%a ->@ %a@]"
        (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty print_out_class_type cty
  | Octy_signature (self_ty, csil) ->
      let pr_param ppf =
        function
        | Some ty -> fprintf ppf "@ @[(%a)@]" print_out_type ty
        | None -> ()
      in
      fprintf ppf "@[<hv 2>@[<2>object%a@]@ %a@;<1 -2>end@]"
        pr_param self_ty
        (print_list print_out_class_sig_item (fun ppf -> fprintf ppf "@ "))
        csil
and print_out_class_sig_item ppf =
  function
  | Ocsg_constraint (ty1, ty2) ->
     fprintf ppf "@[<2>constraint %a =@ %a@]" !outcome_type ty1
       !outcome_type ty2
  | Ocsg_method (name, priv, virt, ty) ->
     fprintf ppf "@[<2>method %s%s%s :@ %a@]"
       (if priv then "private " else "") (if virt then "virtual " else "")
       name !outcome_type ty
  | Ocsg_value (name, mut, ty) ->
     fprintf ppf "@[<2>val %s%s :@ %a@]" (if mut then "mutable " else "") name
       !outcome_type ty

let rec print_out_module_type ppf =
  function
  | Omty_abstract -> ()
  | Omty_functor (name, mty_arg, mty_res) ->
      fprintf ppf "@[<2>functor@ (%s : %a) ->@ %a@]" name
        print_out_module_type mty_arg print_out_module_type mty_res
  | Omty_ident id ->
      fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
      fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" print_signature_body sg
and print_signature_body ppf =
  function
  | [] -> ()
  | item :: [] -> print_out_sig_item ppf item
  | item :: items ->
      fprintf ppf "%a@ %a" print_out_sig_item item print_signature_body items
and print_out_sig_item ppf =
  function
  | Osig_class (vir_flag, name, params, clt) ->
      fprintf ppf "@[<2>class%s@ %a%s@ :@ %a@]"
        (if vir_flag then " virtual" else "")
        print_out_class_params params name print_out_class_type clt
  | Osig_class_type (vir_flag, name, params, clt) ->
      fprintf ppf "@[<2>class type%s@ %a%s@ =@ %a@]"
        (if vir_flag then " virtual" else "")
        print_out_class_params params name print_out_class_type clt
  | Osig_exception (id, tyl) ->
      fprintf ppf "@[<2>exception %a@]" print_out_constr (id, tyl)
  | Osig_modtype (name, Omty_abstract) ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype (name, mty) ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name print_out_module_type mty
  | Osig_module (name, mty) ->
      fprintf ppf "@[<2>module %s :@ %a@]" name print_out_module_type mty
  | Osig_type tdl ->
      print_out_type_decl_list ppf tdl
  | Osig_value (name, ty, prims) ->
      let kwd = if prims = [] then "val" else "external" in
      let pr_prims ppf =
        function
        | [] -> ()
        | s :: sl ->
            fprintf ppf "@ = \"%s\"" s;
            List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
      in
      fprintf ppf "@[<2>%s %a :@ %a%a@]"
        kwd value_ident name !outcome_type ty pr_prims prims
and print_out_type_decl_list ppf =
  function
  | [] -> ()
  | [x] -> print_out_type_decl "type" ppf x
  | x :: l ->
      print_out_type_decl "type" ppf x;
      List.iter (fun x -> fprintf ppf "@ %a" (print_out_type_decl "and") x) l
and print_out_type_decl kwd ppf (name, args, ty, constraints) =
  let print_constraints ppf params =
    List.iter
      (fun (ty1, ty2) ->
         fprintf ppf "@ @[<2>constraint %a =@ %a@]" !outcome_type ty1
           !outcome_type ty2)
      params
  in
  let type_parameter ppf (ty,(co,cn)) =
    fprintf ppf "%s'%s"
      (if not cn then "+" else if not co then "-" else "") ty
  in
  let type_defined ppf =
    match args with
    | [] -> fprintf ppf "%s" name
    | [arg] -> fprintf ppf "@[%a@ %s@]" type_parameter arg name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list type_parameter (fun ppf -> fprintf ppf ",@ "))
          args name
  in
  let print_manifest ppf =
    function
    | Otyp_manifest (ty, _) -> fprintf ppf " =@ %a" !outcome_type ty
    | _ -> ()
  in
  let print_name_args ppf =
    fprintf ppf "%s %t%a" kwd type_defined print_manifest ty
  in
  let ty =
    match ty with
    | Otyp_manifest (_, ty) -> ty
    | _ -> ty
  in
  match ty with
  | Otyp_abstract ->
      fprintf ppf "@[<2>@[<hv 2>%t@]%a@]"
        print_name_args print_constraints constraints
  | Otyp_record lbls ->
      fprintf ppf "@[<2>@[<hv 2>%t = {%a@;<1 -2>}@]@ %a@]"
        print_name_args
        (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
        print_constraints constraints
  | Otyp_sum constrs ->
      fprintf ppf "@[<2>@[<hv 2>%t =@;<1 2>%a@]%a@]"
        print_name_args
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
        print_constraints constraints
  | ty ->
      fprintf ppf "@[<2>@[<hv 2>%t =@ %a@]%a@]"
        print_name_args !outcome_type ty
        print_constraints constraints
and print_out_constr ppf (name, tyl) =
  match tyl with
  | [] -> fprintf ppf "%s" name
  | _ ->
      fprintf ppf "@[<2>%s of@ %a@]" name
        (print_typlist print_simple_out_type " *") tyl
and print_out_label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s%s :@ %a@];"
    (if mut then "mutable " else "") name !outcome_type arg

let outcome_sig_item = ref print_out_sig_item

(* Print one type declaration *)

let constrain ppf ty =
  let ty' = unalias ty in
  if ty != ty'
  then fprintf ppf "@ @[<2>constraint %a =@ %a@]" type_sch ty type_sch ty'

let tree_of_constraints params =
  List.fold_right
    (fun ty list ->
       let ty' = unalias ty in
       if ty != ty' then
         let tr = tree_of_typexp true ty in
         (tr, tree_of_typexp true ty') :: list
       else list)
    params []

let filter_params tyl =
  let params =
    List.fold_left
      (fun tyl ty ->
        let ty = repr ty in
        if List.memq ty tyl then Btype.newgenty (Tsubst ty) :: tyl
        else ty :: tyl)
      [] tyl
  in List.rev params

let string_of_mutable = function
  | Immutable -> ""
  | Mutable -> "mutable "

let rec tree_of_type_decl id decl =

  reset();

  let params = filter_params decl.type_params in

  aliased := params @ !aliased;
  List.iter mark_loops params;
  List.iter check_name_of_type (List.map proxy params);
  begin match decl.type_manifest with
  | None -> ()
  | Some ty -> mark_loops ty
  end;
  begin match decl.type_kind with
  | Type_abstract -> ()
  | Type_variant [] -> ()
  | Type_variant cstrs ->
      List.iter (fun (_, args) -> List.iter mark_loops args) cstrs
  | Type_record(l, rep) ->
      List.iter (fun (_, _, ty) -> mark_loops ty) l
  end;

  let type_param =
    function
    | Otyp_var (_, id) -> id
    | _ -> "?"
  in
  let type_defined decl =
    if decl.type_kind = Type_abstract && decl.type_manifest = None
       && List.exists (fun x -> x <> (true, true)) decl.type_variance then
      (Ident.name id,
       List.combine
         (List.map (fun ty -> type_param (tree_of_typexp false ty)) params)
         decl.type_variance)
    else
      let ty =
        tree_of_typexp false
          (Btype.newgenty (Tconstr(Pident id, params, ref Mnil)))
      in
      match ty with
      | Otyp_constr (Oide_ident id, tyl) ->
          (id, List.map (fun ty -> (type_param ty, (true, true))) tyl)
      | _ -> ("?", [])
  in
  let tree_of_manifest decl ty1 =
    match decl.type_manifest with
    | None -> ty1
    | Some ty -> Otyp_manifest (tree_of_typexp false ty, ty1)
  in
  let (name, args) = type_defined decl in
  let constraints = tree_of_constraints params in
  let ty =
    match decl.type_kind with
    | Type_abstract ->
        begin match decl.type_manifest with
        | None -> Otyp_abstract
        | Some ty -> tree_of_typexp false ty
        end
    | Type_variant cstrs ->
        tree_of_manifest decl (Otyp_sum (List.map tree_of_constructor cstrs))
    | Type_record(lbls, rep) ->
        tree_of_manifest decl (Otyp_record (List.map tree_of_label lbls))
  in
  (name, args, ty, constraints)

and tree_of_constructor (name, args) =
  (name, tree_of_typlist false args)

and tree_of_label (name, mut, arg) =
  (name, mut = Mutable, tree_of_typexp false arg)

let tree_of_type_declaration id decl =
  Osig_type [tree_of_type_decl id decl]

let type_declaration id ppf decl =
  !outcome_sig_item ppf (tree_of_type_declaration id decl)

(* Print an exception declaration *)

let tree_of_exception_declaration id decl =
  let tyl = tree_of_typlist false decl in
  Osig_exception (Ident.name id, tyl)

let exception_declaration id ppf decl =
  !outcome_sig_item ppf (tree_of_exception_declaration id decl)

(* Print a value declaration *)

let tree_of_value_description id decl =
  let id = Ident.name id in
  let ty = tree_of_type_scheme decl.val_type in
  let prims =
    match decl.val_kind with
    | Val_prim p -> Primitive.description_list p
    | _ -> []
  in
  Osig_value (id, ty, prims)

let value_description id ppf decl =
  !outcome_sig_item ppf (tree_of_value_description id decl)

(* Print a class type *)

let class_var sch ppf l (m, t) =
  fprintf ppf
    "@ @[<2>val %s%s :@ %a@]" (string_of_mutable m) l (typexp sch 0) t

let metho sch concrete ppf (lab, kind, ty) =
  if lab <> "*dummy method*" then begin
    let priv =
      match field_kind_repr kind with
      | Fvar _ (* {contents = None} *) -> "private "
      | _ (* Fpresent *) -> "" in
    let virt =
      if Concr.mem lab concrete then "" else "virtual " in
    fprintf ppf "@ @[<2>method %s%s%s :@ %a@]" priv virt lab (typexp sch 0) ty
  end

let tree_of_metho sch concrete csil (lab, kind, ty) =
  if lab <> "*dummy method*" then begin
    let priv =
      match field_kind_repr kind with
      | Fvar _ (* {contents = None} *) -> true
      | _ (* Fpresent *) -> false in
    let virt = not (Concr.mem lab concrete) in
    Ocsg_method (lab, priv, virt, tree_of_typexp sch ty) :: csil
  end
  else csil

let rec prepare_class_type params = function
  | Tcty_constr (p, tyl, cty) ->
      let sty = Ctype.self_type cty in
      begin try
        if List.memq sty !visited_objects
        || List.exists (fun ty -> (repr ty).desc <> Tvar) params
        then raise (Unify []);
        List.iter (occur Env.empty sty) tyl;
        List.iter mark_loops tyl
      with Unify _ ->
        prepare_class_type params cty
      end
  | Tcty_signature sign ->
      let sty = repr sign.cty_self in
      (* Self may have a name *)
      if List.memq sty !visited_objects then add_alias sty
      else visited_objects := sty :: !visited_objects;
      let (fields, _) =
        Ctype.flatten_fields (Ctype.object_fields sign.cty_self)
      in
      List.iter (fun (_, _, ty) -> mark_loops ty) fields;
      Vars.iter (fun _ (_, ty) -> mark_loops ty) sign.cty_vars
  | Tcty_fun (_, ty, cty) ->
      mark_loops ty;
      prepare_class_type params cty

let rec tree_of_class_type sch params =
  function
  | Tcty_constr (p', tyl, cty) ->
      let sty = Ctype.self_type cty in
      if List.memq sty !visited_objects
      || List.exists (fun ty -> (repr ty).desc <> Tvar) params
      then
        tree_of_class_type sch params cty
      else
        Octy_constr (tree_of_path p', tree_of_typlist true tyl)
  | Tcty_signature sign ->
      let sty = repr sign.cty_self in
      let self_ty =
        if is_aliased sty then Some (Otyp_var (false, name_of_type sty))
        else None
      in
      let (fields, _) =
        Ctype.flatten_fields (Ctype.object_fields sign.cty_self)
      in
      let csil = [] in
      let csil =
        List.fold_left (tree_of_metho sch sign.cty_concr) csil fields
      in
      let all_vars =
        Vars.fold (fun l (m, t) all -> (l, m, t) :: all) sign.cty_vars [] in
      let csil =
        List.fold_left
          (fun csil (l, m, t) ->
             Ocsg_value (l, m = Mutable, tree_of_typexp sch t) :: csil)
          csil all_vars
      in
      let csil =
        List.fold_left
          (fun csil (ty1, ty2) -> Ocsg_constraint (ty1, ty2) :: csil)
          csil (tree_of_constraints params)
      in
      Octy_signature (self_ty, List.rev csil)
  | Tcty_fun (l, ty, cty) ->
      let lab = if !print_labels && l <> "" || is_optional l then l else "" in
      let ty =
       if is_optional l then
         match (repr ty).desc with
         | Tconstr(path, [ty], _) when Path.same path Predef.path_option -> ty
         | _ -> newconstr (Path.Pident(Ident.create "<hidden>")) []
       else ty in
      let tr = tree_of_typexp sch ty in
      Octy_fun (lab, tr, tree_of_class_type sch params cty)

let class_type ppf cty =
  reset ();
  prepare_class_type [] cty;
  print_out_class_type ppf (tree_of_class_type false [] cty)

let tree_of_class_params = function
  | [] -> []
  | params ->
      let tyl = tree_of_typlist true params in
      List.map (function Otyp_var (_, s) -> s | _ -> "?") tyl

let tree_of_class_declaration id cl =
  let params = filter_params cl.cty_params in

  reset ();
  aliased := params @ !aliased;
  prepare_class_type params cl.cty_type;
  let sty = self_type cl.cty_type in
  List.iter mark_loops params;

  List.iter check_name_of_type (List.map proxy params);
  if is_aliased sty then check_name_of_type sty;

  let vir_flag = cl.cty_new = None in
  Osig_class
    (vir_flag, Ident.name id, tree_of_class_params params,
     tree_of_class_type true params cl.cty_type)

let class_declaration id ppf cl =
  !outcome_sig_item ppf (tree_of_class_declaration id cl)

let tree_of_cltype_declaration id cl =
  let params = List.map repr cl.clty_params in

  reset ();
  aliased := params @ !aliased;
  prepare_class_type params cl.clty_type;
  let sty = self_type cl.clty_type in
  List.iter mark_loops params;

  List.iter check_name_of_type (List.map proxy params);
  if is_aliased sty then check_name_of_type sty;

  let sign = Ctype.signature_of_class_type cl.clty_type in

  let virt =
    let (fields, _) =
      Ctype.flatten_fields (Ctype.object_fields sign.cty_self) in
    List.exists
      (fun (lab, _, ty) ->
         not (lab = "*dummy method*" || Concr.mem lab sign.cty_concr))
      fields in

  Osig_class_type
    (virt, Ident.name id, tree_of_class_params params,
     tree_of_class_type true params cl.clty_type)

let cltype_declaration id ppf cl =
  !outcome_sig_item ppf (tree_of_cltype_declaration id cl)

(* Print a module type *)

let rec tree_of_modtype = function
  | Tmty_ident p ->
      Omty_ident (tree_of_path p)
  | Tmty_signature sg ->
      Omty_signature (tree_of_signature sg)
  | Tmty_functor(param, ty_arg, ty_res) ->
      Omty_functor
        (Ident.name param, tree_of_modtype ty_arg, tree_of_modtype ty_res)

and tree_of_signature = function
  | [] -> []
  | item :: rem ->
      match item with
      | Tsig_value(id, decl) ->
          tree_of_value_description id decl :: tree_of_signature rem
      | Tsig_type(id, decl)  ->
          let (type_decl_list, rem) =
            let rec more_type_declarations = function
            | Tsig_type(id, decl) :: rem ->
                let (type_decl_list, rem) = more_type_declarations rem in
                (id, decl) :: type_decl_list, rem
            | rem -> [], rem in
            more_type_declarations rem
          in
          let type_decl_list =
            List.map (fun (id, decl) -> tree_of_type_decl id decl)
              ((id, decl) :: type_decl_list)
          in
          Osig_type type_decl_list
          ::
          tree_of_signature rem
      | Tsig_exception(id, decl)  ->
          Osig_exception (Ident.name id, tree_of_typlist false decl) ::
          tree_of_signature rem
      | Tsig_module(id, mty)  ->
          Osig_module (Ident.name id, tree_of_modtype mty) ::
          tree_of_signature rem
      | Tsig_modtype(id, decl)  ->
          tree_of_modtype_declaration id decl :: tree_of_signature rem
      | Tsig_class(id, decl) ->
          let rem =
            match rem with
            | ctydecl :: tydecl1 :: tydecl2 :: rem -> rem
            | _ -> []
          in
          tree_of_class_declaration id decl :: tree_of_signature rem
      | Tsig_cltype(id, decl) ->
          let rem =
            match rem with
            | tydecl1 :: tydecl2 :: rem -> rem
            | _ -> []
          in
          tree_of_cltype_declaration id decl :: tree_of_signature rem

and tree_of_modtype_declaration id decl =
  let mty =
    match decl with
    | Tmodtype_abstract -> Omty_abstract
    | Tmodtype_manifest mty -> tree_of_modtype mty
  in
  Osig_modtype (Ident.name id, mty)

let tree_of_module id mty = Osig_module (Ident.name id, tree_of_modtype mty)

let modtype ppf mty = print_out_module_type ppf (tree_of_modtype mty)
let modtype_declaration id ppf decl =
  !outcome_sig_item ppf (tree_of_modtype_declaration id decl)

(* Print a signature body (used by -i when compiling a .ml) *)

let print_signature ppf tree =
  fprintf ppf "@[<v>%a@]" print_signature_body tree
let outcome_signature = ref print_signature

let signature ppf sg =
  fprintf ppf "%a" !outcome_signature (tree_of_signature sg)

(* Print an unification error *)

let type_expansion t ppf t' =
  if t == t' then type_expr ppf t
  else fprintf ppf "@[<2>%a@ =@ %a@]" type_expr t type_expr t'

let rec trace fst txt ppf = function
  | (t1, t1') :: (t2, t2') :: rem ->
      if not fst then fprintf ppf "@,";
      fprintf ppf "@[Type@;<1 2>%a@ %s@;<1 2>%a@] %a"
       (type_expansion t1) t1' txt (type_expansion t2) t2'
       (trace false txt) rem
  | _ -> ()

let rec mismatch = function
  | [(_, t); (_, t')] -> (t, t')
  | _ :: _ :: rem -> mismatch rem
  | _ -> assert false

let rec filter_trace = function
  | (t1, t1') :: (t2, t2') :: rem ->
      let rem' = filter_trace rem in
      if t1 == t1' && t2 == t2'
      then rem'
      else (t1, t1') :: (t2, t2') :: rem'
  | _ -> []

(* Hide variant name and var, to force printing the expanded type *)
let hide_variant_name t =
  match repr t with
  | {desc = Tvariant row} as t when (row_repr row).row_name <> None ->
      newty2 t.level
        (Tvariant {(row_repr row) with row_name = None;
                   row_more = newty2 (row_more row).level Tvar})
  | _ -> t

let prepare_expansion (t, t') =
  let t' = hide_variant_name t' in
  mark_loops t; if t != t' then mark_loops t';
  (t, t')

let explanation unif t3 t4 ppf =
  match t3.desc, t4.desc with
  | Tfield _, Tvar | Tvar, Tfield _ ->
      fprintf ppf "@,Self type cannot escape its class"
  | Tconstr (p, _, _), Tvar
    when unif && t4.level < Path.binding_time p ->
      fprintf ppf
        "@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
        path p
  | Tvar, Tconstr (p, _, _)
    when unif && t3.level < Path.binding_time p ->
      fprintf ppf
        "@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
        path p
  | Tfield ("*dummy method*", _, _, _), _
  | _, Tfield ("*dummy method*", _, _, _) ->
      fprintf ppf
        "@,Self type cannot be unified with a closed object type"
  | Tfield (l, _, _, _), _ ->
      fprintf ppf
        "@,@[Only the first object type has a method %s@]" l
  | _, Tfield (l, _, _, _) ->
      fprintf ppf
        "@,@[Only the second object type has a method %s@]" l
  | _ -> ()

let unification_error unif tr txt1 ppf txt2 =
  reset ();
  let tr = List.map (fun (t, t') -> (t, hide_variant_name t')) tr in
  let (t3, t4) = mismatch tr in
  match tr with
  | [] | _ :: [] -> assert false
  | t1 :: t2 :: tr ->
    try
      let t1, t1' = prepare_expansion t1
      and t2, t2' = prepare_expansion t2 in
      print_labels := not !Clflags.classic;
      let tr = filter_trace tr in
      let tr = List.map prepare_expansion tr in
      fprintf ppf
        "@[<v>\
          @[%t@;<1 2>%a@ \
            %t@;<1 2>%a\
          @]%a%t\
         @]"
        txt1 (type_expansion t1) t1'
        txt2 (type_expansion t2) t2'
        (trace false "is not compatible with type") tr
        (explanation unif t3 t4);
      print_labels := true
    with exn ->
      print_labels := true;
      raise exn

let report_unification_error ppf tr txt1 txt2 =
  unification_error true tr txt1 ppf txt2;;

let trace fst txt ppf tr =
  print_labels := not !Clflags.classic;
  try match tr with
    t1 :: t2 :: tr' ->
      if fst then trace fst txt ppf (t1 :: t2 :: filter_trace tr')
      else trace fst txt ppf (filter_trace tr);
      print_labels := true
  | _ -> ()
  with exn ->
    print_labels := true;
    raise exn

let report_subtyping_error ppf tr1 txt1 tr2 =
  reset ();
  let tr1 = List.map prepare_expansion tr1
  and tr2 = List.map prepare_expansion tr2 in
  trace true txt1 ppf tr1;
  if tr2 = [] then () else
  let t3, t4 = mismatch tr2 in
  trace false "is not compatible with type" ppf tr2;
  explanation true t3 t4 ppf
