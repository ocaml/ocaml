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
  row.row_name <> None && row.row_closed &&
  List.for_all
    (fun (_, f) ->
       match row_field_repr f with
       | Reither(c, l, _, _) -> if c then l = [] else List.length l = 1
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
            if all_present then
              Otyp_constr (id, args)
            else
              let non_gen = is_non_gen sch px in
              let tags = List.map fst present in
              Otyp_class (non_gen, tree_of_path p, args, tags)
        | _ ->
            let non_gen =
              not (row.row_closed && all_present) && is_non_gen sch px
            in
            let row_fields = List.map (tree_of_row_field sch) fields in
            let tags =
              if all_present then None else Some (List.map fst present)
            in
            Otyp_variant (non_gen, row_fields, row.row_closed, tags)
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
  | ty :: tyl -> tree_of_typexp sch ty :: tree_of_typlist sch tyl

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
      Otyp_class (non_gen, tree_of_path p, args, [])
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
  | Otyp_var (ng, s) ->
      fprintf ppf "'%s%s" (if ng then "_" else "") s
  | Otyp_constr (id, tyl) ->
      fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
  | Otyp_stuff s ->
      fprintf ppf "%s" s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let print_present ppf =
        function
        | None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a]@]"
        (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "< "
         else if tags = None then "> " else "? ")
        (print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| "))
        row_fields
        print_present tags
  | Otyp_object (fields, rest) ->
      fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
  | Otyp_class (ng, id, tyl, tags) ->
      let print_present ppf =
        function
        | [] -> ()
        | l -> fprintf ppf "@[<hov>[>%a@]" pr_present l
      in
      fprintf ppf "@[%a%s#%a%a@]" print_typargs tyl
        (if ng then "_" else "") print_ident id print_present tags
  | Otyp_alias (_, _) | Otyp_arrow (_, _, _) | Otyp_tuple _ as ty ->
      fprintf ppf "@[<1>(%a)@]" print_out_type ty

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

let outcome_type_hook = ref print_out_type

let typexp sch prio ppf ty =
  !outcome_type_hook ppf (tree_of_typexp sch ty)

let type_expr ppf ty = typexp false 0 ppf ty

and type_sch ppf ty = typexp true 0 ppf ty

and type_scheme ppf ty = reset_and_mark_loops ty; typexp true 0 ppf ty

(* Print one type declaration *)

let constrain ppf ty =
  let ty' = unalias ty in
  if ty != ty'
  then fprintf ppf "@ @[<2>constraint %a =@ %a@]" type_sch ty type_sch ty'

let filter_params tyl =
  let params =
    List.fold_left
      (fun tyl ty ->
        let ty = repr ty in
        if List.memq ty tyl then Btype.newgenty (Tsubst ty) :: tyl
        else ty :: tyl)
      [] tyl
  in List.rev params

let rec type_decl kwd id ppf decl =

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

  let print_constraints ppf params =
    List.iter (constrain ppf) params in

  let type_parameter ppf (ty,(co,cn)) =
    fprintf ppf "%s%a"
      (if not cn then "+" else if not co then "-" else "")
      type_expr ty
  in
  let type_defined ppf decl =
    if decl.type_kind = Type_abstract && decl.type_manifest = None
       && List.exists (fun x -> x <> (true, true)) decl.type_variance then
      fprintf ppf "(@[%a)@]@ %a"
        (print_list type_parameter (fun ppf -> fprintf ppf ",@ "))
        (List.combine params decl.type_variance)
        ident id
    else
      type_expr ppf (Btype.newgenty (Tconstr(Pident id, params, ref Mnil)))
  in
  let print_manifest ppf decl =
    match decl.type_manifest with
    | None -> ()
    | Some ty -> fprintf ppf " =@ %a" type_expr ty in

  let print_name_args ppf decl =
    fprintf ppf "%s%a%a"
      kwd type_defined decl
      print_manifest decl in

  begin match decl.type_kind with
  | Type_abstract ->
      fprintf ppf "@[<2>@[<hv 2>%a@]%a@]"
        print_name_args decl print_constraints params
  | Type_variant [] -> ()
      (* A fatal error actually, except when printing type exn... *)
  | Type_variant cstrs ->
      fprintf ppf "@[<2>@[<hv 2>%a =@;<1 2>%a@]%a@]"
        print_name_args decl
        (print_list constructor (fun ppf -> fprintf ppf "@ | ")) cstrs
        print_constraints params
  | Type_record(lbls, rep) ->
      fprintf ppf "@[<2>@[<hv 2>%a = {%a@;<1 -2>}@]@ %a@]"
        print_name_args decl
        (print_list_init label (fun ppf -> fprintf ppf "@ ")) lbls
        print_constraints params
  end

and constructor ppf (name, args) =
  match args with
  | [] -> fprintf ppf "%s" name
  | _ ->
      let tyl = tree_of_typlist false args in
      fprintf ppf "@[<2>%s of@ %a@]" name
        (print_typlist print_simple_out_type " *") tyl

and label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s%s :@ %a@];" (string_of_mutable mut) name type_expr arg

and string_of_mutable = function
  | Immutable -> ""
  | Mutable -> "mutable "

let type_declaration id decl = type_decl "type " id decl

(* Print an exception declaration *)

let exception_declaration id ppf decl =
  fprintf ppf "exception %a" constructor (Ident.name id, decl)

(* Print a value declaration *)

let value_ident ppf id =
  let name = Ident.name id in
  if List.mem name
      ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"]
  then fprintf ppf "( %s )" name
  else match name.[0] with
  | 'a' .. 'z' | '\223' .. '\246' | '\248' .. '\255' | '_' -> ident ppf id
  | _ -> fprintf ppf "( %s )" name

let value_description id ppf decl =
  let kwd = if decl.val_kind = Val_reg then "val " else "external " in
  let pr_val ppf =
    match decl.val_kind with
    | Val_prim p ->
        fprintf ppf "@ = %a" Primitive.print_description p
    | _ -> () in
  fprintf ppf "@[<2>%s%a :@ %a%t@]"
    kwd value_ident id type_scheme decl.val_type pr_val

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

let rec prepare_class_type = function
  | Tcty_constr (p, tyl, cty) ->
      let sty = Ctype.self_type cty in
      begin try
        if List.memq sty !visited_objects then raise (Unify []);
        List.iter (occur Env.empty sty) tyl;
        List.iter mark_loops tyl
      with Unify _ ->
        prepare_class_type cty
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
      prepare_class_type cty

let rec perform_class_type sch params ppf = function
  | Tcty_constr (p', tyl, cty) ->
      let sty = Ctype.self_type cty in
      if List.memq sty !visited_objects then
        perform_class_type sch params ppf cty
      else
        let pr_tyl ppf = function
          | [] -> ()
          | tyl ->
              let tyl = tree_of_typlist true tyl in
              fprintf ppf "@[<1>[%a]@]@ "
                (print_typlist print_out_type ",") tyl in
        fprintf ppf "@[%a%a@]" pr_tyl tyl path p'
  | Tcty_signature sign ->
      let sty = repr sign.cty_self in
      let pr_param ppf sty =
       if is_aliased sty then
        fprintf ppf "@ @[('%a)@]" print_name_of_type sty in

      fprintf ppf "@[<hv 2>@[<2>object%a@]%a"
              pr_param sty
              (fun ppf l -> List.iter (constrain ppf) l) params;
      Vars.iter (class_var sch ppf) sign.cty_vars;
      let (fields, _) =
        Ctype.flatten_fields (Ctype.object_fields sign.cty_self) in
      List.iter (metho sch sign.cty_concr ppf) fields;
      fprintf ppf "@;<1 -2>end@]"
  | Tcty_fun (l, ty, cty) ->
      let ty =
       if is_optional l then
         match (repr ty).desc with
         | Tconstr(path, [ty], _) when Path.same path Predef.path_option -> ty
         | _ -> newconstr (Path.Pident(Ident.create "<hidden>")) []
       else ty in
      fprintf ppf "@[%a%a ->@ %a@]"
       print_label l (typexp sch 2) ty (perform_class_type sch params) cty

let class_type ppf cty =
  reset ();
  prepare_class_type cty;
  perform_class_type false [] ppf cty

let class_params ppf = function
  | [] -> ()
  | params ->
      let tyl = tree_of_typlist true params in
      fprintf ppf "@[<1>[%a]@]@ " (print_typlist print_out_type ",") tyl

let class_declaration id ppf cl =
  let params = filter_params cl.cty_params in

  reset ();
  aliased := params @ !aliased;
  prepare_class_type cl.cty_type;
  let sty = self_type cl.cty_type in
  List.iter mark_loops params;

  List.iter check_name_of_type (List.map proxy params);
  if is_aliased sty then check_name_of_type sty;

  let vir_mark = if cl.cty_new = None then " virtual" else "" in
  fprintf ppf "@[<2>class%s@ %a%a@ :@ %a@]" vir_mark
    class_params params ident id (perform_class_type true params) cl.cty_type

let cltype_declaration id ppf cl =
  let params = List.map repr cl.clty_params in

  reset ();
  aliased := params @ !aliased;
  prepare_class_type cl.clty_type;
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

  let vir_mark = if virt then " virtual" else "" in
  fprintf ppf "@[<2>class type%s@ %a%a@ =@ %a@]"
   vir_mark class_params params 
   ident id
   (perform_class_type true params) cl.clty_type

(* Print a module type *)

let rec modtype ppf = function
  | Tmty_ident p ->
      path ppf p
  | Tmty_signature sg ->
      fprintf ppf "@[<hv 2>sig%a@;<1 -2>end@]" (signature_body true) sg
  | Tmty_functor(param, ty_arg, ty_res) ->
      fprintf ppf "@[<2>functor@ (%a : %a) ->@ %a@]"
       ident param modtype ty_arg modtype ty_res

and signature_body spc ppf = function
  | [] -> ()
  | item :: rem ->
      if spc then fprintf ppf "@ ";
      let cont =
        match item with
        | Tsig_value(id, decl) ->
            value_description id ppf decl; rem
        | Tsig_type(id, decl)  ->
            type_declaration id ppf decl;
            let rec more_type_declarations = function
            | Tsig_type(id, decl) :: rem ->
                fprintf ppf "@ %a" (type_decl "and " id) decl;
                more_type_declarations rem
            | rem -> rem in
            more_type_declarations rem
        | Tsig_exception(id, decl)  ->
            exception_declaration id ppf decl; rem
        | Tsig_module(id, mty)  ->
            fprintf ppf "@[<2>module %a :@ %a@]" ident id modtype mty; rem
        | Tsig_modtype(id, decl)  ->
            modtype_declaration id ppf decl; rem
        | Tsig_class(id, decl) ->
            class_declaration id ppf decl;
            begin match rem with
            | ctydecl :: tydecl1 :: tydecl2 :: rem -> rem
            | _ -> []
            end
        | Tsig_cltype(id, decl) ->
            cltype_declaration id ppf decl;
            begin match rem with
            | tydecl1 :: tydecl2 :: rem -> rem
            | _ -> []
            end
      in signature_body true ppf cont

and modtype_declaration id ppf decl =
  let pr_decl ppf = function
    | Tmodtype_abstract -> ()
    | Tmodtype_manifest mty -> fprintf ppf " =@ %a" modtype mty in
  fprintf ppf "@[<2>module type %a%a@]" ident id pr_decl decl

(* Print a signature body (used by -i when compiling a .ml) *)

let signature ppf sg = fprintf ppf "@[<v>%a@]" (signature_body false) sg

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

(* Hide variant name, to force printing the expanded type *)
let hide_variant_name t =
  match repr t with
  | {desc = Tvariant row} as t when (row_repr row).row_name <> None ->
      newty2 t.level (Tvariant {(row_repr row) with row_name = None})
  | _ -> t

let prepare_expansion (t, t') =
  let t' = hide_variant_name t' in
  mark_loops t; if t != t' then mark_loops t';
  (t, t')

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
      let explanation ppf =
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
        | _ -> () in
      fprintf ppf
        "@[<v>\
          @[%t@;<1 2>%a@ \
            %t@;<1 2>%a\
          @]%a%t\
         @]"
        txt1 (type_expansion t1) t1'
        txt2 (type_expansion t2) t2'
        (trace false "is not compatible with type") tr
        explanation;
      print_labels := true
    with exn ->
      print_labels := true;
      raise exn

let report_unification_error ppf tr txt1 txt2 =
  unification_error true tr txt1 ppf txt2;;

let trace fst txt ppf tr =
  print_labels := not !Clflags.classic;
  try match tr with
    t1 :: t2 :: tr ->
      trace fst txt ppf (t1 :: t2 :: filter_trace tr);
      print_labels := true
  | _ -> ()
  with exn ->
    print_labels := true;
    raise exn

