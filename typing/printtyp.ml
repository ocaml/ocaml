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
(*open Formatmsg*)
open Longident
open Path
open Asttypes
open Types
open Btype

(* Print a long identifier *)

let rec longident ppf = function
  | Lident s -> fprintf ppf "%s" s
  | Ldot(p, s) -> fprintf ppf "%a.%s" longident p s
  | Lapply(p1, p2) -> fprintf ppf "%a(%a)" longident p1 longident p2

(* Print an identifier *)

let ident ppf id = fprintf ppf "%s" (Ident.name id)

(* Print a path *)

let ident_pervasive = Ident.create_persistent "Pervasives"

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

(*
let remove_name_of_type t =
  names := List.remove_assq t !names
*)

let print_name_of_type ppf t = fprintf ppf "%s" (name_of_type t)

let visited_objects = ref ([] : type_expr list)
let aliased = ref ([] : type_expr list)

let add_alias px =
  if not (List.memq px !aliased) then aliased := px :: !aliased

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
       | Reither(c, l, _) -> if c then l = [] else List.length l = 1
       | _ -> true)
    row.row_fields

let rec mark_loops_rec visited ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.memq px visited then add_alias px else
    let visited = px :: visited in
    match ty.desc with
    | Tvar -> ()
    | Tarrow(_, ty1, ty2) ->
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
              iter_row (mark_loops_rec visited) row
         end
    | Tobject (fi, nm) ->
        if List.memq px !visited_objects then add_alias px else
         begin
          if opened_object ty then
            visited_objects := px :: !visited_objects;
          let name =
            match !nm with
            | None -> None
            | Some (n, v :: l) ->
                let v' = repr v in
                begin match v'.desc with
                | Tvar -> Some (n, v' :: l)
                | _ -> None
                end
            | _ ->
                fatal_error "Printtyp.mark_loops_rec"
          in
          nm := name;
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

let rec print_list_term pr sep ppf = function
  | [] -> ()
  | a :: l -> pr ppf a; sep (); print_list_term pr sep ppf l;;

let rec print_list_init pr sep ppf = function
  | [] -> ()
  | a :: l -> sep (); pr ppf a; print_list_init pr sep ppf l;;

(*
let rec print_list pr sep ppf = function
  | [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep (); print_list pr sep ppf l;;
*)

let rec typexp sch prio0 ppf ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.mem_assq px !names then
   let mark = if px.desc = Tvar then non_gen_mark sch px else "" in
   fprintf ppf "'%s%a" mark print_name_of_type px else

  let pr_typ ppf prio =
   (match ty.desc with
    | Tvar ->
        fprintf ppf "'%s%a" (non_gen_mark sch ty) print_name_of_type ty
    | Tarrow(l, ty1, ty2) ->
        let pr_arrow l ty1 ppf ty2 =
          print_label ppf l;
          if is_optional l then
            match (repr ty1).desc with
            | Tconstr(path, [ty], _) when path = Predef.path_option ->
                typexp sch 2 ppf ty
            | _ -> assert false
          else typexp sch 2 ppf ty1;
          fprintf ppf " ->@ %a" (typexp sch 1) ty2 in
        if prio >= 2
        then fprintf ppf "@[<1>(%a)@]" (pr_arrow l ty1) ty2
        else fprintf ppf "@[<0>%a@]" (pr_arrow l ty1) ty2
    | Ttuple tyl ->
        if prio >= 3
        then fprintf ppf "@[<1>(%a)@]" (typlist sch 3 " *") tyl
        else fprintf ppf "@[<0>%a@]" (typlist sch 3 " *") tyl
    | Tconstr(p, tyl, abbrev) ->
        fprintf ppf "@[%a%a@]" (typargs sch) tyl path p
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
        let pr_present ppf l =
          fprintf ppf "@[<hv>%a@]"
            (print_list_init (fun ppf (s, _) -> fprintf ppf "`%s" s)
                             (fun () -> fprintf ppf "@ | "))
            l in
        begin match row.row_name with
        | Some(p, tyl) when namable_row row ->
            let sharp_mark =
              if not all_present then non_gen_mark sch px ^ "#" else "" in
            let print_present ppf = function
              | [] -> ()
              | l ->
                 if not all_present then fprintf ppf "[>%a]" pr_present l in
            fprintf ppf "@[%a%s%a%a@]"
              (typargs sch) tyl sharp_mark path p print_present present
        | _ ->
            let gen_mark =
              if not (row.row_closed && all_present)
              then non_gen_mark sch px
              else "" in
            let close_mark =
              if not all_present then "<" else
              if row.row_closed then "" else
              if fields = [] then "< .." else ">" in
            let pr_ellipsis ppf =
              if not (row.row_closed || all_present)
              then fprintf ppf "| .." in
            let print_present ppf = function
              | [] -> ()
              | l ->
                 if not all_present then fprintf ppf "@ > %a" pr_present l in
            let print_fields =
              print_list_init (row_field sch) (fun () -> fprintf ppf "@ | ") in

            fprintf ppf "%s@[<hv>[%s%a%t%a@ ]@]"
              gen_mark close_mark print_fields fields pr_ellipsis
              print_present present
        end
    | Tobject (fi, nm) ->
        typobject sch ty fi ppf nm
    | Tsubst ty ->
        typexp sch prio ppf ty
    | _ ->
        fatal_error "Printtyp.typexp"
   ) in
  if List.memq px !aliased then begin
    check_name_of_type px;
    if prio0 >= 1
    then printf "@[<1>(%a as '%a)@]" pr_typ 0 print_name_of_type px
    else printf "@[%a as '%a@]" pr_typ prio0 print_name_of_type px end
  else pr_typ ppf prio0

and row_field sch ppf (l, f) =
  let pr_field ppf f =
    match row_field_repr f with
    | Rpresent None | Reither(true, [], _) -> ()
    | Rpresent(Some ty) -> fprintf ppf "@ %a" (typexp sch 0) ty
    | Reither(c, tyl,_) ->
        if c
        then fprintf ppf "@ &@ %a" (typlist sch 0 " &") tyl
        else fprintf ppf "@ %a" (typlist sch 0 " &") tyl
    | Rabsent -> fprintf ppf "@ []" in
  fprintf ppf "@[<hv 2>`%s%a@]" l pr_field f

(* typlist is simply 
   print_list (typexp sch prio) (fun () -> fprintf ppf "%s@ " sep) *)
and typlist sch prio sep ppf = function
  | [] -> ()
  | [ty] -> typexp sch prio ppf ty
  | ty :: tyl ->
      fprintf ppf "%a%s@ %a"
       (typexp sch prio) ty sep (typlist sch prio sep) tyl

and typargs sch ppf = function
  | [] -> ()
  | [ty1] -> fprintf ppf "%a@ " (typexp sch 3) ty1
  | tyl -> fprintf ppf "@[<1>(%a)@]@ " (typlist sch 0 ",") tyl

and typobject sch ty fi ppf nm =
  begin match !nm with
  | None ->
      let pr_fields ppf fi =
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
        typfields sch rest ppf sorted_fields in
      fprintf ppf "@[<2>< %a >@]" pr_fields fi
  | Some (p, {desc = Tvar} :: tyl) ->
      fprintf ppf "@[%a%s#%a@]" (typargs sch) tyl (non_gen_mark sch ty) path p
  | _ ->
        fatal_error "Printtyp.typobject"
  end

and non_gen_mark sch ty =
    if sch && ty.level <> generic_level then "_" else "" 

and typfields sch rest ppf = function
  | [] ->
      begin match rest.desc with
      | Tvar -> fprintf ppf "%s.." (non_gen_mark sch rest)
      | Tnil -> ()
      | _ -> fatal_error "typfields (1)"
      end
  | [(s, t)] ->
      fprintf ppf "%s : %a" s (typexp sch 0) t;
      begin match rest.desc with
      | Tvar -> fprintf ppf ";@ "
      | Tnil -> ()
      | _ -> fatal_error "typfields (2)"
      end;
      typfields sch rest ppf []
  | (s, t) :: l ->
      fprintf ppf "%s : %a;@ %a" s (typexp sch 0) t (typfields sch rest) l

let type_expr ppf ty = typexp false 0 ppf ty

and type_sch ppf ty = typexp true 0 ppf ty

and type_scheme ppf ty = reset_and_mark_loops ty; typexp true 0 ppf ty

(* Print one type declaration *)

let constrain ppf ty =
  let ty' = unalias ty in
  if ty != ty'
  then fprintf ppf "@ @[<2>constraint %a =@ %a@]" type_sch ty type_sch ty'

let rec type_decl kwd id ppf decl =

  reset();

  let params = List.map repr decl.type_params in

  aliased := params @ !aliased;
  List.iter mark_loops params;
  List.iter check_name_of_type params;
  begin match decl.type_manifest with
  | None -> ()
  | Some ty -> mark_loops ty
  end;
  begin match decl.type_kind with
  | Type_abstract -> ()
  | Type_variant [] -> ()
  | Type_variant cstrs ->
      List.iter (fun (_, args) -> List.iter mark_loops args) cstrs
  | Type_record l ->
      List.iter (fun (_, _, ty) -> mark_loops ty) l
  end;

  let print_constraints ppf params =
    List.iter (constrain ppf) params in

  let print_manifest ppf decl =
    match decl.type_manifest with
    | None -> ()
    | Some ty -> fprintf ppf " =@ %a" type_expr ty in

  let print_name_args ppf decl =
    fprintf ppf "%s%a%a"
      kwd type_expr (Btype.newgenty (Tconstr(Pident id, params, ref Mnil)))
      print_manifest decl in

  begin match decl.type_kind with
  | Type_abstract ->
      fprintf ppf "@[<2>@[<hv 3>%a@]@ %a@]"
        print_name_args decl print_constraints params
  | Type_variant [] -> ()
      (* A fatal error actually, except when printing type exn... *)
  | Type_variant cstrs ->
      fprintf ppf "@[<2>@[<hv 3>%a =%a@]@ %a@]"
        print_name_args decl
        (print_list_init constructor (fun () -> fprintf ppf "@ | ")) cstrs
        print_constraints params
  | Type_record lbls ->
      fprintf ppf "@[<2>@[@[<hv 2>%a = {%a@]@ }@]@ %a@]" print_name_args decl
        (print_list_init label (fun () -> fprintf ppf "@ ")) lbls
        print_constraints params
  end

and constructor ppf (name, args) =
  match args with
  | [] -> fprintf ppf "%s" name
  | _ -> fprintf ppf "@[<2>%s of@ %a@]" name (typlist false 3 " *") args

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
        fprintf ppf "@ = "; Primitive.print_description p
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
      if List.memq sty !visited_objects then begin
        if not (List.memq sty !aliased) then
          aliased := sty :: !aliased
      end else
        visited_objects := sty :: !visited_objects;
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
          | tyl -> fprintf ppf "@[<1>[%a]@]@ " (typlist true 0 ",") tyl in
        fprintf ppf "@[%a%a@]" pr_tyl tyl path p'
  | Tcty_signature sign ->
      let sty = repr sign.cty_self in
      let pr_param ppf sty =
       if List.memq sty !aliased then
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
         | Tconstr(path, [ty], _) when path = Predef.path_option -> ty
         | _ -> assert false
       else ty in
      fprintf ppf "@[%a%a ->@ %a@]"
       print_label l (typexp sch 2) ty (perform_class_type sch params) cty

let class_type ppf cty =
  reset ();
  prepare_class_type cty;
  perform_class_type false [] ppf cty

let class_params ppf = function
  | [] -> ()
  | params -> fprintf ppf "@[<1>[%a]@]@ " (typlist true 0 ",") params

let class_declaration id ppf cl =
  let params = List.map repr cl.cty_params in

  reset ();
  aliased := params @ !aliased;
  prepare_class_type cl.cty_type;
  let sty = self_type cl.cty_type in
  List.iter mark_loops params;

  List.iter check_name_of_type params;
  if List.memq sty !aliased then
    check_name_of_type sty;

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

  List.iter check_name_of_type params;
  if List.memq sty !aliased then
    check_name_of_type sty;

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
            match rem with tydecl1 :: tydecl2 :: rem -> rem | _ -> []
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
  try
    trace fst txt ppf (filter_trace tr);
    print_labels := true
  with exn ->
    print_labels := true;
    raise exn

