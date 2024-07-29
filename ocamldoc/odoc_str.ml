(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The functions to get a string from different kinds of elements (types, modules, ...). *)

module Name = Odoc_name
let () = Out_type.Ident_names.enable false

let string_of_variance t v =
  if ( t.Odoc_type.ty_kind = Odoc_type.Type_abstract ||
      t.Odoc_type.ty_kind = Odoc_type.Type_open ) &&
    t.Odoc_type.ty_manifest = None
  then
    let inj =
      if t.Odoc_type.ty_kind = Odoc_type.Type_abstract
      && Types.Variance.(mem Inj v)
      then "!"
      else ""
    in
    match Types.Variance.get_upper v with
    | (true, false) -> inj ^ "+"
    | (false, true) -> inj ^ "-"
    | _ -> inj
  else
    ""
let rec is_arrow_type t =
  match Types.get_desc t with
    Types.Tarrow _ -> true
  | Types.Tlink t2 -> is_arrow_type t2
  | Types.Ttuple _
  | Types.Tconstr _
  | Types.Tvar _ | Types.Tunivar _ | Types.Tobject _ | Types.Tpoly _
  | Types.Tfield _ | Types.Tnil | Types.Tvariant _ | Types.Tpackage _ -> false
  | Types.Tsubst _ -> assert false


let rec need_parent t =
  match Types.get_desc t with
    Types.Tarrow _ | Types.Ttuple _ -> true
  | Types.Tlink t2 -> need_parent t2
  | Types.Tconstr _
  | Types.Tvar _ | Types.Tunivar _ | Types.Tobject _ | Types.Tpoly _
  | Types.Tfield _ | Types.Tnil | Types.Tvariant _ | Types.Tpackage _ -> false
  | Types.Tsubst _ -> assert false

let print_type_scheme ppf t =
  if need_parent t then
    Format.fprintf ppf "(%a)" Printtyp.shared_type_scheme t
  else
    Printtyp.shared_type_scheme ppf t

let print_type_param decl ppf (param,v) =
  (* HACK: we print type parameters as type expressions, and amend ["'_"] to ["_"] *)
  let ty = Format.asprintf "%a" Printtyp.shared_type_scheme param in
  let ty = if ty = "'_" then "_" else ty in
  let var = string_of_variance decl v in
  if need_parent param then
    Format.fprintf  ppf "(%s%s)" var ty
  else
    Format.fprintf ppf "%s%s" var ty

let raw_string_of_type_list sep elt ppf type_list =
  let pp_sep ppf () = Format.fprintf ppf "@,%s" sep in
  Format.fprintf ppf "@[<hov 2>%a@]"
    (Format.pp_print_list ~pp_sep elt) type_list

let string_of_type_list ?par sep type_list =
  let par =
    match par with
    | Some b -> b
    | None ->
        match type_list with
          [] | [_] -> false
        | _ -> true
  in
  Format.asprintf "%s%a%s"
    (if par then "(" else "")
    (raw_string_of_type_list sep print_type_scheme) type_list
    (if par then ")" else "")

let string_of_type_param_list t =
  let par =
    match t.Odoc_type.ty_parameters with
      [] | [_] -> false
    | _ -> true
  in
  Format.asprintf "%s%a%s"
    (if par then "(" else "")
    (raw_string_of_type_list ", " @@ print_type_param t)
          t.Odoc_type.ty_parameters
    (if par then ")" else "")

let string_of_type_extension_param_list te =
  let par =
    match te.Odoc_extension.te_type_parameters with
      [] | [_] -> false
    | _ -> true
  in
  Format.asprintf "%s%a%s"
    (if par then "(" else "")
    (raw_string_of_type_list ", " print_type_scheme)
          te.Odoc_extension.te_type_parameters
    (if par then ")" else "")


let string_of_class_type_param_list l =
  let par =
    match l with
      [] | [_] -> false
    | _ -> true
  in
  Format.asprintf "%s%a%s"
    (if par then "[" else "")
    (raw_string_of_type_list ", " print_type_scheme)
    l
    (if par then "]" else "")

let string_of_class_params c =
  let b = Buffer.create 256 in
  let rec iter = function
      Types.Cty_arrow (label, t, ctype) ->
        let parent = is_arrow_type t in
        Printf.bprintf b "%s%s%s%s -> "
          (
           match label with
             Asttypes.Nolabel -> ""
           | s -> Asttypes.string_of_label s ^":"
          )
          (if parent then "(" else "")
          (Odoc_print.string_of_type_expr
             (if Odoc_misc.is_optional label then
               Odoc_misc.remove_option t
             else
               t
             )
          )
          (if parent then ")" else "");
        iter ctype
    | Types.Cty_signature _
    | Types.Cty_constr _ -> ()
  in
  iter c.Odoc_class.cl_type;
  Buffer.contents b

let bool_of_private = function
  | Asttypes.Private -> true
  | _ -> false

let field_doc_str = function
  | None -> ""
  | Some t -> Printf.sprintf "(* %s *)" (Odoc_misc.string_of_info t)

let string_of_record l =
  let module M = Odoc_type in
  let module P = Printf in
  P.sprintf "{\n%s\n}" (
    String.concat "\n" (
      List.map (fun field ->
          P.sprintf "   %s%s : %s;%s"
            (if field.M.rf_mutable then "mutable " else "") field.M.rf_name
            (Odoc_print.string_of_type_expr field.M.rf_type)
            (field_doc_str field.M.rf_text)
        ) l
    )
  )

let string_of_type t =
  let module M = Odoc_type in
   let module P = Printf in
   let priv = bool_of_private t.M.ty_private in
   let parameters_str =
     String.concat " " (
       List.map (fun (p, v) ->
         (string_of_variance t v) ^ (Odoc_print.string_of_type_expr p)
       ) t.M.ty_parameters
     )
   in
   let manifest_str =
     match t.M.ty_manifest with
     | None -> ""
     | Some (M.Object_type fields) ->
       P.sprintf "= %s<\n%s\n>\n" (if priv then "private " else "") (
         String.concat "\n" (
           List.map (fun field ->
             P.sprintf "   %s : %s;%s" field.M.of_name
               (Odoc_print.string_of_type_expr field.M.of_type)
               (field_doc_str field.M.of_text)
           ) fields
        )
     )
   | Some (M.Other typ) ->
     "= " ^ (if priv then "private " else "" ) ^
       (Odoc_print.string_of_type_expr typ) ^ " "
 in
 let type_kind_str =
   match t.M.ty_kind with
   | M.Type_abstract -> ""
   | M.Type_variant l ->
     P.sprintf "=%s\n%s\n" (if priv then " private" else "") (
       String.concat "\n" (
         List.map (fun cons ->
           let comment =
             match cons.M.vc_text with
             | None -> ""
             | Some t -> P.sprintf "(* %s *)" (Odoc_misc.string_of_info t)
           in
           let string_of_parameters = function
             | M.Cstr_tuple l ->
                 String.concat " * " (
                   List.map (fun t -> "("^Odoc_print.string_of_type_expr t^")") l
                 )
             | M.Cstr_record l ->
                 string_of_record l
           in
           P.sprintf "  | %s%s%s" cons.M.vc_name (
             match cons.M.vc_args, cons.M.vc_ret with
              | M.Cstr_tuple [], None -> ""
              | li, None -> " of " ^ (string_of_parameters li)
              | M.Cstr_tuple [], Some r -> " : " ^ Odoc_print.string_of_type_expr r
              | li, Some r ->
                 P.sprintf " : %s -> %s" (string_of_parameters li)
                   (Odoc_print.string_of_type_expr r)
             ) comment
           ) l
        )
      )

  | M.Type_open ->
      "= .." (* FIXME MG: when introducing new constructors next time,
                thanks to setup a minimal correct output *)
  | M.Type_record l ->
     P.sprintf "= %s{\n%s\n}\n" (if priv then "private " else "")
       (string_of_record l)
 in
 P.sprintf "type %s %s %s%s%s" parameters_str (Name.simple t.M.ty_name)
   manifest_str type_kind_str
   (match t.M.ty_info with
    | None -> ""
    | Some info -> Odoc_misc.string_of_info info)

let string_of_type_extension te =
  let module M = Odoc_extension in
  let module T = Odoc_type in
    "type "
    ^(String.concat ""
        (List.map
           (fun p -> (Odoc_print.string_of_type_expr p)^" ")
           te.M.te_type_parameters
        ))
    ^te.M.te_type_name
    ^" += "
    ^(if (bool_of_private te.M.te_private) then "private " else "")
    ^"\n"
    ^(String.concat ""
        (List.map
           (fun x ->
              "  | "
              ^(Name.simple x.M.xt_name)
              ^(match x.M.xt_args, x.M.xt_ret with
                  | T.Cstr_tuple [], None -> ""
                  | T.Cstr_tuple l, None ->
                      " of " ^
                        (String.concat " * "
                           (List.map
                              (fun t -> "("^Odoc_print.string_of_type_expr t^")") l))
                  | T.Cstr_tuple [], Some r -> " : " ^ Odoc_print.string_of_type_expr r
                  | T.Cstr_tuple l, Some r ->
                      " : " ^
                        (String.concat " * "
                           (List.map
                              (fun t -> "("^Odoc_print.string_of_type_expr t^")") l))
                      ^ " -> " ^ Odoc_print.string_of_type_expr r
                  | T.Cstr_record l, None ->
                      " of " ^  string_of_record l
                  | T.Cstr_record l, Some r ->
                      " : " ^ string_of_record l ^ " -> "
                      ^ Odoc_print.string_of_type_expr r
               )
              ^(match x.M.xt_alias with
                    None -> ""
                  | Some xa ->
                      " = "^
                        (match xa.M.xa_xt with
                             None -> xa.M.xa_name
                           | Some x2 -> x2.M.xt_name
                        )
               )
              ^(match x.M.xt_text with
                    None ->
                      ""
                  | Some t ->
                      "(* "^(Odoc_misc.string_of_info t)^" *)"
               )^"\n"
           )
           te.M.te_constructors))
    ^(match te.M.te_info with
          None -> ""
        | Some i -> Odoc_misc.string_of_info i
     )

let string_of_exception e =
  let module T = Odoc_type in
  let module M = Odoc_exception in
  "exception "^(Name.simple e.M.ex_name)^
  (match e.M.ex_args, e.M.ex_ret with
     T.Cstr_tuple [], None -> ""
   | T.Cstr_tuple l,None ->
       " of "^
       (String.concat " * "
         (List.map (fun t -> "("^(Odoc_print.string_of_type_expr t)^")") l))
   | T.Cstr_tuple [],Some r ->
       " : "^
       (Odoc_print.string_of_type_expr r)
   | T.Cstr_tuple l,Some r ->
       " : "^
       (String.concat " * "
         (List.map (fun t -> "("^(Odoc_print.string_of_type_expr t)^")") l))^
       " -> "^
       (Odoc_print.string_of_type_expr r)
   | T.Cstr_record l, None ->
       " of " ^  string_of_record l
   | T.Cstr_record l, Some r ->
       " : " ^ string_of_record l ^ " -> "
       ^ Odoc_print.string_of_type_expr r
  )^
  (match e.M.ex_alias with
    None -> ""
  | Some ea ->
      " = "^
      (match ea.M.ea_ex with
        None -> ea.M.ea_name
      | Some e2 -> e2.M.ex_name
      )
  )^"\n"^
  (match e.M.ex_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

let string_of_value v =
  let module M = Odoc_value in
  "val "^(Name.simple v.M.val_name)^" : "^
  (Odoc_print.string_of_type_expr v.M.val_type)^"\n"^
  (match v.M.val_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

let string_of_attribute a =
  let module M = Odoc_value in
  "val "^
  (if a.M.att_virtual then "virtual " else "")^
  (if a.M.att_mutable then Odoc_messages.mutab^" " else "")^
  (Name.simple a.M.att_value.M.val_name)^" : "^
  (Odoc_print.string_of_type_expr a.M.att_value.M.val_type)^"\n"^
  (match a.M.att_value.M.val_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

let string_of_method m =
  let module M = Odoc_value in
  "method "^
  (if m.M.met_private then Odoc_messages.privat^" " else "")^
  (Name.simple m.M.met_value.M.val_name)^" : "^
  (Odoc_print.string_of_type_expr m.M.met_value.M.val_type)^"\n"^
  (match m.M.met_value.M.val_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)
