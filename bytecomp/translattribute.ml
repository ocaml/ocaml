(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Typedtree
open Lambda
open Location

let is_inline_attribute = function
  | {txt=("inline"|"ocaml.inline")}, _ -> true
  | _ -> false

let is_inlined_attribute = function
  | {txt=("inlined"|"ocaml.inlined")}, _ -> true
  | _ -> false

(* the 'inline' and 'inlined' attributes can be used as
   [@inline], [@inline never] or [@inline always].
   [@inline] is equivalent to [@inline always] *)

let make_get_inline_attribute is_attribute attributes =
  let warning txt = Warnings.Attribute_payload
      (txt, "It must be either empty, 'always' or 'never'")
  in
  let inline_attribute, exp_attributes =
    List.partition is_attribute attributes
  in
  let attribute_value =
    match inline_attribute with
    | [] -> Default_inline
    | [({txt;loc}, payload)] -> begin
        let open Parsetree in
        match payload with
        | PStr [] -> Always_inline
        | PStr [{pstr_desc = Pstr_eval ({pexp_desc},[])}] -> begin
            match pexp_desc with
            | Pexp_ident { txt = Longident.Lident "never" } ->
                Never_inline
            | Pexp_ident { txt = Longident.Lident "always" } ->
                Always_inline
            | _ ->
                Location.prerr_warning loc (warning txt);
                Default_inline
          end
        | _ ->
            Location.prerr_warning loc (warning txt);
            Default_inline
      end
    | _ :: ({txt;loc}, _) :: _ ->
        Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
        Default_inline
  in
  attribute_value, exp_attributes

let get_inline_attribute l =
  fst (make_get_inline_attribute is_inline_attribute l)

let add_inline_attribute expr loc attributes =
  match expr, get_inline_attribute attributes with
  | expr, Default_inline -> expr
  | Lfunction({ attr } as funct), inline_attribute ->
      begin match attr.inline with
      | Default_inline -> ()
      | Always_inline | Never_inline ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "inline")
      end;
      Lfunction { funct with attr = { inline = inline_attribute } }
  | expr, (Always_inline | Never_inline) ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute "inline");
      expr

(* Get the [@inlined] attibute payload (or default if not present).
   It also returns the expression without this attribute. This is
   used to ensure that this expression is not misplaced: If it
   appears on any expression, it is an error, otherwise it would
   have been removed by this function *)
let get_inlined_attribute e =
  let attribute_value, exp_attributes =
    make_get_inline_attribute is_inlined_attribute e.exp_attributes
  in
  attribute_value, { e with exp_attributes }

(* It also remove the attribute from the expression, like
   get_inlined_attribute *)
let get_tailcall_attribute e =
  let is_tailcall_attribute = function
    | {txt=("tailcall"|"ocaml.tailcall")}, _ -> true
    | _ -> false
  in
  let tailcalls, exp_attributes =
    List.partition is_tailcall_attribute e.exp_attributes
  in
  match tailcalls with
  | [] -> false, e
  | _ :: r ->
      begin match r with
      | [] -> ()
      | ({txt;loc}, _) :: _ ->
          Location.prerr_warning loc (Warnings.Duplicated_attribute txt)
      end;
      true, { e with exp_attributes }

let check_attribute e ({ txt; loc }, _) =
  match txt with
  | "inline" | "ocaml.inline" ->  begin
      match e.exp_desc with
      | Texp_function _ -> ()
      | _ ->
          Location.prerr_warning loc
            (Warnings.Misplaced_attribute txt)
    end
  | "inlined" | "ocaml.inlined"
  | "tailcall" | "ocaml.tailcall" ->
      (* Removed by the Texp_apply cases *)
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute txt)
  | _ -> ()
