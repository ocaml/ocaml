(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree
open Lambda
open Location

let is_inline_attribute = function
  | {txt=("inline"|"ocaml.inline")} -> true
  | _ -> false

let is_inlined_attribute = function
  | {txt=("inlined"|"ocaml.inlined")} -> true
  | {txt=("unrolled"|"ocaml.unrolled")} when Config.flambda -> true
  | _ -> false

let is_specialise_attribute = function
  | {txt=("specialise"|"ocaml.specialise")} when Config.flambda -> true
  | _ -> false

let is_specialised_attribute = function
  | {txt=("specialised"|"ocaml.specialised")} when Config.flambda -> true
  | _ -> false

let is_local_attribute = function
  | {txt=("local"|"ocaml.local")} -> true
  | _ -> false

let is_tmc_attribute = function
  | {txt=("tail_mod_cons"|"ocaml.tail_mod_cons")} -> true
  | _ -> false

let is_poll_attribute = function
  | {txt=("poll")} -> true
  | _ -> false

let find_attribute p attributes =
  let inline_attribute, other_attributes =
    List.partition (fun a -> p a.Parsetree.attr_name) attributes
  in
  let attr =
    match inline_attribute with
    | [] -> None
    | [attr] -> Some attr
    | _ :: {Parsetree.attr_name = {txt;loc}; _} :: _ ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
      None
  in
  attr, other_attributes

let is_unrolled = function
  | {txt="unrolled"|"ocaml.unrolled"} -> true
  | {txt="inline"|"ocaml.inline"|"inlined"|"ocaml.inlined"} -> false
  | _ -> assert false

let get_payload get_from_exp =
  let open Parsetree in
  function
  | PStr [{pstr_desc = Pstr_eval (exp, [])}] -> get_from_exp exp
  | _ -> Result.Error ()

let get_optional_payload get_from_exp =
  let open Parsetree in
  function
  | PStr [] -> Result.Ok None
  | other -> Result.map Option.some (get_payload get_from_exp other)

let get_id_from_exp =
  let open Parsetree in
  function
  | { pexp_desc = Pexp_ident { txt = Longident.Lident id } } -> Result.Ok id
  | _ -> Result.Error ()

let get_int_from_exp =
  let open Parsetree in
  function
    | { pexp_desc = Pexp_constant (Pconst_integer(s, None)) } ->
        begin match Misc.Int_literal_converter.int s with
        | n -> Result.Ok n
        | exception (Failure _) -> Result.Error ()
        end
    | _ -> Result.Error ()

let get_construct_from_exp =
  let open Parsetree in
  function
    | { pexp_desc =
          Pexp_construct ({ txt = Longident.Lident constr }, None) } ->
        Result.Ok constr
    | _ -> Result.Error ()

let get_bool_from_exp exp =
  Result.bind (get_construct_from_exp exp)
    (function
      | "true" -> Result.Ok true
      | "false" -> Result.Ok false
      | _ -> Result.Error ())

let parse_id_payload txt loc ~default ~empty cases payload =
  let[@local] warn () =
    let ( %> ) f g x = g (f x) in
    let msg =
      cases
      |> List.map (fst %> Printf.sprintf "'%s'")
      |> String.concat ", "
      |> Printf.sprintf "It must be either %s or empty"
    in
    Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
    default
  in
  match get_optional_payload get_id_from_exp payload with
  | Error () -> warn ()
  | Ok None -> empty
  | Ok (Some id) ->
      match List.assoc_opt id cases with
      | Some r -> r
      | None -> warn ()

let parse_inline_attribute attr =
  match attr with
  | None -> Default_inline
  | Some {Parsetree.attr_name = {txt;loc} as id; attr_payload = payload} ->
    if is_unrolled id then begin
      (* the 'unrolled' attributes must be used as [@unrolled n]. *)
      let warning txt = Warnings.Attribute_payload
          (txt, "It must be an integer literal")
      in
      match get_payload get_int_from_exp payload with
      | Ok n -> Unroll n
      | Error () ->
        Location.prerr_warning loc (warning txt);
        Default_inline
    end else
      parse_id_payload txt loc
        ~default:Default_inline
        ~empty:Always_inline
        [
          "never", Never_inline;
          "always", Always_inline;
          "hint", Hint_inline;
        ]
        payload

let parse_specialise_attribute attr =
  match attr with
  | None -> Default_specialise
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_specialise
        ~empty:Always_specialise
        [
          "never", Never_specialise;
          "always", Always_specialise;
        ]
        payload

let parse_local_attribute attr =
  match attr with
  | None -> Default_local
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_local
        ~empty:Always_local
        [
          "never", Never_local;
          "always", Always_local;
          "maybe", Default_local;
        ]
        payload

let parse_poll_attribute attr =
  match attr with
  | None -> Default_poll
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_poll
        ~empty:Default_poll
        [
          "error", Error_poll;
        ]
        payload

let get_inline_attribute l =
  let attr, _ = find_attribute is_inline_attribute l in
  parse_inline_attribute attr

let get_specialise_attribute l =
  let attr, _ = find_attribute is_specialise_attribute l in
  parse_specialise_attribute attr

let get_local_attribute l =
  let attr, _ = find_attribute is_local_attribute l in
  parse_local_attribute attr

let get_poll_attribute l =
  let attr, _ = find_attribute is_poll_attribute l in
  parse_poll_attribute attr

let check_local_inline loc attr =
  match attr.local, attr.inline with
  | Always_local, (Always_inline | Hint_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Duplicated_attribute "local/inline")
  | _ ->
      ()

let check_poll_inline loc attr =
  match attr.poll, attr.inline with
  | Error_poll, (Always_inline | Hint_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
          "[@poll error] is incompatible with inlining")
  | _ ->
      ()

let check_poll_local loc attr =
  match attr.poll, attr.local with
  | Error_poll, Always_local ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
          "[@poll error] is incompatible with local function optimization")
  | _ ->
      ()

let lfunction_with_attr ~attr { kind; params; return; body; attr=_; loc } =
  lfunction ~kind ~params ~return ~body ~attr ~loc

let add_inline_attribute expr loc attributes =
  match expr, get_inline_attribute attributes with
  | expr, Default_inline -> expr
  | Lfunction({ attr = { stub = false } as attr } as funct), inline ->
      begin match attr.inline with
      | Default_inline -> ()
      | Always_inline | Hint_inline | Never_inline | Unroll _ ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "inline")
      end;
      let attr = { attr with inline } in
      check_local_inline loc attr;
      check_poll_inline loc attr;
      lfunction_with_attr ~attr funct
  | expr, (Always_inline | Hint_inline | Never_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute "inline");
      expr

let add_specialise_attribute expr loc attributes =
  match expr, get_specialise_attribute attributes with
  | expr, Default_specialise -> expr
  | Lfunction({ attr = { stub = false } as attr } as funct), specialise ->
      begin match attr.specialise with
      | Default_specialise -> ()
      | Always_specialise | Never_specialise ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "specialise")
      end;
      let attr = { attr with specialise } in
      lfunction_with_attr ~attr funct
  | expr, (Always_specialise | Never_specialise) ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute "specialise");
      expr

let add_local_attribute expr loc attributes =
  match expr, get_local_attribute attributes with
  | expr, Default_local -> expr
  | Lfunction({ attr = { stub = false } as attr } as funct), local ->
      begin match attr.local with
      | Default_local -> ()
      | Always_local | Never_local ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "local")
      end;
      let attr = { attr with local } in
      check_local_inline loc attr;
      check_poll_local loc attr;
      lfunction_with_attr ~attr funct
  | expr, (Always_local | Never_local) ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute "local");
      expr

let add_tmc_attribute expr loc attributes =
  let is_tmc_attribute a = is_tmc_attribute a.Parsetree.attr_name in
  if List.exists is_tmc_attribute attributes then
    match expr with
    | Lfunction funct ->
        if funct.attr.tmc_candidate then
            Location.prerr_warning loc
              (Warnings.Duplicated_attribute "tail_mod_cons");
        let attr = { funct.attr with tmc_candidate = true } in
        lfunction_with_attr ~attr funct
    | expr ->
        Location.prerr_warning loc
          (Warnings.Misplaced_attribute "tail_mod_cons");
        expr
  else
    expr

let add_poll_attribute expr loc attributes =
  match expr, get_poll_attribute attributes with
  | expr, Default_poll -> expr
  | Lfunction({ attr = { stub = false } as attr } as funct), poll ->
      begin match attr.poll with
      | Default_poll -> ()
      | Error_poll ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "error_poll")
      end;
      let attr = { attr with poll } in
      check_poll_inline loc attr;
      check_poll_local loc attr;
      let attr = { attr with inline = Never_inline; local = Never_local } in
      lfunction_with_attr ~attr funct
  | expr, Error_poll ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute "error_poll");
      expr

(* Get the [@inlined] attribute payload (or default if not present).
   It also returns the expression without this attribute. This is
   used to ensure that this attribute is not misplaced: If it
   appears on any expression, it is an error, otherwise it would
   have been removed by this function *)
let get_and_remove_inlined_attribute e =
  let attr, exp_attributes =
    find_attribute is_inlined_attribute e.exp_attributes
  in
  let inlined = parse_inline_attribute attr in
  inlined, { e with exp_attributes }

let get_and_remove_inlined_attribute_on_module e =
  let rec get_and_remove mod_expr =
    let attr, mod_attributes =
      find_attribute is_inlined_attribute mod_expr.mod_attributes
    in
    let attr = parse_inline_attribute attr in
    let attr, mod_desc =
      match mod_expr.Typedtree.mod_desc with
      | Tmod_constraint (me, mt, mtc, mc) ->
        let inner_attr, me = get_and_remove me in
        let attr =
          match attr with
          | Always_inline | Hint_inline | Never_inline | Unroll _ -> attr
          | Default_inline -> inner_attr
        in
        attr, Tmod_constraint (me, mt, mtc, mc)
      | md -> attr, md
    in
    attr, { mod_expr with mod_desc; mod_attributes }
  in
  get_and_remove e

let get_and_remove_specialised_attribute e =
  let attr, exp_attributes =
    find_attribute is_specialised_attribute e.exp_attributes
  in
  let specialised = parse_specialise_attribute attr in
  specialised, { e with exp_attributes }

(* It also removes the attribute from the expression, like
   get_inlined_attribute *)
let get_tailcall_attribute e =
  let is_tailcall_attribute = function
    | {Parsetree.attr_name = {txt=("tailcall"|"ocaml.tailcall")}; _} -> true
    | _ -> false
  in
  let tailcalls, other_attributes =
    List.partition is_tailcall_attribute e.exp_attributes
  in
  let tailcall_attribute = match tailcalls with
    | [] -> Default_tailcall
    | {Parsetree.attr_name = {txt; loc}; attr_payload = payload} :: r ->
        begin match r with
        | [] -> ()
        | {Parsetree.attr_name = {txt;loc}; _} :: _ ->
            Location.prerr_warning loc (Warnings.Duplicated_attribute txt)
        end;
        match get_optional_payload get_bool_from_exp payload with
        | Ok (None | Some true) -> Tailcall_expectation true
        | Ok (Some false) -> Tailcall_expectation false
        | Error () ->
            let msg = "Only an optional boolean literal is supported." in
            Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
            Default_tailcall
      in
      tailcall_attribute, { e with exp_attributes = other_attributes }

let check_attribute e {Parsetree.attr_name = { txt; loc }; _} =
  match txt with
  | "inline" | "ocaml.inline"
  | "specialise" | "ocaml.specialise"
  | "poll" -> begin
      match e.exp_desc with
      | Texp_function _ -> ()
      | _ ->
          Location.prerr_warning loc
            (Warnings.Misplaced_attribute txt)
    end
  | "inlined" | "ocaml.inlined"
  | "specialised" | "ocaml.specialised"
  | "tailcall" | "ocaml.tailcall" ->
      (* Removed by the Texp_apply cases *)
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute txt)
  | _ -> ()

let check_attribute_on_module e {Parsetree.attr_name = { txt; loc }; _} =
  match txt with
  | "inline" | "ocaml.inline" ->  begin
      match e.mod_desc with
      | Tmod_functor _ -> ()
      | _ ->
          Location.prerr_warning loc
            (Warnings.Misplaced_attribute txt)
    end
  | "inlined" | "ocaml.inlined" ->
      (* Removed by the Texp_apply cases *)
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute txt)
  | _ -> ()

let add_function_attributes lam loc attr =
  let lam =
    add_inline_attribute lam loc attr
  in
  let lam =
    add_specialise_attribute lam loc attr
  in
  let lam =
    add_local_attribute lam loc attr
  in
  let lam =
    add_tmc_attribute lam loc attr
  in
  let lam =
    (* last because poll overrides inline and local *)
    add_poll_attribute lam loc attr
  in
  lam
