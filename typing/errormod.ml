(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


module Context = struct
  type pos =
    | Module of Ident.t
    | Modtype of Ident.t
    | Arg of Types.functor_parameter
    | Body of Types.functor_parameter

  let path_of_context = function
      Module id :: rem ->
        let rec subm path = function
          | [] -> path
          | Module id :: rem -> subm (Path.Pdot (path, Ident.name id)) rem
          | _ -> assert false
        in subm (Path.Pident id) rem
    | _ -> assert false


  let rec context ppf = function
      Module id :: rem ->
        Format.fprintf ppf "@[<2>module %a%a@]" Printtyp.ident id args rem
    | Modtype id :: rem ->
        Format.fprintf ppf "@[<2>module type %a =@ %a@]"
          Printtyp.ident id context_mty rem
    | Body x :: rem ->
        Format.fprintf ppf "functor (%s) ->@ %a" (argname x) context_mty rem
    | Arg x :: rem ->
        Format.fprintf ppf "functor (%s : %a) -> ..."
          (argname x) context_mty rem
    | [] ->
        Format.fprintf ppf "<here>"
  and context_mty ppf = function
      (Module _ | Modtype _) :: _ as rem ->
        Format.fprintf ppf "@[<2>sig@ %a@;<1 -2>end@]" context rem
    | cxt -> context ppf cxt
  and args ppf = function
      Body x :: rem ->
        Format.fprintf ppf "(%s)%a" (argname x) args rem
    | Arg x :: rem ->
        Format.fprintf ppf "(%s :@ %a) : ..." (argname  x) context_mty rem
    | cxt ->
        Format.fprintf ppf " :@ %a" context_mty cxt
  and argname = function
    | Types.Unit -> ""
    | Types.Named (None, _) -> "_"
    | Types.Named (Some id, _) -> Ident.name id

  let alt_pp ppf cxt =
    if cxt = [] then () else
    if List.for_all (function Module _ -> true | _ -> false) cxt then
      Format.fprintf ppf "in module %a," Printtyp.path (path_of_context cxt)
    else
      Format.fprintf ppf "@[<hv 2>at position@ %a,@]" context cxt

  let pp ppf cxt =
    if cxt = [] then () else
    if List.for_all (function Module _ -> true | _ -> false) cxt then
      Format.fprintf ppf "In module %a:@ " Printtyp.path (path_of_context cxt)
    else
      Format.fprintf ppf "@[<hv 2>At position@ %a@]@ " context cxt
end

type ('a,'b) diff = {got:'a; expected:'a; symptom:'b}
type 'a core_diff =('a,unit) diff
let diff x y s = {got=x;expected=y; symptom=s}
let sdiff x y = {got=x; expected=y; symptom=()}

type functor_arg_descr =
  | Anonymous of Parsetree.module_expr
  | Named_arg of Path.t
  | Unit_arg


type core_sigitem_symptom =
  | Value_descriptions of Types.value_description core_diff
  | Type_declarations of
      (Types.type_declaration, Includecore.type_mismatch) diff
  | Extension_constructors of
      (Types.extension_constructor,
       Includecore.extension_constructor_mismatch) diff
  | Class_type_declarations of
      (Types.class_type_declaration, Ctype.class_match_failure list) diff
  | Class_declarations of
      (Types.class_declaration, Ctype.class_match_failure list) diff

type core_module_type_symptom =
  | Not_an_alias
  | Not_an_identifier
  | Incompatible_aliases
  | Abstract_module_type
  | Unbound_module_path of Path.t

type module_type_symptom =
  | Mt_core of core_module_type_symptom
  | Signature of signature_symptom
  | Functor of functor_symptom
  | Invalid_module_alias of Path.t
  | After_alias_expansion of module_type_diff


and module_type_diff = (Types.module_type, module_type_symptom) diff

and functor_symptom =
  | Params of functor_params_diff
  | Result of module_type_diff

and ('arg,'path) functor_param_symptom =
  | Incompatible_params of 'arg * Types.functor_parameter
  | Mismatch of module_type_diff

and arg_functor_param_symptom =
  (Types.functor_parameter, Ident.t) functor_param_symptom

and functor_params_diff =
  (Types.functor_parameter list * Types.module_type) core_diff

and signature_symptom = {
  env: Env.t;
  missings: Types.signature_item list;
  incompatibles: (Ident.t * sigitem_symptom) list;
  oks: (int * Typedtree.module_coercion) list;
}
and sigitem_symptom =
  | Core of core_sigitem_symptom
  | Module_type_declaration of
      (Types.modtype_declaration, module_type_declaration_symptom) diff
  | Module_type of module_type_diff

and module_type_declaration_symptom =
  | Illegal_permutation of Typedtree.module_coercion
  | Not_greater_than of module_type_diff
  | Not_less_than of module_type_diff
  | Incomparable of
      {less_than:module_type_diff; greater_than: module_type_diff}


type all =
  | In_Compilation_unit of (string, signature_symptom) diff
  | In_Signature of signature_symptom
  | In_Module_type of module_type_diff
  | In_Module_type_substitution of
      Ident.t * (Types.module_type,module_type_declaration_symptom) diff
  | In_Type_declaration of Ident.t * core_sigitem_symptom
  | In_Expansion of core_module_type_symptom


module Illegal_permutation = struct
  (** Extraction of information in case of illegal permutation
      in a module type *)

  (** When examining coercions, we only have runtime component indices,
      we use thus a limited version of {!pos}. *)
  type coerce_pos =
    | Item of int
    | InArg
    | InBody

  let either f x g y = match f x with
    | None -> g y
    | Some _ as v -> v

  (** We extract a lone transposition from a full tree of permutations. *)
  let rec transposition_under path = function
    | Typedtree.Tcoerce_structure(c,_) ->
        either
          (not_fixpoint path 0) c
          (first_non_id path 0) c
    | Typedtree.Tcoerce_functor(arg,res) ->
        either
          (transposition_under (InArg::path)) arg
          (transposition_under (InBody::path)) res
    | Typedtree.Tcoerce_none -> None
    | Typedtree.Tcoerce_alias _ | Typedtree.Tcoerce_primitive _ ->
        (* these coercions are not inversible, and raise an error earlier when
           checking for module type equivalence *)
        assert false
  (* we search the first point which is not invariant at the current level *)
  and not_fixpoint path pos = function
    | [] -> None
    | (n, _) :: q ->
        if n = pos then
          not_fixpoint path (pos+1) q
        else
          Some(List.rev path, pos, n)
  (* we search the first item with a non-identity inner coercion *)
  and first_non_id path pos = function
    | [] -> None
    | (_,Typedtree.Tcoerce_none) :: q -> first_non_id path (pos + 1) q
    | (_,c) :: q ->
        either
          (transposition_under (Item pos :: path)) c
          (first_non_id path (pos + 1)) q

  let transposition c =
    match transposition_under [] c with
    | None -> raise Not_found
    | Some x -> x

  let rec runtime_item k = function
    | [] -> raise Not_found
    | item :: q ->
        if not(Commonmod.is_runtime_component item) then
          runtime_item k q
        else if k = 0 then
          item
        else
          runtime_item (k-1) q

  (* Find module type at position [path] and convert the [coerce_pos] path to
     a [pos] path *)
  let rec find env ctx path mt = match mt, path with
    | Types.(Mty_ident p | Mty_alias p), _ ->
        begin match (Env.find_modtype p env).mtd_type with
        | None -> raise Not_found
        | Some mt -> find env ctx path mt
        end
    | Types.Mty_signature s , [] -> List.rev ctx, s
    | Types.Mty_signature s, Item k :: q ->
        begin match runtime_item k s with
        | Sig_module (id, _, md,_,_) ->
            find env (Context.Module id :: ctx) q md.md_type
        | _ -> raise Not_found
        end
    | Types.Mty_functor(Named (_,mt) as arg,_), InArg :: q ->
        find env (Context.Arg arg :: ctx) q mt
    | Types.Mty_functor(arg, mt), InBody :: q ->
        find env (Context.Body arg :: ctx) q mt
    | _ -> raise Not_found

  let find env path mt = find env [] path mt
  let item mt k = Commonmod.item_ident_name (runtime_item k mt)

  let pp_item ppf (id,_,kind) =
    Format.fprintf ppf "%s %S"
      (Commonmod.kind_of_field_desc kind) (Ident.name id)

  let pp ctx_printer env ppf (mty,c) =
    try
      let p, k, l = transposition c in
      let ctx, mt = find env p mty in
      Format.fprintf ppf
        "@[<hv 2>Illegal permutation of runtime components in a module type.@ \
         @[For example,@ %a@]@ @[the %a@ and the %a are not in the same order@ \
         in the expected and actual module types.@]@]"
        ctx_printer ctx pp_item (item mt k) pp_item (item mt l)
    with Not_found -> (* this should not happen *)
      Format.fprintf ppf
        "Illegal permutation of runtime components in a module type."

end
