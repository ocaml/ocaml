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

module Short_name = struct

  type 'a item = {
    item: 'a;
    name : string;
    from: Ident.t option;
  }

  type 'a t =
    | Original of 'a
    | Synthetic of string * 'a

  type functor_param =
    | Unit
    | Named of (Ident.t option * Types.module_type t)

  let modtype (r : _ item) = match r.item with
    | Types.Mty_ident _
    | Types.Mty_alias _
    | Types.Mty_signature []
      -> Original r.item
    | Types.Mty_signature _ | Types.Mty_functor _
      -> Synthetic (r.name, r.item)

  let functor_param (ua : _ item) = match ua.item with
    | Types.Unit -> Unit
    | Types.Named (from, mty) ->
        Named (from, modtype { ua with item = mty ; from })

  let modexpr (r : _ item) = match r.item.Parsetree.pmod_desc with
    | Pmod_ident _
    | Pmod_structure []
      -> Original r.item
    | _
      -> Synthetic (r.name, r.item)

  let pp ppx = function
    | Original x -> ppx x
    | Synthetic (s,_) -> Format.dprintf "%s" s

  let pp_orig ppx = function
    | Original x | Synthetic (_, x) -> ppx x

end

module Pp = struct

  let buffer = ref Bytes.empty
  let is_big obj =
    let size = !Clflags.error_size in
    size > 0 &&
    begin
      if Bytes.length !buffer < size then buffer := Bytes.create size;
      try ignore (Marshal.to_buffer !buffer 0 size obj []); false
      with _ -> true
    end

  let show_loc msg ppf loc =
    let pos = loc.Location.loc_start in
    if List.mem pos.Lexing.pos_fname [""; "_none_"; "//toplevel//"] then ()
    else Format.fprintf ppf "@\n@[<2>%a:@ %s@]" Location.print_loc loc msg

  let show_locs ppf (loc1, loc2) =
    show_loc "Expected declaration" ppf loc2;
    show_loc "Actual declaration" ppf loc1

  let core id x =
    match x with
    | Value_descriptions diff ->
        let t1 = Printtyp.tree_of_value_description id diff.got in
        let t2 = Printtyp.tree_of_value_description id diff.expected in
        Format.dprintf
          "@[<hv 2>Values do not match:@ %a@;<1 -2>is not included in@ %a@]%a%t"
          !Oprint.out_sig_item t1
          !Oprint.out_sig_item t2
        show_locs (diff.got.val_loc, diff.expected.val_loc)
        Printtyp.Conflicts.print_explanations
    | Type_declarations diff ->
        Format.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a%a%t@]"
          "Type declarations do not match"
          !Oprint.out_sig_item
          (Printtyp.tree_of_type_declaration id diff.got Trec_first)
          "is not included in"
          !Oprint.out_sig_item
          (Printtyp.tree_of_type_declaration id diff.expected Trec_first)
          (Includecore.report_type_mismatch
             "the first" "the second" "declaration") diff.symptom
          show_locs (diff.got.type_loc, diff.expected.type_loc)
          Printtyp.Conflicts.print_explanations
    | Extension_constructors diff ->
        Format.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]@ %a%a%t@]"
          "Extension declarations do not match"
          !Oprint.out_sig_item
          (Printtyp.tree_of_extension_constructor id diff.got Text_first)
          "is not included in"
          !Oprint.out_sig_item
          (Printtyp.tree_of_extension_constructor id diff.expected Text_first)
          (Includecore.report_extension_constructor_mismatch
             "the first" "the second" "declaration") diff.symptom
          show_locs (diff.got.ext_loc, diff.expected.ext_loc)
          Printtyp.Conflicts.print_explanations
    | Class_type_declarations diff ->
        Format.dprintf
          "@[<hv 2>Class type declarations do not match:@ \
           %a@;<1 -2>does not match@ %a@]@ %a%t"
          !Oprint.out_sig_item
          (Printtyp.tree_of_cltype_declaration id diff.got Trec_first)
          !Oprint.out_sig_item
          (Printtyp.tree_of_cltype_declaration id diff.expected Trec_first)
          Includeclass.report_error diff.symptom
          Printtyp.Conflicts.print_explanations
    | Class_declarations {got;expected;symptom} ->
        let t1 = Printtyp.tree_of_class_declaration id got Trec_first in
        let t2 = Printtyp.tree_of_class_declaration id expected Trec_first in
        Format.dprintf
          "@[<hv 2>Class declarations do not match:@ \
           %a@;<1 -2>does not match@ %a@]@ %a%t"
          !Oprint.out_sig_item t1
          !Oprint.out_sig_item t2
          Includeclass.report_error symptom
          Printtyp.Conflicts.print_explanations

  let missing_field ppf item =
    let id, loc, kind = Commonmod.item_ident_name item in
    Format.fprintf ppf "The %s `%a' is required but not provided%a"
      ( Commonmod.kind_of_field_desc kind) Printtyp.ident id
    (show_loc "Expected declaration") loc

  let module_types {got=mty1; expected=mty2} =
    Format.dprintf
      "@[<hv 2>Modules do not match:@ \
       %a@;<1 -2>is not included in@ %a@]"
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty1)
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty2)

  let eq_module_types {got=mty1; expected=mty2} =
    Format.dprintf
      "@[<hv 2>Module types do not match:@ \
       %a@;<1 -2>is not equal to@ %a@]"
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty1)
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty2)


  let module_type_declarations id {got=d1 ; expected=d2} =
    Format.dprintf
      "@[<hv 2>Module type declarations do not match:@ \
       %a@;<1 -2>does not match@ %a@]"
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d1)
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d2)

  let interface_mismatch ppf diff =
    Format.fprintf ppf
      "The implementation %s@ does not match the interface %s:@ "
      diff.got diff.expected

  let core_module_type_symptom x  =
    match x with
    | Not_an_alias | Not_an_identifier | Abstract_module_type
    | Incompatible_aliases ->
        if Printtyp.Conflicts.exists () then
          Some Printtyp.Conflicts.print_explanations
        else None
    | Unbound_module_path path ->
        Some(Format.dprintf "Unbound module %a" Printtyp.path path)

  let dmodtype mty =
    let tmty = Printtyp.tree_of_modtype mty in
    Format.dprintf "%a" !Oprint.out_module_type tmty

  let definition_of_functor_param x = match Short_name.functor_param x with
    | Short_name.Unit -> Format.dprintf "()"
    | Short_name.Named(_,short_mty) ->
        match short_mty with
        | Original mty -> dmodtype mty
        | Synthetic (name, mty) ->
            Format.dprintf
              "%s@ =@ %t" name (dmodtype mty)

  let short_functor_param x = match Short_name.functor_param x with
    | Short_name.Unit -> Format.dprintf "()"
    | Short_name.Named (_, short_mty) ->
        Short_name.pp dmodtype short_mty

  let functor_param x = match Short_name.functor_param x with
    | Short_name.Unit -> Format.dprintf "()"
    | Short_name.Named (None, Original (Mty_signature []) ) ->
        Format.dprintf "(sig end)"
    | Short_name.Named (None, short_mty) ->
        Short_name.pp dmodtype short_mty
    | Short_name.Named (Some p, short_mty) ->
        Format.dprintf "(%s : %t)"
          (Ident.name p) (Short_name.pp dmodtype short_mty)

  let definition_of_argument ua =
    let arg, mty = ua.Short_name.item in
    match arg with
    | Unit_arg -> Format.dprintf "()"
    | Named_arg p ->
        let mty = Short_name.modtype { ua with item = mty } in
        Format.dprintf
          "%a@ :@ %t"
          Printtyp.path p
          (Short_name.pp_orig dmodtype mty)
    | Anonymous md ->
        let short_md = Short_name.modexpr { ua with item = md } in
        begin match short_md with
        | Original md -> fun ppf -> Pprintast.module_expr ppf md
        | Synthetic (name, md) -> fun ppf ->
            Format.fprintf ppf
              "%s@ =@ %a" name Pprintast.module_expr md
        end

  let short_argument ua =
    let arg, _mty = ua.Short_name.item in
    match arg with
    | Unit_arg -> Format.dprintf "()"
    | Named_arg p -> fun ppf -> Printtyp.path ppf p
    | Anonymous md ->
        let short_md = Short_name.modexpr { ua with item=md } in
        Short_name.pp (fun x ppf -> Pprintast.module_expr ppf x) short_md

  let style = function
    | Diffing.Keep _ -> Misc.Color.[ FG Green ]
    | Diffing.Delete _ -> Misc.Color.[ FG Red; Bold]
    | Diffing.Insert _ -> Misc.Color.[ FG Red; Bold]
    | Diffing.Change _ -> Misc.Color.[ FG Magenta; Bold]

  let prefix ppf (pos, p) =
    let sty = style p in
    Format.pp_open_stag ppf (Misc.Color.Style sty);
    Format.fprintf ppf "%i." pos;
    Format.pp_close_stag ppf ()

  let param_id x = match x.Short_name.item with
    | Types.Named (Some _ as x,_) -> x
    | Types.(Unit | Named(None,_)) -> None

  let got_arg = function
    | Diffing.Delete mty
    | Diffing.Keep (mty,_,_)
    | Diffing.Change (mty,_,_) as x ->
        Some (None,(x,mty))
    | Diffing.Insert _ -> None

 let got_app = function
    | Diffing.Delete mty
    | Diffing.Keep (mty,_,_)
    | Diffing.Change (mty,_,_) as x ->
        Some (param_id mty,(x,mty))
    | Diffing.Insert _ -> None

  let expected = function
    | Diffing.Insert mty
    | Diffing.Keep(_,mty,_)
    | Diffing.Change (_,mty,_) as x ->
        Some (param_id mty,(x, mty))
    | Diffing.Delete _ -> None

  let space ppf () = Format.fprintf ppf "@ "
  let params_diff sep proj printer patch =
    let elt (x,param) =
      let sty = style x in
      Format.dprintf "%a%t%a"
        Format.pp_open_stag (Misc.Color.Style sty)
        (printer param)
        Format.pp_close_stag ()
    in
    let params = List.filter_map proj @@ List.map snd patch in
    Printtyp.functor_parameters ~sep elt params

end
