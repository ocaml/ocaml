(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inclusion checks for the module language *)

open Misc
open Typedtree
open Types

type symptom =
    Missing_field of Ident.t * Location.t * string (* kind *)
  | Value_descriptions of Ident.t * value_description * value_description
                          * Includecore.value_mismatch
  | Type_declarations of Ident.t * type_declaration
        * type_declaration * Includecore.type_mismatch
  | Extension_constructors of Ident.t * extension_constructor
        * extension_constructor * Includecore.extension_constructor_mismatch
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation of Types.module_type * Typedtree.module_coercion
  | Interface_mismatch of string * string
  | Class_type_declarations of
      Ident.t * class_type_declaration * class_type_declaration *
      Ctype.class_match_failure list
  | Class_declarations of
      Ident.t * class_declaration * class_declaration *
      Ctype.class_match_failure list
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type pos =
  | Module of Ident.t
  | Modtype of Ident.t
  | Arg of functor_parameter
  | Body of functor_parameter


module Error = struct

  type functor_arg_descr =
    | Anonymous
    | Named of Path.t
    | Unit
    | Empty_struct
     (** For backward compatibility's sake, an empty struct can be implicitly
         converted to an unit module  *)

  type ('a,'b) diff = {got:'a; expected:'a; symptom:'b}
  type 'a core_diff =('a,unit) diff
  let diff x y s = {got=x;expected=y; symptom=s}
  let sdiff x y = {got=x; expected=y; symptom=()}

  type core_sigitem_symptom =
    | Value_descriptions of (value_description, Includecore.value_mismatch) diff
    | Type_declarations of (type_declaration, Includecore.type_mismatch) diff
    | Extension_constructors of
        (extension_constructor, Includecore.extension_constructor_mismatch) diff
    | Class_type_declarations of
        (class_type_declaration, Ctype.class_match_failure list) diff
    | Class_declarations of
        (class_declaration, Ctype.class_match_failure list) diff

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


  and module_type_diff = (module_type, module_type_symptom) diff

  and functor_symptom =
    | Params of functor_params_diff
    | Result of module_type_diff

  and ('arg,'path) functor_param_symptom =
    | Incompatible_params of 'arg * functor_parameter
    | Mismatch of module_type_diff

  and arg_functor_param_symptom =
    (functor_parameter, Ident.t) functor_param_symptom

  and functor_params_diff = (functor_parameter list * module_type) core_diff

  and signature_symptom = {
    env: Env.t;
    missings: signature_item list;
    incompatibles: (Ident.t * sigitem_symptom) list;
    oks: (int * module_coercion) list;
    leftovers: (signature_item * signature_item * int) list;
  }
  and sigitem_symptom =
    | Core of core_sigitem_symptom
    | Module_type_declaration of
        (modtype_declaration, module_type_declaration_symptom) diff
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

end

module Directionality = struct


  type mark =
  | Mark_both
  | Mark_positive
  | Mark_neither

  type pos =
    | Strictly_positive
      (** Strictly positive positions are notable for tools since they are the
          the case where we match a implementation definition with an interface
          declaration. Oherwise in the positive case we are matching
          declatations inside functor arguments at even level of nesting.*)
    | Positive
    | Negative


(**
   When checking inclusion, the [Directionality.t] type tracks the
   subtyping direction at the syntactic level.

   The [posivity] field is used in the [cmt_declaration_dependencies] to
   distinguish between directed and undirected edges, and to avoid recording
   matched declarations twice.

   The [mark_as_used] field describes if we should record only positive use,
   any use (because there is no clear implementation side), or none (because we
   are inside an auxiliary check function.)

   The [in_eq] field is [true] when we are checking both directions inside of
   module types which allows optimizing module type equality checks. The module
   subtyping relation [A <: B] checks that [A.T = B.T] when [A] and [B] define a
   module type [T]. The relation [A.T = B.T] is equivalent to [(A.T <: B.T) and
   (B.T <: A.T)], but checking both recursively would lead to an exponential
   slowdown (see #10598 and #10616). To avoid this issue, when [in_eq] is
   [true], we compute a coarser relation [A << B] which is the same as [A <: B]
   except that module types [T] are checked only for [A.T << B.T] and not the
   reverse. Thus, we can implement a cheap module type equality check [A.T =
   B.T] by computing [(A.T << B.T) and (B.T << A.T)], avoiding the exponential
   slowdown described above.
*)
  type t = {
      in_eq:bool;
      mark_as_used:mark;
      pos:pos;
    }

  let strictly_positive ~mark =
    let mark_as_used = if mark then Mark_positive else Mark_neither in
    { in_eq=false; pos=Strictly_positive; mark_as_used }

  let unknown ~mark =
    let mark_as_used = if mark then Mark_both else Mark_neither in
    { in_eq=false; pos=Positive; mark_as_used }

  let negate_pos = function
    | Positive | Strictly_positive -> Negative
    | Negative -> Positive

  let negate d = { d with pos = negate_pos d.pos }

  let at_most_positive = function
    | Strictly_positive -> Positive
    | Positive | Negative as non_strict -> non_strict

  let enter_eq d =
    {
      in_eq = true;
      pos = at_most_positive d.pos;
      mark_as_used = d.mark_as_used
    }

  let mark_as_used d = match d.mark_as_used with
    | Mark_neither -> false
    | Mark_both -> true
    | Mark_positive ->
       match d.pos with
       | Positive | Strictly_positive -> true
       | Negative -> false

end

module Core_inclusion = struct
  (* All functions "blah env x1 x2" check that x1 is included in x2,
     i.e. that x1 is the type of an implementation that fulfills the
     specification x2. If not, Error is raised with a backtrace of the error. *)

  (* Inclusion between value descriptions *)

  let value_descriptions ~loc env ~direction subst id vd1 vd2 =
    if Directionality.mark_as_used direction then
      Env.mark_value_used vd1.val_uid;
    let vd2 = Subst.value_description subst vd2 in
    try
      Ok (Includecore.value_descriptions ~loc env (Ident.name id) vd1 vd2)
    with Includecore.Dont_match err ->
      Error Error.(Core (Value_descriptions (diff vd1 vd2 err)))

  (* Inclusion between type declarations *)

  let type_declarations ~loc env ~direction subst id decl1 decl2 =
    let mark = Directionality.mark_as_used direction in
    if mark then
      Env.mark_type_used decl1.type_uid;
    let decl2 = Subst.type_declaration subst decl2 in
    match
      Includecore.type_declarations ~loc env ~mark
        (Ident.name id) decl1 (Path.Pident id) decl2
    with
    | None -> Ok Tcoerce_none
    | Some err ->
        Error Error.(Core(Type_declarations (diff decl1 decl2 err)))

  (* Inclusion between extension constructors *)

  let extension_constructors ~loc env ~direction subst id ext1 ext2 =
    let mark = Directionality.mark_as_used direction in
    let ext2 = Subst.extension_constructor subst ext2 in
    match Includecore.extension_constructors ~loc env ~mark id ext1 ext2 with
    | None -> Ok Tcoerce_none
    | Some err ->
        Error Error.(Core(Extension_constructors(diff ext1 ext2 err)))

  (* Inclusion between class declarations *)

  let class_type_declarations ~loc env ~direction:_ subst _id decl1 decl2 =
    let decl2 = Subst.cltype_declaration subst decl2 in
    match Includeclass.class_type_declarations ~loc env decl1 decl2 with
      []     -> Ok Tcoerce_none
    | reason ->
        Error Error.(Core(Class_type_declarations(diff decl1 decl2 reason)))

  let class_declarations ~loc:_ env ~direction:_ subst _id decl1 decl2 =
    let decl2 = Subst.class_declaration subst decl2 in
    match Includeclass.class_declarations env decl1 decl2 with
      []     -> Ok Tcoerce_none
    | reason ->
        Error Error.(Core(Class_declarations(diff decl1 decl2 reason)))
end

(* Expand a module type identifier when possible *)

let expand_modtype_path env path =
   match Env.find_modtype_expansion path env with
     | exception Not_found -> None
     | x -> Some x

let expand_module_alias ~strengthen env path =
  match
    if strengthen then Env.find_strengthened_module ~aliasable:true path env
    else (Env.find_module path env).md_type
  with
  | x -> Ok x
  | exception Not_found -> Error (Error.Unbound_module_path path)

(* Extract name, kind and ident from a signature item *)

type field_kind =
  | Field_value
  | Field_type
  | Field_exception
  | Field_typext
  | Field_module
  | Field_modtype
  | Field_class
  | Field_classtype



type field_desc = { name: string; kind: field_kind }

let kind_of_field_desc fd = match fd.kind with
  | Field_value -> "value"
  | Field_type -> "type"
  | Field_exception -> "exception"
  | Field_typext -> "extension constructor"
  | Field_module -> "module"
  | Field_modtype -> "module type"
  | Field_class -> "class"
  | Field_classtype -> "class type"

let field_desc kind id = { kind; name = Ident.name id }

(** Map indexed by both field types and names.
    This avoids name clashes between different sorts of fields
    such as values and types. *)
module FieldMap = Map.Make(struct
    type t = field_desc
    let compare = Stdlib.compare
  end)

let item_ident_name = function
    Sig_value(id, d, _) -> (id, d.val_loc, field_desc Field_value id)
  | Sig_type(id, d, _, _) -> (id, d.type_loc, field_desc Field_type  id )
  | Sig_typext(id, d, _, _) ->
      let kind =
        if Path.same d.ext_type_path Predef.path_exn
        then Field_exception
        else Field_typext
      in
      (id, d.ext_loc, field_desc kind id)
  | Sig_module(id, _, d, _, _) -> (id, d.md_loc, field_desc Field_module id)
  | Sig_modtype(id, d, _) -> (id, d.mtd_loc, field_desc Field_modtype id)
  | Sig_class(id, d, _, _) -> (id, d.cty_loc, field_desc Field_class id)
  | Sig_class_type(id, d, _, _) ->
      (id, d.clty_loc, field_desc Field_classtype id)

let is_runtime_component = function
  | Sig_value(_,{val_kind = Val_prim _}, _)
  | Sig_type(_,_,_,_)
  | Sig_module(_,Mp_absent,_,_,_)
  | Sig_modtype(_,_,_)
  | Sig_class_type(_,_,_,_) -> false
  | Sig_value(_,_,_)
  | Sig_typext(_,_,_,_)
  | Sig_module(_,Mp_present,_,_,_)
  | Sig_class(_,_,_,_) -> true

(* Print a coercion *)

let rec print_list pr ppf = function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; Format.fprintf ppf ";@ "; print_list pr ppf l
let print_list pr ppf l =
  Format.fprintf ppf "[@[%a@]]" (print_list pr) l

let rec print_coercion ppf c =
  let pr fmt = Format.fprintf ppf fmt in
  match c with
    Tcoerce_none -> pr "id"
  | Tcoerce_structure (fl, nl) ->
      pr "@[<2>struct@ %a@ %a@]"
        (print_list print_coercion2) fl
        (print_list print_coercion3) nl
  | Tcoerce_functor (inp, out) ->
      pr "@[<2>functor@ (%a)@ (%a)@]"
        print_coercion inp
        print_coercion out
  | Tcoerce_primitive {pc_desc; pc_env = _; pc_type}  ->
      pr "prim %s@ (%a)" pc_desc.Primitive.prim_name
        Rawprinttyp.type_expr pc_type
  | Tcoerce_alias (_, p, c) ->
      pr "@[<2>alias %a@ (%a)@]"
        Printtyp.path p
        print_coercion c
and print_coercion2 ppf (n, c) =
  Format.fprintf ppf "@[%d,@ %a@]" n print_coercion c
and print_coercion3 ppf (i, n, c) =
  Format.fprintf ppf "@[%s, %d,@ %a@]"
    (Ident.unique_name i) n print_coercion c

(* Simplify a structure coercion *)

let equal_module_paths env p1 subst p2 =
  Path.same p1 p2
  || Path.same (Env.normalize_module_path None env p1)
       (Env.normalize_module_path None env
          (Subst.module_path subst p2))

let equal_modtype_paths env p1 subst p2 =
  Path.same p1 p2
  || Path.same (Env.normalize_modtype_path env p1)
       (Env.normalize_modtype_path env
          (Subst.modtype_path subst p2))

let simplify_structure_coercion cc id_pos_list =
  let rec is_identity_coercion pos = function
  | [] ->
      true
  | (n, c) :: rem ->
      n = pos && c = Tcoerce_none && is_identity_coercion (pos + 1) rem in
  if is_identity_coercion 0 cc
  then Tcoerce_none
  else Tcoerce_structure (cc, id_pos_list)

let retrieve_functor_params env mty =
  let rec retrieve_functor_params before env =
    function
    | Mty_ident p as res ->
        begin match expand_modtype_path env p with
        | Some mty -> retrieve_functor_params before env mty
        | None -> List.rev before, res
        end
    | Mty_alias p as res ->
        begin match expand_module_alias ~strengthen:false env p with
        | Ok mty ->  retrieve_functor_params before env mty
        | Error _ -> List.rev before, res
        end
    | Mty_functor (p, res) -> retrieve_functor_params (p :: before) env res
    | Mty_signature _ as res -> List.rev before, res
  in
  retrieve_functor_params [] env mty

(* Inclusion between module types.
   Return the restriction that transforms a value of the smaller type
   into a value of the bigger type. *)

(* When computing a signature difference, we need to distinguish between
   recoverable errors at the value level and unrecoverable errors at the type
   level that require us to stop the computation of the difference due to
   incoherent types.
*)
type 'a recoverable_error = { error: 'a; recoverable:bool }
let mark_error_as_recoverable r =
  Result.map_error (fun error -> { error; recoverable=true}) r
let mark_error_as_unrecoverable r =
  Result.map_error (fun error -> { error; recoverable=false}) r


module Sign_diff = struct
  type t = {
    runtime_coercions: (int * Typedtree.module_coercion) list;
    shape_map: Shape.Map.t;
    deep_modifications:bool;
    errors: (Ident.t * Error.sigitem_symptom) list;
    leftovers: ((Types.signature_item as 'it) * 'it * int) list
  }

  let empty = {
    runtime_coercions = [];
    shape_map = Shape.Map.empty;
    deep_modifications = false;
    errors = [];
    leftovers = []
  }

  let merge x y =
    {
      runtime_coercions = x.runtime_coercions @ y.runtime_coercions;
      shape_map = y.shape_map;
      (* the shape map is threaded the map during the difference computation,
          the last shape map contains all previous elements. *)
      deep_modifications = x.deep_modifications || y.deep_modifications;
      errors = x.errors @ y.errors;
      leftovers = x.leftovers @ y.leftovers
    }
end

(** Core type system subtyping-like relation that we want to lift at the module
    level. We have two relations that we want to lift:

  - the normal subtyping relation [<:].
  - the coarse-grain consistency relation [C], which is defined by
   [d1 C d2] if there is an environment [E] such that [E |- d1 <: d2]. *)
type 'a core_incl =
  loc:Location.t -> Env.t -> direction:Directionality.t -> Subst.t -> Ident.t ->
  'a -> 'a -> (module_coercion, Error.sigitem_symptom) result

type core_relation = {
  value_descriptions: Types.value_description core_incl;
  type_declarations: Types.type_declaration core_incl;
  extension_constructors: Types.extension_constructor core_incl;
  class_declarations: Types.class_declaration core_incl;
  class_type_declarations: Types.class_type_declaration core_incl;
}


let rec modtypes ~core ~direction ~loc env subst mty1 mty2 shape =
  match try_modtypes ~core ~direction ~loc env subst mty1 mty2 shape with
  | Ok _ as ok -> ok
  | Error reason ->
    let mty2 = Subst.modtype Make_local subst mty2 in
    Error Error.(diff mty1 mty2 reason)

and try_modtypes ~core ~direction ~loc env subst mty1 mty2 orig_shape =
  match mty1, mty2 with
  | (Mty_alias p1, Mty_alias p2) ->
      if Env.is_functor_arg p2 env then
        Error (Error.Invalid_module_alias p2)
      else if not (equal_module_paths env p1 subst p2) then
          Error Error.(Mt_core Incompatible_aliases)
      else Ok (Tcoerce_none, orig_shape)
  | (Mty_alias p1, _) -> begin
      match
        Env.normalize_module_path (Some Location.none) env p1
      with
      | exception Env.Error (Env.Missing_module (_, _, path)) ->
          Error Error.(Mt_core(Unbound_module_path path))
      | p1 ->
          begin match expand_module_alias ~strengthen:false env p1 with
          | Error e -> Error (Error.Mt_core e)
          | Ok mty1 ->
              match strengthened_modtypes ~core ~direction ~loc ~aliasable:true
                      env subst mty1 p1 mty2 orig_shape
              with
              | Ok _ as x -> x
              | Error reason -> Error (Error.After_alias_expansion reason)
          end
    end
  | (Mty_ident p1, Mty_ident p2) ->
      let p1 = Env.normalize_modtype_path env p1 in
      let p2 = Env.normalize_modtype_path env (Subst.modtype_path subst p2) in
      if Path.same p1 p2 then Ok (Tcoerce_none, orig_shape)
      else
        begin match expand_modtype_path env p1, expand_modtype_path env p2 with
        | Some mty1, Some mty2 ->
            try_modtypes ~core ~direction ~loc env subst mty1 mty2 orig_shape
        | None, _  | _, None -> Error (Error.Mt_core Abstract_module_type)
        end
  | (Mty_ident p1, _) ->
      let p1 = Env.normalize_modtype_path env p1 in
      begin match expand_modtype_path env p1 with
      | Some p1 ->
          try_modtypes ~core ~direction ~loc env subst p1 mty2 orig_shape
      | None -> Error (Error.Mt_core Abstract_module_type)
      end
  | (_, Mty_ident p2) ->
      let p2 = Env.normalize_modtype_path env (Subst.modtype_path subst p2) in
      begin match expand_modtype_path env p2 with
      | Some p2 ->
          try_modtypes ~core ~direction ~loc env subst mty1 p2 orig_shape
      | None ->
          begin match mty1 with
          | Mty_functor _ ->
              let params1 = retrieve_functor_params env mty1 in
              let d = Error.sdiff params1 ([],mty2) in
              Error Error.(Functor (Params d))
          | _ -> Error Error.(Mt_core Not_an_identifier)
          end
      end
  | (Mty_signature sig1, Mty_signature sig2) ->
      begin match
        signatures ~core ~direction ~loc env subst sig1 sig2 orig_shape
      with
      | Ok _ as ok -> ok
      | Error e -> Error (Error.Signature e)
      end
  | Mty_functor (param1, res1), Mty_functor (param2, res2) ->
      let cc_arg, env, subst =
        let direction = Directionality.negate direction in
        functor_param ~core ~direction ~loc env
          subst param1 param2
      in
      let var, res_shape =
        match Shape.decompose_abs orig_shape with
        | Some (var, res_shape) -> var, res_shape
        | None ->
            (* Using a fresh variable with a placeholder uid here is fine: users
               will never try to jump to the definition of that variable. If
               they try to jump to the parameter from inside the functor, they
               will use the variable shape that is stored in the local
               environment. *)
            let var, shape_var =
              Shape.fresh_var Uid.internal_not_actually_unique
            in
            var, Shape.app orig_shape ~arg:shape_var
      in
      let cc_res =
        modtypes ~core ~direction ~loc env subst res1 res2 res_shape
      in
      begin match cc_arg, cc_res with
      | Ok Tcoerce_none, Ok (Tcoerce_none, final_res_shape) ->
          let final_shape =
            if final_res_shape == res_shape
            then orig_shape
            else Shape.abs var final_res_shape
          in
          Ok (Tcoerce_none, final_shape)
      | Ok cc_arg, Ok (cc_res, final_res_shape) ->
          let final_shape =
            if final_res_shape == res_shape
            then orig_shape
            else Shape.abs var final_res_shape
          in
          Ok (Tcoerce_functor(cc_arg, cc_res), final_shape)
      | _, Error {Error.symptom = Error.Functor Error.Params res; _} ->
          let got_params, got_res = res.got in
          let expected_params, expected_res = res.expected in
          let d = Error.sdiff
              (param1::got_params, got_res)
              (param2::expected_params, expected_res) in
          Error Error.(Functor (Params d))
      | Error _, _ ->
          let params1, res1 = retrieve_functor_params env res1 in
          let params2, res2 = retrieve_functor_params env res2 in
          let d = Error.sdiff (param1::params1, res1) (param2::params2, res2) in
          Error Error.(Functor (Params d))
      | Ok _, Error res ->
          Error Error.(Functor (Result res))
      end
  | Mty_functor _, _
  | _, Mty_functor _ ->
      let params1 = retrieve_functor_params env mty1 in
      let params2 = retrieve_functor_params env mty2 in
      let d = Error.sdiff params1 params2 in
      Error Error.(Functor (Params d))
  | _, Mty_alias _ ->
      Error (Error.Mt_core Error.Not_an_alias)

(* Functor parameters *)

and functor_param ~core ~direction ~loc env subst param1 param2 =
  match param1, param2 with
  | Unit, Unit ->
      Ok Tcoerce_none, env, subst
  | Named (name1, arg1), Named (name2, arg2) ->
      let arg2' = Subst.modtype Keep subst arg2 in
      let cc_arg =
        match
          modtypes ~core ~direction ~loc env Subst.identity arg2' arg1
                Shape.dummy_mod
        with
        | Ok (cc, _) -> Ok cc
        | Error err -> Error (Error.Mismatch err)
      in
      let env, subst = equate_one_functor_param subst env arg2' name1 name2 in
      cc_arg, env, subst
  | _, _ ->
      Error (Error.Incompatible_params (param1, param2)), env, subst

and equate_one_functor_param subst env arg2' name1 name2  =
  match name1, name2 with
  | Some id1, Some id2 ->
  (* two matching abstract parameters: we add one identifier to the
     environment and record the equality between the two identifiers
     in the substitution *)
      Env.add_module id1 Mp_present arg2' env,
      Subst.add_module id2 (Path.Pident id1) subst
  | None, Some id2 ->
      let id1 = Ident.rename id2 in
      Env.add_module id1 Mp_present arg2' env,
      Subst.add_module id2 (Path.Pident id1) subst
  | Some id1, None ->
      Env.add_module id1 Mp_present arg2' env, subst
  | None, None ->
      env, subst

and strengthened_modtypes ~core ~direction ~loc ~aliasable env
    subst mty1 path1 mty2 shape =
  match mty1, mty2 with
  | Mty_ident p1, Mty_ident p2 when equal_modtype_paths env p1 subst p2 ->
      Ok (Tcoerce_none, shape)
  | _, _ ->
      let mty1 = Mtype.strengthen ~aliasable env mty1 path1 in
      modtypes ~core ~direction ~loc env subst mty1 mty2 shape

and strengthened_module_decl ~core ~loc ~aliasable ~direction env
    subst md1 path1 md2 shape =
  match md1.md_type, md2.md_type with
  | Mty_ident p1, Mty_ident p2 when equal_modtype_paths env p1 subst p2 ->
      Ok (Tcoerce_none, shape)
  | _, _ ->
      let md1 = Mtype.strengthen_decl ~aliasable env md1 path1 in
      modtypes ~core ~direction ~loc env subst md1.md_type md2.md_type shape

(* Inclusion between signatures *)

and signatures ~core ~direction ~loc env subst sig1 sig2 mod_shape =
  (* Environment used to check inclusion of components *)
  let new_env =
    Env.add_signature sig1 (Env.in_signature true env) in
  (* Keep ids for module aliases *)
  let (id_pos_list,_) =
    List.fold_left
      (fun (l,pos) -> function
          Sig_module (id, Mp_present, _, _, _) ->
            ((id,pos,Tcoerce_none)::l , pos+1)
        | item -> (l, if is_runtime_component item then pos+1 else pos))
      ([], 0) sig1 in
  (* Build a table of the components of sig1, along with their positions.
     The table is indexed by kind and name of component *)
  let rec build_component_table nb_exported pos tbl = function
      [] -> nb_exported, pos, tbl
    | item :: rem ->
        let pos, nextpos =
          if is_runtime_component item then pos, pos + 1
          else -1, pos
        in
        match item_visibility item with
        | Hidden ->
            (* do not pair private items. *)
            build_component_table nb_exported nextpos tbl rem
        | Exported ->
            let (id, _loc, name) = item_ident_name item in
            build_component_table (nb_exported + 1) nextpos
              (FieldMap.add name (id, item, pos) tbl) rem
  in
  let exported_len1, runtime_len1, comps1 =
    build_component_table 0 0 FieldMap.empty sig1
  in
  let exported_len2, runtime_len2 =
    List.fold_left (fun (el, rl) i ->
      let el = match item_visibility i with Hidden -> el | Exported -> el + 1 in
      let rl = if is_runtime_component i then rl + 1 else rl in
      el, rl
    ) (0, 0) sig2
  in
  (* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. *)
  let rec pair_components ~core subst paired unpaired = function
      [] ->
        let open Sign_diff in
        let d =
          signature_components ~core ~direction ~loc env new_env subst
            mod_shape Shape.Map.empty
            (List.rev paired)
        in
        begin match unpaired, d.errors, d.runtime_coercions, d.leftovers with
            | [], [], cc, [] ->
                let shape =
                  if not d.deep_modifications && exported_len1 = exported_len2
                  then mod_shape
                  else Shape.str ?uid:mod_shape.Shape.uid d.shape_map
                in
                if runtime_len1 = runtime_len2 then (* see PR#5098 *)
                  Ok (simplify_structure_coercion cc id_pos_list, shape)
                else
                  Ok (Tcoerce_structure (cc, id_pos_list), shape)
            | missings, incompatibles, runtime_coercions, leftovers ->
                Error {
                  Error.env=new_env;
                  missings;
                  incompatibles;
                  oks=runtime_coercions;
                  leftovers;
                }
        end
    | item2 :: rem ->
        let (id2, _loc, name2) = item_ident_name item2 in
        let name2, report =
          match item2, name2 with
            Sig_type (_, {type_manifest=None}, _, _), {name=s; kind=Field_type}
            when Btype.is_row_name s ->
              (* Do not report in case of failure,
                 as the main type will generate an error *)
              { kind=Field_type; name=String.sub s 0 (String.length s - 4) },
              false
          | _ -> name2, true
        in
        begin match FieldMap.find name2 comps1 with
        | (id1, item1, pos1) ->
          let new_subst =
            match item2 with
              Sig_type _ ->
                Subst.add_type id2 (Path.Pident id1) subst
            | Sig_module _ ->
                Subst.add_module id2 (Path.Pident id1) subst
            | Sig_modtype _ ->
                Subst.add_modtype id2 (Path.Pident id1) subst
            | Sig_value _ | Sig_typext _
            | Sig_class _ | Sig_class_type _ ->
                subst
          in
          pair_components ~core new_subst
            ((item1, item2, pos1) :: paired) unpaired rem
        | exception Not_found ->
          let unpaired =
            if report then
              item2 :: unpaired
            else unpaired in
          pair_components ~core subst paired unpaired rem
        end in
  (* Do the pairing and checking, and return the final coercion *)
  pair_components ~core subst [] [] sig2

(* Inclusion between signature components *)

and signature_components ~core ~direction ~loc old_env env subst
    orig_shape shape_map paired =
  match paired with
  | [] -> Sign_diff.{ empty with shape_map }
  | (sigi1, sigi2, pos) :: rem ->
      let shape_modified = ref false in
      let id, item, paired_uids, shape_map, present_at_runtime =
        match sigi1, sigi2 with
        | Sig_value(id1, valdecl1, _) ,Sig_value(_id2, valdecl2, _) ->
            let item =
              core.value_descriptions ~loc ~direction env subst id1
                valdecl1 valdecl2
            in
            let item = mark_error_as_recoverable item in
            let present_at_runtime = match valdecl2.val_kind with
              | Val_prim _ -> false
              | _ -> true
            in
            let shape_map = Shape.Map.add_value_proj shape_map id1 orig_shape in
            let paired_uids = (valdecl1.val_uid, valdecl2.val_uid) in
            id1, item, paired_uids, shape_map, present_at_runtime
        | Sig_type(id1, tydec1, _, _), Sig_type(_id2, tydec2, _, _) ->
            let item =
              core.type_declarations ~loc ~direction env subst id1 tydec1 tydec2
            in
            let item = mark_error_as_unrecoverable item in
            (* Right now we don't filter hidden constructors / labels from the
            shape. *)
            let shape_map = Shape.Map.add_type_proj shape_map id1 orig_shape in
            id1, item, (tydec1.type_uid, tydec2.type_uid), shape_map, false
        | Sig_typext(id1, ext1, _, _), Sig_typext(_id2, ext2, _, _) ->
            let item =
              core.extension_constructors ~loc ~direction env subst id1
                ext1 ext2
            in
            let item = mark_error_as_unrecoverable item in
            let shape_map =
              Shape.Map.add_extcons_proj shape_map id1 orig_shape
            in
            id1, item, (ext1.ext_uid, ext2.ext_uid), shape_map, true
        | Sig_module(id1, pres1, mty1, _, _), Sig_module(_, pres2, mty2, _, _)
          -> begin
              let orig_shape =
                Shape.(proj orig_shape (Item.module_ id1))
              in
              let item =
                module_declarations ~core ~direction ~loc env subst id1
                  mty1 mty2 orig_shape
              in
              let item, shape_map =
                match item with
                | Ok (cc, shape) ->
                    if shape != orig_shape then shape_modified := true;
                    let mod_shape = Shape.set_uid_if_none shape mty1.md_uid in
                    Ok cc, Shape.Map.add_module shape_map id1 mod_shape
                | Error diff ->
                    Error (Error.Module_type diff),
                    (* We add the original shape to the map, even though
                       there is a type error.
                       It could still be useful for merlin. *)
                    Shape.Map.add_module shape_map id1 orig_shape
              in
              let present_at_runtime, item =
                match pres1, pres2, mty1.md_type with
                | Mp_present, Mp_present, _ -> true, item
                | _, Mp_absent, _ -> false, item
                | Mp_absent, Mp_present, Mty_alias p1 ->
                    true, Result.map (fun i -> Tcoerce_alias (env, p1, i)) item
                | Mp_absent, Mp_present, _ -> assert false
              in
              let item = mark_error_as_unrecoverable item in
              let paired_uids = (mty1.md_uid, mty2.md_uid) in
              id1, item, paired_uids, shape_map, present_at_runtime
            end
        | Sig_modtype(id1, info1, _), Sig_modtype(_id2, info2, _) ->
            let item =
              modtype_infos ~core ~direction ~loc env  subst id1 info1 info2
            in
            let shape_map =
              Shape.Map.add_module_type_proj shape_map id1 orig_shape
            in
            let item = mark_error_as_unrecoverable item in
            id1, item, (info1.mtd_uid, info2.mtd_uid), shape_map, false
        | Sig_class(id1, decl1, _, _), Sig_class(_id2, decl2, _, _) ->
            let item =
              core.class_declarations ~loc ~direction env subst id1 decl1 decl2
            in
            let shape_map =
              Shape.Map.add_class_proj shape_map id1 orig_shape
            in
            let item = mark_error_as_unrecoverable item in
            id1, item, (decl1.cty_uid, decl2.cty_uid), shape_map, true
        | Sig_class_type(id1, info1, _, _), Sig_class_type(_id2, info2, _, _) ->
            let item =
              core.class_type_declarations ~loc ~direction env subst id1
                info1 info2
            in
            let item = mark_error_as_unrecoverable item in
            let shape_map =
              Shape.Map.add_class_type_proj shape_map id1 orig_shape
            in
            id1, item, (info1.clty_uid, info2.clty_uid), shape_map, false
        | _ ->
            assert false
      in
      let deep_modifications = !shape_modified in
      let first =
        match item with
        | Ok x ->
            begin match direction with
            | { Directionality.in_eq = true; pos = Negative }
            | { Directionality.mark_as_used = Mark_neither; _ } ->
              (* We do not store paired uids when checking for reverse
                module-type inclusion as it would introduce duplicates. *)
                ()
            | { Directionality.pos; _} ->
              let paired_uids =
                let elt1, elt2 = paired_uids in
                match pos with
                | Negative ->
                    (Cmt_format.Declaration_to_declaration, elt2, elt1)
                | Positive ->
                    (Cmt_format.Declaration_to_declaration, elt1, elt2)
                | Strictly_positive ->
                    (Cmt_format. Definition_to_declaration, elt1, elt2)
              in
              Cmt_format.record_declaration_dependency paired_uids
            end;
            let runtime_coercions =
              if present_at_runtime then [pos,x] else []
            in
            Sign_diff.{ empty with deep_modifications; runtime_coercions }
        | Error { error; recoverable=_ } ->
            Sign_diff.{ empty with errors=[id,error]; deep_modifications }
      in
      let continue = match item with
        | Ok _ -> true
        | Error x -> x.recoverable
      in
      let rest =
        if continue then
          signature_components ~core ~direction ~loc old_env env subst
            orig_shape shape_map rem
        else Sign_diff.{ empty with leftovers=rem }
       in
       Sign_diff.merge first rest

and module_declarations ~direction ~loc env  subst id1 md1 md2 orig_shape =
  Builtin_attributes.check_alerts_inclusion
    ~def:md1.md_loc
    ~use:md2.md_loc
    loc
    md1.md_attributes md2.md_attributes
    (Ident.name id1);
  let p1 = Path.Pident id1 in
  if Directionality.mark_as_used direction then
    Env.mark_module_used md1.md_uid;
  strengthened_modtypes ~direction ~loc ~aliasable:true env subst
    md1.md_type p1 md2.md_type orig_shape

(* Inclusion between module type specifications *)

and modtype_infos ~core ~direction ~loc env subst id info1 info2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:info1.mtd_loc
    ~use:info2.mtd_loc
    loc
    info1.mtd_attributes info2.mtd_attributes
    (Ident.name id);
  let info2 = Subst.modtype_declaration Keep subst info2 in
  let r =
    match (info1.mtd_type, info2.mtd_type) with
      (None, None) -> Ok Tcoerce_none
    | (Some _, None) -> Ok Tcoerce_none
    | (Some mty1, Some mty2) ->
        check_modtype_equiv ~core ~direction ~loc env mty1 mty2
    | (None, Some mty2) ->
        let mty1 = Mty_ident(Path.Pident id) in
        check_modtype_equiv ~core ~direction ~loc env mty1 mty2 in
  match r with
  | Ok _ as ok -> ok
  | Error e -> Error Error.(Module_type_declaration (diff info1 info2 e))

and check_modtype_equiv ~core ~direction ~loc env mty1 mty2 =
  let nested_eq = direction.Directionality.in_eq in
  let direction = Directionality.enter_eq direction in
  let c1 =
    modtypes ~core ~direction ~loc env Subst.identity mty1 mty2 Shape.dummy_mod
  in
  let c2 =
    (* For nested module type paths, we check only one side of the equivalence:
       the outer module type is the one responsible for checking the other side
       of the equivalence.
     *)
    if nested_eq then None
    else
      let direction = Directionality.negate direction in
      Some (
        modtypes ~core ~direction ~loc env Subst.identity
          mty2 mty1 Shape.dummy_mod
      )
  in
  match c1, c2 with
  | Ok (Tcoerce_none, _), (Some Ok (Tcoerce_none, _)|None) -> Ok Tcoerce_none
  | Ok (c1, _), (Some Ok _ | None) ->
      (* Format.eprintf "@[c1 = %a@ c2 = %a@]@."
           print_coercion _c1 print_coercion _c2; *)
      Error Error.(Illegal_permutation c1)
  | Ok _, Some Error e -> Error Error.(Not_greater_than e)
  | Error e, (Some Ok _ | None) -> Error Error.(Not_less_than e)
  | Error less_than, Some Error greater_than ->
      Error Error.(Incomparable {less_than; greater_than})


(* Simplified inclusion check between module types (for Env) *)

let can_alias env path =
  let rec no_apply = function
    | Path.Pident _ -> true
    | Path.Pdot(p, _) | Path.Pextra_ty (p, _) -> no_apply p
    | Path.Papply _ -> false
  in
  no_apply path && not (Env.is_functor_arg path env)

let core_inclusion = Core_inclusion.{
  type_declarations;
  value_descriptions;
  extension_constructors;
  class_type_declarations;
  class_declarations;
}

let core_consistency =
  let type_declarations ~loc:_ env ~direction:_ _ _ d1 d2 =
    match Includecore.type_declarations_consistency env d1 d2 with
    | None -> Ok Tcoerce_none
    | Some err ->  Error Error.(Core(Type_declarations (diff d1 d2 err)))
  in
  let value_descriptions ~loc:_ env ~direction:_ _ _ vd1 vd2 =
    match Includecore.value_descriptions_consistency env vd1 vd2 with
    | x -> Ok x
    | exception Includecore.Dont_match err ->
        Error Error.(Core (Value_descriptions (diff vd1 vd2 err)))
  in
  let accept ~loc:_ _env ~direction:_ _subst _id _d1 _d2 = Ok Tcoerce_none in
  {
    type_declarations;
    value_descriptions;
    class_declarations=accept;
    class_type_declarations=accept;
    extension_constructors=accept;
  }

type explanation = Env.t * Error.all
exception Error of explanation

type application_name =
  | Anonymous_functor
  | Full_application_path of Longident.t
  | Named_leftmost_functor of Longident.t
exception Apply_error of {
    loc : Location.t ;
    env : Env.t ;
    app_name : application_name ;
    mty_f : module_type ;
    args : (Error.functor_arg_descr * module_type) list ;
  }

let check_modtype_inclusion_raw ~loc env mty1 path1 mty2 =
  let aliasable = can_alias env path1 in
  let direction = Directionality.unknown ~mark:true in
  strengthened_modtypes ~core:core_inclusion ~direction ~loc ~aliasable env
    Subst.identity mty1 path1 mty2 Shape.dummy_mod
  |> Result.map fst

let check_modtype_inclusion ~loc env mty1 path1 mty2 =
  match check_modtype_inclusion_raw ~loc env mty1 path1 mty2 with
  | Ok _ -> None
  | Error e -> Some (env, Error.In_Module_type e)

let check_functor_application_in_path
    ~errors ~loc ~lid_whole_app ~f0_path ~args
    ~arg_path ~arg_mty ~param_mty env =
  match check_modtype_inclusion_raw ~loc env arg_mty arg_path param_mty with
  | Ok _ -> ()
  | Error _errs ->
      if errors then
        let prepare_arg (arg_path, arg_mty) =
          let aliasable = can_alias env arg_path in
          let smd = Mtype.strengthen ~aliasable env arg_mty arg_path in
          (Error.Named arg_path, smd)
        in
        let mty_f = (Env.find_module f0_path env).md_type in
        let args = List.map prepare_arg args in
        let app_name = Full_application_path lid_whole_app in
        raise (Apply_error {loc; env; app_name; mty_f; args})
      else
        raise Not_found

let () =
  Env.check_functor_application := check_functor_application_in_path


(* Check that an implementation of a compilation unit meets its
   interface. *)

let compunit env ~mark impl_name impl_sig intf_name intf_sig unit_shape =
  let loc = Location.in_file impl_name in
  let direction = Directionality.strictly_positive ~mark in
  match
    signatures ~core:core_inclusion ~direction ~loc env Subst.identity
      impl_sig intf_sig unit_shape
  with Result.Error reasons ->
    let cdiff =
      Error.In_Compilation_unit(Error.diff impl_name intf_name reasons) in
    raise(Error(env, cdiff))
  | Ok x -> x

(* Functor diffing computation:
   The diffing computation uses the internal typing function
 *)

module Functor_inclusion_diff = struct

  module Defs = struct
    type left = Types.functor_parameter
    type right = left
    type eq = Typedtree.module_coercion
    type diff = (Types.functor_parameter, unit) Error.functor_param_symptom
    type state = {
      res: module_type option;
      env: Env.t;
      subst: Subst.t;
    }
  end
  open Defs

  module Diff = Diffing.Define(Defs)

  let param_name = function
      | Named(x,_) -> x
      | Unit -> None

  let weight: Diff.change -> _ = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep (param1, param2, _) -> begin
        match param_name param1, param_name param2 with
        | None, None
          -> 0
        | Some n1, Some n2
          when String.equal (Ident.name n1) (Ident.name n2)
          -> 0
        | Some _, Some _ -> 1
        | Some _,  None | None, Some _ -> 1
      end



  let keep_expansible_param = function
    | Mty_ident _ | Mty_alias _ as mty -> Some mty
    | Mty_signature _ | Mty_functor _ -> None

  let lookup_expansion { env ; res ; _ } = match res with
    | None -> None
    | Some res ->
        match retrieve_functor_params env res with
        | [], _ -> None
        | params, res ->
            let more = Array.of_list params  in
            Some (keep_expansible_param res, more)

  let expand_params state  =
    match lookup_expansion state with
    | None -> state, [||]
    | Some (res, expansion) -> { state with res }, expansion

  (* Whenever we have a named parameter that doesn't match it anonymous
     counterpart, we add it to the typing environment because it may
     contain useful abbreviations, but without adding any equations  *)
  let bind id arg state =
    let arg' = Subst.modtype Keep state.subst arg in
    let env = Env.add_module id Mp_present arg' state.env in
    { state with env }

  let rec update (d:Diff.change) st =
    match d with
    | Insert (Unit | Named (None,_))
    | Delete (Unit | Named (None,_))
    | Keep (Unit,_,_)
    | Keep (_,Unit,_) ->
        (* No named abstract parameters: we keep the same environment *)
        st, [||]
    | Insert (Named (Some id, arg)) | Delete (Named (Some id, arg)) ->
        (* one named parameter to bind *)
        st |> bind id arg |> expand_params
    | Change (delete, insert, _) ->
        (* Change should be delete + insert: we add both abstract parameters
           to the environment without equating them. *)
        let st, _expansion = update (Diffing.Delete delete) st in
        update (Diffing.Insert insert) st
    | Keep (Named (name1, _), Named (name2, arg2), _) ->
        let arg = Subst.modtype Keep st.subst arg2 in
        let env, subst =
          equate_one_functor_param st.subst st.env arg name1 name2
        in
        expand_params { st with env; subst }

  let diff env (l1,res1) (l2,_) =
    let module Compute = Diff.Left_variadic(struct
        let test st mty1 mty2 =
          let loc = Location.none in
          let res, _, _ =
            let direction=Directionality.unknown ~mark:false in
            functor_param ~core:core_inclusion ~direction ~loc st.env
              st.subst mty1 mty2
          in
          res
        let update = update
        let weight = weight
      end)
    in
    let param1 = Array.of_list l1 in
    let param2 = Array.of_list l2 in
    let state =
      { env; subst = Subst.identity; res = keep_expansible_param res1}
    in
    Compute.diff state param1 param2

end

module Functor_app_diff = struct
  module I = Functor_inclusion_diff
  module Defs= struct
    type left = Error.functor_arg_descr * Types.module_type
    type right = Types.functor_parameter
    type eq = Typedtree.module_coercion
    type diff = (Error.functor_arg_descr, unit) Error.functor_param_symptom
    type state = I.Defs.state
  end
  module Diff = Diffing.Define(Defs)

  let weight: Diff.change -> _ = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep (param1, param2, _) ->
        (* We assign a small penalty to named arguments with
           non-matching names *)
        begin
          let desc1 : Error.functor_arg_descr = fst param1 in
          match desc1, I.param_name param2 with
          | (Unit | Empty_struct | Anonymous) , None
            -> 0
          | Named (Path.Pident n1), Some n2
            when String.equal (Ident.name n1) (Ident.name n2)
            -> 0
          | Named _, Some _ -> 1
          | Named _,  None | (Unit | Empty_struct | Anonymous), Some _ -> 1
        end

  let update (d: Diff.change) (st:Defs.state) =
    let open Error in
    match d with
    | Insert (Unit|Named(None,_))
    | Delete _ (* delete is a concrete argument, not an abstract parameter*)
    | Keep ((Unit,_),_,_) (* Keep(Unit,_) implies Keep(Unit,Unit) *)
    | Keep (_,(Unit|Named(None,_)),_)
    | Change (_,(Unit|Named (None,_)), _ ) ->
        (* no abstract parameters to add, nor any equations *)
        st, [||]
    | Insert(Named(Some param, param_ty))
    | Change(_, Named(Some param, param_ty), _ ) ->
        (* Change is Delete + Insert: we add the Inserted parameter to the
           environment to track equalities with external components that the
           parameter might add. *)
        let mty = Subst.modtype Keep st.subst param_ty in
        let env = Env.add_module ~arg:true param Mp_present mty st.env in
        I.expand_params { st with env }
    | Keep ((Named arg,  _mty) , Named (Some param, _param), _) ->
        let res =
          Option.map (fun res ->
              let scope = Ctype.create_scope () in
              let subst = Subst.add_module param arg Subst.identity in
              Subst.modtype (Rescope scope) subst res
            )
            st.res
        in
        let subst = Subst.add_module param arg st.subst in
        I.expand_params { st with subst; res }
    | Keep (((Anonymous|Empty_struct), mty),
            Named (Some param, _param), _) ->
        let mty' = Subst.modtype Keep st.subst mty in
        let env = Env.add_module ~arg:true param Mp_present mty' st.env in
        let res = Option.map (Mtype.nondep_supertype env [param]) st.res in
        I.expand_params { st with env; res}

  let diff env ~f ~args =
    let params, res = retrieve_functor_params env f in
    let module Compute = Diff.Right_variadic(struct
        let update = update
        let test (state:Defs.state) (arg,arg_mty) param =
          let loc = Location.none in
          let res = match (arg:Error.functor_arg_descr), param with
            | (Unit|Empty_struct), Unit -> Ok Tcoerce_none
            | Unit, Named _ | (Anonymous | Named _), Unit ->
                Result.Error (Error.Incompatible_params(arg,param))
            | ( Anonymous | Named _ | Empty_struct ), Named (_, param) ->
               let direction=Directionality.unknown ~mark:false in
                match
                  modtypes
                    ~core:core_inclusion ~direction ~loc
                    state.env state.subst arg_mty param
                    Shape.dummy_mod
                with
                | Error mty -> Result.Error (Error.Mismatch mty)
                | Ok (cc, _) -> Ok cc
          in
          res
        let weight = weight
      end)
    in
    let args = Array.of_list args in
    let params = Array.of_list params in
    let state : Defs.state =
      { env; subst = Subst.identity; res = I.keep_expansible_param res }
    in
    Compute.diff state args params

end

(* Hide the context and substitution parameters to the outside world *)

let modtypes_with_shape ~shape ~loc env ~mark mty1 mty2 =
  (* modtypes with shape is used when typing module expressions in [Typemod] *)
  let direction = Directionality.strictly_positive ~mark in
  match
    modtypes ~core:core_inclusion ~direction ~loc env Subst.identity
      mty1 mty2 shape
  with
  | Ok (cc, shape) -> cc, shape
  | Error reason -> raise (Error (env, Error.(In_Module_type reason)))

let modtypes_consistency ~loc env mty1 mty2 =
  let direction = Directionality.unknown ~mark:false in
  match
    modtypes ~core:core_consistency ~direction ~loc env Subst.identity
      mty1 mty2 Shape.dummy_mod
  with
  | Ok _ -> ()
  | Error reason -> raise (Error (env, Error.(In_Module_type reason)))

let modtypes ~loc env ~mark mty1 mty2 =
  let direction = Directionality.unknown ~mark in
  match
    modtypes ~core:core_inclusion ~direction ~loc env Subst.identity
      mty1 mty2 Shape.dummy_mod
  with
  | Ok (cc, _) -> cc
  | Error reason -> raise (Error (env, Error.(In_Module_type reason)))

let gen_signatures env ~direction sig1 sig2 =
  match
    signatures
      ~core:core_inclusion ~direction ~loc:Location.none env
      Subst.identity sig1 sig2 Shape.dummy_mod
  with
  | Ok (cc, _) -> cc
  | Error reason -> raise (Error(env,Error.(In_Signature reason)))

let signatures env ~mark sig1 sig2 =
  let direction = Directionality.unknown ~mark in
  gen_signatures env ~direction sig1 sig2

let check_implementation env impl intf =
  let direction = Directionality.strictly_positive ~mark:true in
  ignore (gen_signatures env ~direction impl intf)

let type_declarations ~loc env ~mark id decl1 decl2 =
  let direction = Directionality.unknown ~mark in
  match Core_inclusion.type_declarations ~loc env ~direction
          Subst.identity id decl1 decl2
  with
  | Ok _ -> ()
  | Error (Error.Core reason) ->
      raise (Error(env,Error.(In_Type_declaration(id,reason))))
  | Error _ -> assert false

let strengthened_module_decl ~loc ~aliasable env ~mark md1 path1 md2 =
  let direction = Directionality.unknown ~mark in
  match strengthened_module_decl ~core:core_inclusion ~loc ~aliasable ~direction
          env Subst.identity md1 path1 md2 Shape.dummy_mod with
  | Ok (x, _shape) -> x
  | Error mdiff ->
      raise (Error(env,Error.(In_Module_type mdiff)))

let expand_module_alias ~strengthen env path =
  match expand_module_alias ~strengthen env path with
  | Ok x -> x
  | Result.Error _ ->
      raise (Error(env,In_Expansion(Error.Unbound_module_path path)))

let check_modtype_equiv ~loc env id mty1 mty2 =
  let direction = Directionality.unknown ~mark:true in
  match
    check_modtype_equiv ~core:core_inclusion ~loc ~direction env mty1 mty2
  with
  | Ok _ -> ()
  | Error e ->
      raise (Error(env,
                   Error.(In_Module_type_substitution (id,diff mty1 mty2 e)))
            )
