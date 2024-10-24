(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Partout, INRIA Saclay                        *)
(*   Nicolas Chataing, ENS Paris                                          *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types
open Data_types
open Head_shape_types

type t = shape

let constructor_tag ~loc n =
  if (n < 0 || n > Obj.last_non_constant_constructor_tag) then
    (* TODO: turn into a proper error with a registered printer. *)
    Location.raise_errorf ~loc
      "The shape (constr ~tag:n) can only be used for regular block \
       constructor, ~tag:%d is outside valid range." n;
  Tag n

let rec of_shape_name loc : Asttypes.shape_name -> shape = function
  | Any -> Shape.any
  | Imm n -> Shape.imm [Imm n]
  | Constructor { tag; size } -> Shape.block ?size [constructor_tag ~loc tag]
  | Int -> Shape.any_immediate
  | Float -> Shape.float
  | String -> Shape.string
  | Tuple { size } -> Shape.tuple ?size ()
  | Array -> Shape.array
  | Floatarray -> Shape.floatarray
  | Function -> Shape.\#function
  | Object -> Shape.\#object
  | Lazy arg_shape_names ->
      let arg_shape =
        List.fold_left (fun shape name ->
          Shape.union shape (of_shape_name loc name)
        ) Shape.empty arg_shape_names
      in
      Shape.\#lazy arg_shape
  | Continuation -> Shape.continuation
  | Extensible_variant -> Shape.extensible_variant
  | Polymorphic_variant { has_consts; has_nonconsts } ->
      Shape.polymorphic_variant ~has_consts ~has_nonconsts
  | Abstract { size } -> Shape.abstract ?size ()
  | Custom { size } -> Shape.custom ?size ()

let rec of_type_expr env ty fuel =
  match Types.get_desc ty with
  | Tvar _ | Tunivar _ ->
      (* FIXME: variables that are universally quantified
         (including type parameters) should get [any], but GADT
         variables that are existentially quantified should get
         [poison] instead -- they are not separated. *)
      Shape.any
  | Tconstr (p, args, _abbrev) ->
      (* Both [of_predef_abstract_type] and [of_typedescr] may loop over
         infinite types or recursive expansions; decrease fuel now. *)
      if fuel = 0 then of_unknown_type env p args else
      let fuel = fuel - 1 in
      begin match Predef.find_type_constr p with
      | Some (#Predef.abstract_type_constr as tconstr) ->
          of_predef_abstract_type env tconstr args fuel
      | None | Some #Predef.data_type_constr ->
      match Env.find_type_descrs p env, Env.find_type p env with
      | descr, decl ->
          of_typedescr env p decl descr ~args fuel
      | exception Not_found ->
          of_unknown_type env p args
      end
  | Ttuple li ->
      Shape.tuple ~size:(List.length li) ()
  | Tarrow _ | Tfunctor _ ->
      Shape.\#function
  | Tpackage _ ->
      (* We could be more precise on the arity of
         first-class modules. *)
      Shape.tuple ()
  | Tobject _ ->
      Shape.\#object
  | Tvariant row ->
      let has_consts, has_nonconsts =
        if not (Types.row_closed row) then true, true
        else begin
          (* Note: for closed polymorphic variants, we could refine
             our shape by listing specific immediates for constant
             constructors (the hashes of the name). This would allow
             merging two unboxed constructors containing disjoint
             constant polymorphic variants. *)
          let consts = ref false in
          let nonconsts = ref false in
          let notify (_label, row_field) =
            match Types.row_field_repr row_field with
            | Types.Rpresent None -> consts := true
            | Types.Rpresent (Some _) -> nonconsts := true
            | Types.Rabsent -> ()
            | Types.Reither (is_const, _, _) ->
                (if is_const then consts else nonconsts) := true
          in
          List.iter notify (Types.row_fields row);
          !consts, !nonconsts
        end
      in
      Shape.polymorphic_variant ~has_consts ~has_nonconsts
  | Tpoly (ty, _vars) ->
      of_type_expr env ty fuel
  | Tlink _ | Texpand _ | Tsubst _ | Tfield _ | Tnil ->
      (* cannot be returned by [get_desc] *)
      assert false

and of_unknown_type env p args =
  (* FIXME: if one of the parameters contains a non-separated variable,
     then this unknown type should be considered non-separated as well.
     (It may be a projection into this parameter.)
     This corresponds to the DeepSep case of the separability analysis. *)
  ignore (env, p, args);
  Shape.any

and of_predef_abstract_type env tconstr args fuel =
  match tconstr with
  | `Int | `Char -> Shape.any_immediate
  | `Array | `Iarray -> Shape.array
  | `Float -> Shape.float
  | `Floatarray -> Shape.floatarray
  | `Nativeint | `Int32 | `Int64 ->
      Shape.custom ~size:1 ()
  | `String | `Bytes ->
      Shape.string
  | `Continuation ->
      Shape.continuation
  | `Extension_constructor ->
      Shape.block ~size:1 [Tag Obj.object_tag]
  | `Lazy_t ->
      (* Once lazy values are forced, their Forward_tag block can be
         'cut short', and then they are represented exactly like the
         underlying type.

         Note that this union may be non-disjoint: it is okay if the
         underlying type can itself have lazy tags, shortcutting will
         be disabled by a runtime check.  *)
      let ty = match args with [ty] -> ty | _ -> assert false in
      Shape.\#lazy (of_type_expr env ty fuel)
  | `Atomic_loc ->
      Shape.tuple ~size:2 ()
  | `Todo_info ->
      Shape.any

and of_typedescr env p ty_decl ty_descr ~args fuel =
  let with_unify_res instance f =
    let snap = Btype.snapshot () in
    Fun.protect ~finally:(fun () -> Btype.backtrack snap) @@ fun () ->
    Ctype.unify env instance (Ctype.newconstr p args);
    f ()
  in
  let of_type_manifest ty =
    let ty = Ctype.apply env ty_decl.type_params ty args in
    of_type_expr env ty fuel
  in
  let of_unboxed_label lbl =
    let (_universals, ty_arg, ty_res) =
      Ctype.instance_label ~fixed:false lbl in
    with_unify_res ty_res @@ fun () ->
    of_type_expr env ty_arg fuel
  in
  let of_unboxed_cstr cstr =
    let cstr_args, cstr_res, _existentials =
      Ctype.instance_constructor Keep_existentials_flexible cstr in
    with_unify_res cstr_res @@ fun () ->
    (* an unboxed variant must have exactly one argument *)
    begin match cstr_args with
    | [ty] -> of_type_expr env ty fuel
    | _ -> assert false
    end
  in
  match ty_descr with
  | Type_record (lbls, Record_regular) ->
      Shape.tuple ~size:(List.length lbls) ()
  | Type_record (_lbls, Record_float) ->
      Shape.floatarray
  | Type_record (fields, Record_unboxed _) ->
      (* an [@@unboxed] record must have exactly one field *)
      begin match fields with
      | [lbl] -> of_unboxed_label lbl
      | _ -> assert false
      end
  | Type_record (lbls, Record_inlined tag) ->
      Shape.block ~size:(List.length lbls) [Tag tag]
  | Type_record (lbls, Record_extension _) ->
      (* non-constant extension constructors have tag 0,
         and one additional argument storing the
         extension constructor dynamic value. *)
      Shape.block ~size:(1 + List.length lbls) [Tag 0]
  | Type_open ->
      Shape.extensible_variant
  | Type_variant (cstrs, Variant_unboxed) ->
      (* an [@@unboxed] variant must have exactly one constructor
         with one parameter *)
      begin match cstrs with
      | [cstr] -> of_unboxed_cstr cstr
      | _ -> assert false
      end
  | Type_variant (cstr_descrs, Variant_regular) ->
      let of_cstr_descr descr =
        match descr.cstr_tag with
        | Cstr_constant n -> Shape.imm [Imm n]
        | Cstr_block tag -> Shape.block ~size:descr.cstr_arity [Tag tag]
        | Cstr_unboxed _ ->
            (* Note: it would be incorrect to call the function
               [of_unboxed_cstr_description] below, as the shape we
               compute here depends on the type parameters [args], in
               general it is more precise -- and cannot be cached in
               the type definition. *)
            of_unboxed_cstr descr
        | Cstr_extension _ ->
            (* cannot occur in regular variants *)
            assert false
      in
      (* Here we use the {!union} function to compute the head shape
         of the variant, without trying to enforce that the union is
         disjoint. Indeed, we already know that it must be disjoint,
         otherwise it would have been rejected at declaration time by
         the {!check_typedecl} function below. *)
      List.map of_cstr_descr cstr_descrs
      |> List.fold_left Shape.union Shape.empty
  | Type_abstract _ | Type_external _ ->
      match ty_decl.type_manifest with
      | Some ty -> of_type_manifest ty
      | None ->
      match Builtin_attributes.find_shapes ty_decl.type_attributes with
      | Some shape_names ->
        List.map (of_shape_name ty_decl.type_loc) shape_names
        |> List.fold_left Shape.union Shape.empty
      | None ->
      match ty_decl.type_immediate with
      | Always -> Shape.any_immediate
      | Always_on_64bits ->
        (* TODO maybe refine? *)
        Shape.any
      | Unknown ->
        of_unknown_type env p args

let initial_fuel =
  (* choice of fuel: see
     {!Typedecl_unboxed.get_unboxed_type_representation} *)
  100

let of_type_path env path =
  let decl = Env.find_type path env in
  let params = Ctype.instance_list decl.type_params in
  let ty = Ctype.newconstr path params in
  of_type_expr env ty initial_fuel

(* This function computes the head shape of each unboxed constructors
   for any possible value of the type parameters; this is what is
   required to check that the definition is valid, and it can be
   cached and reused for pattern-matching compilation. *)
let of_unboxed_cstr_description env descr =
  try Misc.Cached.force descr (fun (decl, ty_arg) ->
    let params = Ctype.instance_list decl.type_params in
    let ty_arg = Ctype.apply env decl.type_params ty_arg params in
    of_type_expr env ty_arg initial_fuel)
  with Misc.Cached.Forcing_race ->
    (* There could be a forcing race in the case of a cycle:
         type foo =
           | Self of foo [@unboxed]
           | ...
       If ... contains a constructor, then the declaration must be
       rejected: for example if foo has a constructor [Bar], then it
       conflicts with [Self Bar]. This can only be accepted if there
       are no other constructors than [Self] in [foo], and in this
       case the fixpoint is under-determined and we could give any
       shape; conservatively we use [any].
    *)
    Shape.any

let of_regular_cstr_description env descr =
  match descr.cstr_tag with
  | Cstr_constant n -> Shape.imm [Imm n]
  | Cstr_block tag -> Shape.block ~size:descr.cstr_arity [Tag tag]
  | Cstr_unboxed descr -> of_unboxed_cstr_description env descr
  | Cstr_extension _ ->
      (* cannot occur in regular variants *)
      assert false

let cstr_is_unboxed cstr =
  match cstr.cstr_tag with
  | Cstr_constant _ | Cstr_block _ | Cstr_extension _ -> false
  | Cstr_unboxed _ -> true

let find_constructors_with_unboxed = function
  | Type_open | Type_record _
  | Type_abstract _ | Type_external _
  | Type_variant (_, Variant_unboxed) -> None
  | Type_variant (cstrs, Variant_regular) ->
      let has_unboxed_cstr = List.exists cstr_is_unboxed cstrs in
      if has_unboxed_cstr then Some cstrs
      else None

let check_typedecl_conflicts ~loc genv cstrs =
  (* Boxed constructors, by construction, cannot have overlapping
     representations, as they get assigned distinct immediates or tags.

     If an overlap arises, it is necessarily between an unboxed
     constructor an another constructor (boxed or unboxed).
  
     Checking all pairs of a boxed constructor and another constructor
     would be quadratic. Instead we do as follows:
     - in one traversal of all constructors, we check if there is
       a conflict by accumulating the union of all shapes seen so far;
     - if there is a conflict at position [i], we traverse all
       constructors at position [j < i] to detect the first constructor
       [j] that overlaps with [i].
  *)
  let cstr_shapes =
    Array.of_list @@ List.map (fun descr ->
      of_regular_cstr_description genv descr
    ) cstrs
  in
  let overlap sh1 sh2 = not Shape.(is_empty (inter sh1 sh2)) in
  let conflict =
    let acc = ref Shape.empty in
    cstrs |> List.find_mapi (fun i cstr ->
      let sh = cstr_shapes.(i) in
      if cstr_is_unboxed cstr && overlap !acc sh then Some (i, cstr, sh)
      else (
        acc := Shape.union !acc sh;
        None
      )
    )
  in
  match conflict with
  | None -> ()
  | Some (i, cstri, shi) ->
    cstrs |> List.iteri (fun j cstrj ->
      assert (j < i);
      let shj = cstr_shapes.(j) in
      if overlap shi shj then
        (* TODO: define a proper error with an error printer. *)
        Location.raise_errorf ~loc
          "@[Constructors %s and %s have overlapping representations.\
             @;<1 2>shape of %s: %a\
             @;<1 2>shape of %s: %a\
           @]"
          cstrj.cstr_name cstri.cstr_name
          cstrj.cstr_name Print_head_shape.doc shj
          cstri.cstr_name Print_head_shape.doc shi
    )

let check_typedecl_constraint env (path, decl) =
  match Builtin_attributes.find_shapes decl.type_attributes with
  | Some shape_names ->
      let loc = decl.type_loc in
      let expected_shape =
        List.map (of_shape_name loc) shape_names
        |> List.fold_left Shape.union Shape.empty
      in
      let actual_shape = of_type_path env path in
      if not (Shape.subset actual_shape expected_shape) then
        Location.raise_errorf ~loc
          "@[%a@]"
          Format_doc.pp_print_text
          "In this type declaration, the actual head shape does not \
           match the expected type shape."
  | None -> ()

let check_typedecl env (path, decl) =
  let loc = decl.type_loc in
  let descr =
    try Env.find_type_descrs path env
    with Not_found -> assert false
  in
  begin match find_constructors_with_unboxed descr with
  | None -> ()
  | Some cstrs ->
      check_typedecl_conflicts ~loc env cstrs
  end;
  check_typedecl_constraint env (path, decl);
