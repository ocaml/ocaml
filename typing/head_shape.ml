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

let of_attributes loc attrs =
  match Builtin_attributes.find_shapes attrs with
  | None -> None
  | Some shapes ->
      Some (
        List.fold_left (fun acc shape ->
          Shape.union acc (of_shape_name loc shape)
        ) Shape.empty shapes
      )

(* During head-shape computation we keep track of universal and
   existential variables encountered during the way. Universal
   variables are the parameters of the initial parametrized type
   (when checking a declaration), and for example variables of
   polymorphic record fields encountered along the way. Existential
   variables are introduced by GADT constructors.

   Tracking existential variables is necessary to detect non-separable
   definitions such as:
   {[
     type 'a id = Id of 'a [@unboxed]
     type wrong = Any : 'ex id -> wrong [@unboxed]
   ]}
   The values at type [wrong] are exactly the values at type ['ex],
   whcih can be instantiated with both [float] and [int].

   Tracking universal variables is not strictly necessary, but we thought
   that it would be a better design to avoid dangling variables.
*)
type mode = Forall | Exists
type env = { global : Env.t; local : (type_expr * mode) list }

let add_forall_vars vars env =
  let vars = List.map (fun var -> (var, Forall)) vars in
  { env with local = vars @ env.local }

let add_exists_vars vars env =
  let vars = List.map (fun var -> (var, Exists)) vars in
  { env with local = vars @ env.local }

(* Existential variables can sometimes be refined by unification into
   non-variables expressions. Consider for example:
   {[
     type 'a pair = Pair : ('b * 'c) -> ('b * 'c) pair [@unboxed]
     type any_pair : Any : 'ex pair -> any_pair [@unboxed]
   ]}
   When checking this declaration we start with an existential [ex],
   which gets refined into [('b * 'c)] when checking the [Pair]
   constructor. When we see a constructed type such as [('b * 'c)] in
   our environment, we add its free variables (here ['b], ['c]) as new
   existentials -- the user can choose their instantiation
   arbitrarily, by choosing appropriate values of ['ex].
*)
let normalize_binding env = function
  | (_var, Forall) as binding -> [binding]
  | (var, Exists) ->
      Ctype.free_variables ~env var
      |> List.map (fun var -> (var, Exists))

let var_is_existential env tyvar =
  env.local
  |> List.concat_map (normalize_binding env.global)
  |> List.find_map (fun (var, mode) ->
    if Types.eq_type var tyvar then Some mode
    else None)
  |> function
  | None -> false
  | Some Forall -> false
  | Some Exists -> true

let rec of_type_expr env ty fuel =
  match Types.get_desc ty with
  | Tvar _ | Tunivar _ ->
      if var_is_existential env ty
      then Shape.poison
      else Shape.any
  | Tconstr (p, args, _abbrev) ->
      (* Both [of_predef_abstract_type] and [of_typedescr] may loop over
         infinite types or recursive expansions; decrease fuel now. *)
      if fuel = 0 then of_unknown_type env p args else
      let fuel = fuel - 1 in
      begin match Predef.find_type_constr p with
      | Some (#Predef.abstract_type_constr as tconstr) ->
          of_predef_abstract_type env tconstr args fuel
      | None | Some #Predef.data_type_constr ->
      match Env.find_type_descrs p env.global, Env.find_type p env.global with
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
  | Tpoly (ty, vars) ->
      let env = add_forall_vars vars env in
      of_type_expr env ty fuel
  | Tlink _ | Texpand _ | Tsubst _ | Tfield _ | Tnil ->
      (* cannot be returned by [get_desc] *)
      assert false

and of_unknown_type env _p args =
  (* The shape of [foo t] when [t] is abstract must be [poison] not
     only if [foo] is an existential variable, but also if it
     _contains_ an existential variables, due to the ability of
     decomposing argument types with constraints. Consider for
     example:
     {[
       type wrong = Any : ('ex * int) unk -> wrong [@unboxed]
     ]}
     It looks like the argument ['ex * int] of [unk] will always
     have the shape of a pair, but in fact [unk] may be defined as
     {[
       type 'a unk = 'b constraint 'a = 'b * 'c
     ]}
  *)
  let free_vars = Ctype.free_variables_list ~env:env.global args in
  if List.exists (var_is_existential env) free_vars
  then Shape.poison
  else Shape.any

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
    Ctype.unify env.global instance (Ctype.newconstr p args);
    f ()
  in
  let of_type_manifest ty =
    let ty = Ctype.apply env.global ty_decl.type_params ty args in
    of_type_expr env ty fuel
  in
  let of_unboxed_label lbl =
    let (universals, ty_arg, ty_res) =
      Ctype.instance_label ~fixed:false lbl in
    let env = add_forall_vars universals env in
    with_unify_res ty_res @@ fun () ->
    of_type_expr env ty_arg fuel
  in
  let of_unboxed_cstr cstr =
    let cstr_args, cstr_res, existentials =
      Ctype.instance_constructor Keep_existentials_flexible cstr in
    let env = add_exists_vars existentials env in
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
      let from_immediacy =
        match ty_decl.type_immediate with
        | Always -> Some Shape.any_immediate
        | Always_on_64bits ->
          (* TODO maybe refine? *)
          None
        | Unknown ->
          None
      in
      let from_attributes =
        of_attributes ty_decl.type_loc ty_decl.type_attributes
      in
      match from_immediacy, from_attributes with
      | Some sh1, Some sh2 -> Shape.inter sh1 sh2
      | Some sh, None | None, Some sh -> sh
      | None, None ->
        of_unknown_type env p args

let initial_fuel =
  (* choice of fuel: see
     {!Typedecl_unboxed.get_unboxed_type_representation} *)
  100

let instance_decl genv decl =
  let params = Ctype.instance_list decl.type_params in
  let env =
    let param_vars =
      (* in presence of constraints such as
           type 'a t = ... constraint 'a = 'b * 'c
         the [type_params] may be non-variable types,
         here ['b * 'c]. *)
      Ctype.free_variables_list ~env:genv params in
    { global = genv; local = [] }
    |> add_forall_vars param_vars
  in
  env, params

let of_type_path genv path =
  let decl = Env.find_type path genv in
  let env, params = instance_decl genv decl in
  let ty = Ctype.newconstr path params in
  of_type_expr env ty initial_fuel

(* This function computes the head shape of each unboxed constructors
   for any possible value of the type parameters; this is what is
   required to check that the definition is valid, and it can be
   cached and reused for pattern-matching compilation. *)
let of_unboxed_cstr_description genv descr =
  try Misc.Cached.force descr (fun (decl, ty_arg) ->
    let env, params = instance_decl genv decl in
    let ty_arg = Ctype.apply genv decl.type_params ty_arg params in
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

let of_regular_cstr_description genv descr =
  match descr.cstr_tag with
  | Cstr_constant n -> Shape.imm [Imm n]
  | Cstr_block tag -> Shape.block ~size:descr.cstr_arity [Tag tag]
  | Cstr_unboxed descr -> of_unboxed_cstr_description genv descr
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

let check_typedecl_constraint ~loc decl shape =
  match of_attributes loc decl.type_attributes with
  | Some expected_shape ->
      if not (Shape.subset shape expected_shape) then
        Location.raise_errorf ~loc
          "@[%a@]"
          Format_doc.pp_print_text
          "In this type declaration, the actual head shape does not \
           match the expected type shape."
  | None -> ()

let check_typedecl_separated ~loc shape =
  if not shape.separated then
    Location.raise_errorf ~loc
      "@[%a@]"
      Format_doc.pp_print_text
      "This type declaration is non-separated, \
       it may contain both float and non-float values."

(* We check three soundness properties on head shapes:
   - The new type must be separated.
   - If unboxed constructors are used, they must not introduce
     representation conflicts (two distinct source terms that
     have the same representation).
   - If a shape annotation is present, the actual shape of the type
     must be a subset of the annotated shape.

   The implementation tries to be zero-cost: it computes nothing at
   all unless unboxed constructors or shape annotations are used.
*)
let check_typedecl genv (path, decl) =
  let loc = decl.type_loc in
  let descr =
    try Env.find_type_descrs path genv
    with Not_found -> assert false
  in
  let shape = of_type_path genv path in
  check_typedecl_separated ~loc shape;
  begin match find_constructors_with_unboxed descr with
  | None -> ()
  | Some cstrs ->
      check_typedecl_conflicts ~loc genv cstrs;
  end;
  check_typedecl_constraint ~loc decl shape;
