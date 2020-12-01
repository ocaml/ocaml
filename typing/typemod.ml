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

open Misc
open Longident
open Path
open Asttypes
open Parsetree
open Types
open Format

module String = Misc.Stdlib.String

module Sig_component_kind = struct
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Module -> "module"
    | Module_type -> "module type"
    | Extension_constructor -> "extension constructor"
    | Class -> "class"
    | Class_type -> "class type"

  (** Whether the name of a component of that kind can appear in a type. *)
  let can_appear_in_types = function
    | Value
    | Extension_constructor ->
        false
    | Type
    | Module
    | Module_type
    | Class
    | Class_type ->
        true
end

type hiding_error =
  | Illegal_shadowing of {
      shadowed_item_id: Ident.t;
      shadowed_item_kind: Sig_component_kind.t;
      shadowed_item_loc: Location.t;
      shadower_id: Ident.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }
  | Appears_in_signature of {
      opened_item_id: Ident.t;
      opened_item_kind: Sig_component_kind.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }

type error =
    Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | With_makes_applicative_functor_ill_typed of
      Longident.t * Path.t * Includemod.error list
  | With_changes_module_alias of Longident.t * Ident.t * Path.t
  | With_cannot_remove_constrained_type
  | Repeated_name of Sig_component_kind.t * string
  | Non_generalizable of type_expr
  | Non_generalizable_class of Ident.t * class_declaration
  | Non_generalizable_module of module_type
  | Implementation_is_required of string
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Longident.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias of Path.t
  | Cannot_scrape_package_type of Path.t
  | Badly_formed_signature of string * Typedecl.error
  | Cannot_hide_id of hiding_error
  | Invalid_type_subst_rhs

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

open Typedtree

let rec path_concat head p =
  match p with
    Pident tail -> Pdot (Pident head, Ident.name tail)
  | Pdot (pre, s) -> Pdot (path_concat head pre, s)
  | Papply _ -> assert false

(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Env.scrape_alias env mty with
    Mty_signature sg -> sg
  | Mty_alias path ->
      raise(Error(loc, env, Cannot_scrape_alias path))
  | _ -> raise(Error(loc, env, Signature_expected))

let extract_sig_open env loc mty =
  match Env.scrape_alias env mty with
    Mty_signature sg -> sg
  | Mty_alias path ->
      raise(Error(loc, env, Cannot_scrape_alias path))
  | mty -> raise(Error(loc, env, Structure_expected mty))

(* Compute the environment after opening a module *)

let type_open_ ?used_slot ?toplevel ovf env loc lid =
  let path = Env.lookup_module_path ~load:true ~loc:lid.loc lid.txt env in
  match Env.open_signature ~loc ?used_slot ?toplevel ovf path env with
  | Ok env -> path, env
  | Error _ ->
      let md = Env.find_module path env in
      ignore (extract_sig_open env lid.loc md.md_type);
      assert false

let initial_env ~loc ~safe_string ~initially_opened_module
    ~open_implicit_modules =
  let env =
    if safe_string then
      Env.initial_safe_string
    else
      Env.initial_unsafe_string
  in
  let open_module env m =
    let open Asttypes in
    let lexbuf = Lexing.from_string m in
    let txt =
      Location.init lexbuf (Printf.sprintf "command line argument: -open %S" m);
      Parse.simple_module_path lexbuf in
        snd (type_open_ Override env loc {txt;loc})
  in
  let add_units env units =
    String.Set.fold
      (fun name env ->
         Env.add_persistent_structure (Ident.create_persistent name) env)
      units
      env
  in
  let units =
    List.map Env.persistent_structures_of_dir (Load_path.get ())
  in
  let env, units =
    match initially_opened_module with
    | None -> (env, units)
    | Some m ->
        (* Locate the directory that contains [m], adds the units it
           contains to the environment and open [m] in the resulting
           environment. *)
        let rec loop before after =
          match after with
          | [] -> None
          | units :: after ->
              if String.Set.mem m units then
                Some (units, List.rev_append before after)
              else
                loop (units :: before) after
        in
        let env, units =
          match loop [] units with
          | None ->
              (env, units)
          | Some (units_containing_m, other_units) ->
              (add_units env units_containing_m, other_units)
        in
        (open_module env m, units)
  in
  let env = List.fold_left add_units env units in
  List.fold_left open_module env open_implicit_modules

let type_open_descr ?used_slot ?toplevel env sod =
  let (path, newenv) =
    Builtin_attributes.warning_scope sod.popen_attributes
      (fun () ->
         type_open_ ?used_slot ?toplevel sod.popen_override env sod.popen_loc
           sod.popen_expr
      )
  in
  let od =
    {
      open_expr = (path, sod.popen_expr);
      open_bound_items = [];
      open_override = sod.popen_override;
      open_env = newenv;
      open_attributes = sod.popen_attributes;
      open_loc = sod.popen_loc;
    }
  in
  (od, newenv)

(* Forward declaration, to be filled in by type_module_type_of *)
let type_module_type_of_fwd :
    (Env.t -> Parsetree.module_expr ->
      Typedtree.module_expr * Types.module_type) ref
  = ref (fun _env _m -> assert false)

(* Additional validity checks on type definitions arising from
   recursive modules *)

let check_recmod_typedecls env decls =
  let recmod_ids = List.map fst decls in
  List.iter
    (fun (id, md) ->
      List.iter
        (fun path ->
          Typedecl.check_recmod_typedecl env md.Types.md_loc recmod_ids
                                         path (Env.find_type path env))
        (Mtype.type_paths env (Pident id) md.Types.md_type))
    decls

(* Merge one "with" constraint in a signature *)

let rec add_rec_types env = function
    Sig_type(id, decl, Trec_next, _) :: rem ->
      add_rec_types (Env.add_type ~check:true id decl env) rem
  | _ -> env

let check_type_decl env loc id row_id newdecl decl rs rem =
  let env = Env.add_type ~check:true id newdecl env in
  let env =
    match row_id with
    | None -> env
    | Some id -> Env.add_type ~check:false id newdecl env
  in
  let env = if rs = Trec_not then env else add_rec_types env rem in
  Includemod.type_declarations ~mark:Mark_both ~loc env id newdecl decl;
  Typedecl.check_coherence env loc (Path.Pident id) newdecl

let update_rec_next rs rem =
  match rs with
    Trec_next -> rem
  | Trec_first | Trec_not ->
      match rem with
        Sig_type (id, decl, Trec_next, priv) :: rem ->
          Sig_type (id, decl, rs, priv) :: rem
      | Sig_module (id, pres, mty, Trec_next, priv) :: rem ->
          Sig_module (id, pres, mty, rs, priv) :: rem
      | _ -> rem

let make_variance p n i =
  let open Variance in
  set May_pos p (set May_neg n (set May_weak n (set Inj i null)))

let rec iter_path_apply p ~f =
  match p with
  | Pident _ -> ()
  | Pdot (p, _) -> iter_path_apply p ~f
  | Papply (p1, p2) ->
     iter_path_apply p1 ~f;
     iter_path_apply p2 ~f;
     f p1 p2 (* after recursing, so we know both paths are well typed *)

let path_is_strict_prefix =
  let rec list_is_strict_prefix l ~prefix =
    match l, prefix with
    | [], [] -> false
    | _ :: _, [] -> true
    | [], _ :: _ -> false
    | s1 :: t1, s2 :: t2 ->
       String.equal s1 s2 && list_is_strict_prefix t1 ~prefix:t2
  in
  fun path ~prefix ->
    match Path.flatten path, Path.flatten prefix with
    | `Contains_apply, _ | _, `Contains_apply -> false
    | `Ok (ident1, l1), `Ok (ident2, l2) ->
       Ident.same ident1 ident2
       && list_is_strict_prefix l1 ~prefix:l2

let iterator_with_env env =
  let env = ref (lazy env) in
  let super = Btype.type_iterators in
  env, { super with
    Btype.it_signature = (fun self sg ->
      (* add all items to the env before recursing down, to handle recursive
         definitions *)
      let env_before = !env in
      env := lazy (Env.add_signature sg (Lazy.force env_before));
      super.Btype.it_signature self sg;
      env := env_before
    );
    Btype.it_module_type = (fun self -> function
    | Mty_functor (param, mty_body) ->
      let env_before = !env in
      begin match param with
      | Unit -> ()
      | Named (param, mty_arg) ->
        self.Btype.it_module_type self mty_arg;
        match param with
        | None -> ()
        | Some id ->
          env := lazy (Env.add_module ~arg:true id Mp_present
                       mty_arg (Lazy.force env_before))
      end;
      self.Btype.it_module_type self mty_body;
      env := env_before;
    | mty ->
      super.Btype.it_module_type self mty
    )
  }

let retype_applicative_functor_type ~loc env funct arg =
  let mty_functor = (Env.find_module funct env).md_type in
  let mty_arg = (Env.find_module arg env).md_type in
  let mty_param =
    match Env.scrape_alias env mty_functor with
    | Mty_functor (Named (_, mty_param), _) -> mty_param
    | _ -> assert false (* could trigger due to MPR#7611 *)
  in
  Includemod.check_modtype_inclusion ~loc env mty_arg arg mty_param

(* When doing a deep destructive substitution with type M.N.t := .., we change M
   and M.N and so we have to check that uses of the modules other than just
   extracting components from them still make sense. There are only two such
   kinds of uses:
   - applicative functor types: F(M).t might not be well typed anymore
   - aliases: module A = M still makes sense but it doesn't mean the same thing
     anymore, so it's forbidden until it's clear what we should do with it.
   This function would be called with M.N.t and N.t to check for these uses. *)
let check_usage_of_path_of_substituted_item paths env signature ~loc ~lid =
  let iterator =
    let env, super = iterator_with_env env in
    { super with
      Btype.it_signature_item = (fun self -> function
      | Sig_module (id, _, { md_type = Mty_alias aliased_path; _ }, _, _)
        when List.exists
               (fun path -> path_is_strict_prefix path ~prefix:aliased_path)
               paths
        ->
         let e = With_changes_module_alias (lid.txt, id, aliased_path) in
         raise(Error(loc, Lazy.force !env, e))
      | sig_item ->
         super.Btype.it_signature_item self sig_item
      );
      Btype.it_path = (fun referenced_path ->
        iter_path_apply referenced_path ~f:(fun funct arg ->
          if List.exists
               (fun path -> path_is_strict_prefix path ~prefix:arg)
               paths
          then
            let env = Lazy.force !env in
            try retype_applicative_functor_type ~loc env funct arg
            with Includemod.Error explanation ->
              raise(Error(loc, env,
                          With_makes_applicative_functor_ill_typed
                            (lid.txt, referenced_path, explanation)))
        )
      );
    }
  in
  iterator.Btype.it_signature iterator signature;
  Btype.unmark_iterators.Btype.it_signature Btype.unmark_iterators signature

(* After substitution one also needs to re-check the well-foundedness
   of type declarations in recursive modules *)
let rec extract_next_modules = function
  | Sig_module (id, _, mty, Trec_next, _) :: rem ->
      let (id_mty_l, rem) = extract_next_modules rem in
      ((id, mty) :: id_mty_l, rem)
  | sg -> ([], sg)

let check_well_formed_module env loc context mty =
  (* Format.eprintf "@[check_well_formed_module@ %a@]@."
     Printtyp.modtype mty; *)
  let open Btype in
  let iterator =
    let rec check_signature env = function
      | [] -> ()
      | Sig_module (id, _, mty, Trec_first, _) :: rem ->
          let (id_mty_l, rem) = extract_next_modules rem in
          begin try
            check_recmod_typedecls (Lazy.force env) ((id, mty) :: id_mty_l)
          with Typedecl.Error (_, err) ->
            raise (Error (loc, Lazy.force env,
                          Badly_formed_signature(context, err)))
          end;
          check_signature env rem
      | _ :: rem ->
          check_signature env rem
    in
    let env, super = iterator_with_env env in
    { super with
      it_type_expr = (fun _self _ty -> ());
      it_signature = (fun self sg ->
        let env_before = !env in
        let env = lazy (Env.add_signature sg (Lazy.force env_before)) in
        check_signature env sg;
        super.it_signature self sg);
    }
  in
  iterator.it_module_type iterator mty

let () = Env.check_well_formed_module := check_well_formed_module

let type_decl_is_alias sdecl = (* assuming no explicit constraint *)
  match sdecl.ptype_manifest with
  | Some {ptyp_desc = Ptyp_constr (lid, stl)}
       when List.length stl = List.length sdecl.ptype_params ->
     begin
       match
         List.iter2 (fun x (y, _) ->
             match x, y with
               {ptyp_desc=Ptyp_var sx}, {ptyp_desc=Ptyp_var sy}
                  when sx = sy -> ()
             | _, _ -> raise Exit)
           stl sdecl.ptype_params;
       with
       | exception Exit -> None
       | () -> Some lid
     end
  | _ -> None
;;

let params_are_constrained =
  let rec loop = function
    | [] -> false
    | hd :: tl ->
       match (Btype.repr hd).desc with
       | Tvar _ -> List.memq hd tl || loop tl
       | _ -> true
  in
  loop
;;

let merge_constraint initial_env remove_aliases loc sg constr =
  let lid =
    match constr with
    | Pwith_type (lid, _) | Pwith_module (lid, _)
    | Pwith_typesubst (lid, _) | Pwith_modsubst (lid, _) -> lid
  in
  let destructive_substitution =
    match constr with
    | Pwith_type _ | Pwith_module _ -> false
    | Pwith_typesubst _ | Pwith_modsubst _ -> true
  in
  let real_ids = ref [] in
  let rec merge sig_env sg namelist row_id =
    match (sg, namelist, constr) with
      ([], _, _) ->
        raise(Error(loc, sig_env, With_no_component lid.txt))
    | (Sig_type(id, decl, rs, priv) :: rem, [s],
       Pwith_type (_, ({ptype_kind = Ptype_abstract} as sdecl)))
      when Ident.name id = s && Typedecl.is_fixed_type sdecl ->
        let decl_row =
          let arity = List.length sdecl.ptype_params in
          {
            type_params =
              List.map (fun _ -> Btype.newgenvar()) sdecl.ptype_params;
            type_arity = arity;
            type_kind = Type_abstract;
            type_private = Private;
            type_manifest = None;
            type_variance =
              List.map
                (fun (_, (v, i)) ->
                   let (c, n) =
                     match v with
                     | Covariant -> true, false
                     | Contravariant -> false, true
                     | NoVariance -> false, false
                   in
                   make_variance (not n) (not c) (i = Injective)
                )
                sdecl.ptype_params;
            type_separability =
              Types.Separability.default_signature ~arity;
            type_loc = sdecl.ptype_loc;
            type_is_newtype = false;
            type_expansion_scope = Btype.lowest_level;
            type_attributes = [];
            type_immediate = Unknown;
            type_unboxed = unboxed_false_default_false;
            type_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
          }
        and id_row = Ident.create_local (s^"#row") in
        let initial_env =
          Env.add_type ~check:false id_row decl_row initial_env
        in
        let tdecl =
          Typedecl.transl_with_constraint id (Some(Pident id_row))
            ~sig_env ~sig_decl:decl ~outer_env:initial_env sdecl in
        let newdecl = tdecl.typ_type in
        check_type_decl sig_env sdecl.ptype_loc id row_id newdecl decl rs rem;
        let decl_row = {decl_row with type_params = newdecl.type_params} in
        let rs' = if rs = Trec_first then Trec_not else rs in
        (Pident id, lid, Twith_type tdecl),
        Sig_type(id_row, decl_row, rs', priv)
        :: Sig_type(id, newdecl, rs, priv)
        :: rem
    | (Sig_type(id, sig_decl, rs, priv) :: rem , [s],
       (Pwith_type (_, sdecl) | Pwith_typesubst (_, sdecl) as constr))
      when Ident.name id = s ->
        let tdecl =
          Typedecl.transl_with_constraint id None
            ~sig_env ~sig_decl ~outer_env:initial_env sdecl in
        let newdecl = tdecl.typ_type and loc = sdecl.ptype_loc in
        check_type_decl sig_env loc id row_id newdecl sig_decl rs rem;
        begin match constr with
          Pwith_type _ ->
            (Pident id, lid, Twith_type tdecl),
            Sig_type(id, newdecl, rs, priv) :: rem
        | (* Pwith_typesubst *) _ ->
            real_ids := [Pident id];
            (Pident id, lid, Twith_typesubst tdecl),
            update_rec_next rs rem
        end
    | (Sig_type(id, _, _, _) :: rem, [s], (Pwith_type _ | Pwith_typesubst _))
      when Ident.name id = s ^ "#row" ->
        merge sig_env rem namelist (Some id)
    | (Sig_module(id, pres, md, rs, priv) :: rem, [s], Pwith_module (_, lid'))
      when Ident.name id = s ->
        let path, md' = Env.lookup_module ~loc lid'.txt initial_env in
        let mty = md'.md_type in
        let mty = Mtype.scrape_for_type_of ~remove_aliases sig_env mty in
        let md'' = { md' with md_type = mty } in
        let newmd = Mtype.strengthen_decl ~aliasable:false sig_env md'' path in
        ignore(Includemod.modtypes  ~mark:Mark_both ~loc sig_env
                 newmd.md_type md.md_type);
        (Pident id, lid, Twith_module (path, lid')),
        Sig_module(id, pres, newmd, rs, priv) :: rem
    | (Sig_module(id, _, md, rs, _) :: rem, [s], Pwith_modsubst (_, lid'))
      when Ident.name id = s ->
        let path, md' = Env.lookup_module ~loc lid'.txt initial_env in
        let aliasable = not (Env.is_functor_arg path sig_env) in
        ignore
          (Includemod.strengthened_module_decl ~loc ~mark:Mark_both
             ~aliasable sig_env md' path md);
        real_ids := [Pident id];
        (Pident id, lid, Twith_modsubst (path, lid')),
        update_rec_next rs rem
    | (Sig_module(id, _, md, rs, priv) as item :: rem, s :: namelist, constr)
      when Ident.name id = s ->
        let sg = extract_sig sig_env loc md.md_type in
        let ((path, _, tcstr), newsg) = merge_signature sig_env sg namelist in
        let path = path_concat id path in
        real_ids := path :: !real_ids;
        let item =
          match md.md_type, constr with
            Mty_alias _, (Pwith_module _ | Pwith_type _) ->
              (* A module alias cannot be refined, so keep it
                 and just check that the constraint is correct *)
              item
          | _ ->
              let newmd = {md with md_type = Mty_signature newsg} in
              Sig_module(id, Mp_present, newmd, rs, priv)
        in
        (path, lid, tcstr),
        item :: rem
    | (item :: rem, _, _) ->
        let (cstr, items) = merge sig_env rem namelist row_id
        in
        cstr, item :: items
  and merge_signature env sg namelist =
    let sig_env = Env.add_signature sg env in
    merge sig_env sg namelist None
  in
  try
    let names = Longident.flatten lid.txt in
    let (tcstr, sg) = merge_signature initial_env sg names in
    if destructive_substitution then (
      match List.rev !real_ids with
      | [] -> assert false
      | last :: rest ->
        (* The last item is the one that's removed. We don't need to check how
           it's used since it's replaced by a more specific type/module. *)
        assert (match last with Pident _ -> true | _ -> false);
        match rest with
        | [] -> ()
        | _ :: _ ->
          check_usage_of_path_of_substituted_item
            rest initial_env sg ~loc ~lid;
    );
    let sg =
    match tcstr with
    | (_, _, Twith_typesubst tdecl) ->
       let how_to_extend_subst =
         let sdecl =
           match constr with
           | Pwith_typesubst (_, sdecl) -> sdecl
           | _ -> assert false
         in
         match type_decl_is_alias sdecl with
         | Some lid ->
            let replacement, _ =
              try Env.find_type_by_name lid.txt initial_env
              with Not_found -> assert false
            in
            fun s path -> Subst.add_type_path path replacement s
         | None ->
            let body = Option.get tdecl.typ_type.type_manifest in
            let params = tdecl.typ_type.type_params in
            if params_are_constrained params
            then raise(Error(loc, initial_env,
                             With_cannot_remove_constrained_type));
            fun s path -> Subst.add_type_function path ~params ~body s
       in
       let sub = List.fold_left how_to_extend_subst Subst.identity !real_ids in
       (* This signature will not be used directly, it will always be freshened
          by the caller. So what we do with the scope doesn't really matter. But
          making it local makes it unlikely that we will ever use the result of
          this function unfreshened without issue. *)
       Subst.signature Make_local sub sg
    | (_, _, Twith_modsubst (real_path, _)) ->
       let sub =
         List.fold_left
           (fun s path -> Subst.add_module_path path real_path s)
           Subst.identity
           !real_ids
       in
       (* See explanation in the [Twith_typesubst] case above. *)
       Subst.signature Make_local sub sg
    | _ ->
       sg
    in
    check_well_formed_module initial_env loc "this instantiated signature"
      (Mty_signature sg);
    (tcstr, sg)
  with Includemod.Error explanation ->
    raise(Error(loc, initial_env, With_mismatch(lid.txt, explanation)))

(* Add recursion flags on declarations arising from a mutually recursive
   block. *)

let map_rec fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl -> fn Trec_first d1 :: map_end (fn Trec_next) dl rem

let map_rec_type ~rec_flag fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl ->
      let first =
        match rec_flag with
        | Recursive -> Trec_first
        | Nonrecursive -> Trec_not
      in
      fn first d1 :: map_end (fn Trec_next) dl rem

let rec map_rec_type_with_row_types ~rec_flag fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl ->
      if Btype.is_row_name (Ident.name d1.typ_id) then
        fn Trec_not d1 :: map_rec_type_with_row_types ~rec_flag fn dl rem
      else
        map_rec_type ~rec_flag fn decls rem

(* Add type extension flags to extension constructors *)
let map_ext fn exts rem =
  match exts with
  | [] -> rem
  | d1 :: dl -> fn Text_first d1 :: map_end (fn Text_next) dl rem

(* Auxiliary for translating recursively-defined module types.
   Return a module type that approximates the shape of the given module
   type AST.  Retain only module, type, and module type
   components of signatures.  For types, retain only their arity,
   making them abstract otherwise. *)

let rec approx_modtype env smty =
  match smty.pmty_desc with
    Pmty_ident lid ->
      let (path, _info) =
        Env.lookup_modtype ~use:false ~loc:smty.pmty_loc lid.txt env
      in
      Mty_ident path
  | Pmty_alias lid ->
      let path =
        Env.lookup_module_path ~use:false ~load:false
          ~loc:smty.pmty_loc lid.txt env
      in
      Mty_alias(path)
  | Pmty_signature ssg ->
      Mty_signature(approx_sig env ssg)
  | Pmty_functor(param, sres) ->
      let (param, newenv) =
        match param with
        | Unit -> Types.Unit, env
        | Named (param, sarg) ->
          let arg = approx_modtype env sarg in
          match param.txt with
          | None -> Types.Named (None, arg), env
          | Some name ->
            let rarg = Mtype.scrape_for_functor_arg env arg in
            let scope = Ctype.create_scope () in
            let (id, newenv) =
              Env.enter_module ~scope ~arg:true name Mp_present rarg env
            in
            Types.Named (Some id, arg), newenv
      in
      let res = approx_modtype newenv sres in
      Mty_functor(param, res)
  | Pmty_with(sbody, constraints) ->
      let body = approx_modtype env sbody in
      List.iter
        (fun sdecl ->
          match sdecl with
          | Pwith_type _ -> ()
          | Pwith_typesubst _ -> ()
          | Pwith_module (_, lid') ->
              (* Lookup the module to make sure that it is not recursive.
                 (GPR#1626) *)
              ignore (Env.lookup_module ~use:false ~loc:lid'.loc lid'.txt env)
          | Pwith_modsubst (_, lid') ->
              ignore (Env.lookup_module ~use:false ~loc:lid'.loc lid'.txt env))
        constraints;
      body
  | Pmty_typeof smod ->
      let (_, mty) = !type_module_type_of_fwd env smod in
      mty
  | Pmty_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

and approx_module_declaration env pmd =
  {
    Types.md_type = approx_modtype env pmd.pmd_type;
    md_attributes = pmd.pmd_attributes;
    md_loc = pmd.pmd_loc;
    md_uid = Uid.internal_not_actually_unique;
  }

and approx_sig env ssg =
  match ssg with
    [] -> []
  | item :: srem ->
      match item.psig_desc with
      | Psig_type (rec_flag, sdecls) ->
          let decls = Typedecl.approx_type_decl sdecls in
          let rem = approx_sig env srem in
          map_rec_type ~rec_flag
            (fun rs (id, info) -> Sig_type(id, info, rs, Exported)) decls rem
      | Psig_typesubst _ -> approx_sig env srem
      | Psig_module { pmd_name = { txt = None; _ }; _ } ->
          approx_sig env srem
      | Psig_module pmd ->
          let scope = Ctype.create_scope () in
          let md = approx_module_declaration env pmd in
          let pres =
            match md.Types.md_type with
            | Mty_alias _ -> Mp_absent
            | _ -> Mp_present
          in
          let id, newenv =
            Env.enter_module_declaration ~scope (Option.get pmd.pmd_name.txt)
              pres md env
          in
          Sig_module(id, pres, md, Trec_not, Exported) :: approx_sig newenv srem
      | Psig_modsubst pms ->
          let scope = Ctype.create_scope () in
          let _, md =
            Env.lookup_module ~use:false ~loc:pms.pms_manifest.loc
               pms.pms_manifest.txt env
          in
          let pres =
            match md.Types.md_type with
            | Mty_alias _ -> Mp_absent
            | _ -> Mp_present
          in
          let _, newenv =
            Env.enter_module_declaration ~scope pms.pms_name.txt pres md env
          in
          approx_sig newenv srem
      | Psig_recmodule sdecls ->
          let scope = Ctype.create_scope () in
          let decls =
            List.filter_map
              (fun pmd ->
                 Option.map (fun name ->
                   Ident.create_scoped ~scope name,
                   approx_module_declaration env pmd
                 ) pmd.pmd_name.txt
              )
              sdecls
          in
          let newenv =
            List.fold_left
              (fun env (id, md) -> Env.add_module_declaration ~check:false
                  id Mp_present md env)
              env decls
          in
          map_rec
            (fun rs (id, md) -> Sig_module(id, Mp_present, md, rs, Exported))
            decls
            (approx_sig newenv srem)
      | Psig_modtype d ->
          let info = approx_modtype_info env d in
          let scope = Ctype.create_scope () in
          let (id, newenv) =
            Env.enter_modtype ~scope d.pmtd_name.txt info env
          in
          Sig_modtype(id, info, Exported) :: approx_sig newenv srem
      | Psig_open sod ->
          let _, env = type_open_descr env sod in
          approx_sig env srem
      | Psig_include sincl ->
          let smty = sincl.pincl_mod in
          let mty = approx_modtype env smty in
          let scope = Ctype.create_scope () in
          let sg, newenv = Env.enter_signature ~scope
              (extract_sig env smty.pmty_loc mty) env in
          sg @ approx_sig newenv srem
      | Psig_class sdecls | Psig_class_type sdecls ->
          let decls = Typeclass.approx_class_declarations env sdecls in
          let rem = approx_sig env srem in
          map_rec (fun rs decl ->
            let open Typeclass in [
              Sig_class_type(decl.clsty_ty_id, decl.clsty_ty_decl, rs,
                             Exported);
              Sig_type(decl.clsty_obj_id, decl.clsty_obj_abbr, rs, Exported);
              Sig_type(decl.clsty_typesharp_id, decl.clsty_abbr, rs, Exported);
            ]
          ) decls [rem]
          |> List.flatten
      | _ ->
          approx_sig env srem

and approx_modtype_info env sinfo =
  {
   mtd_type = Option.map (approx_modtype env) sinfo.pmtd_type;
   mtd_attributes = sinfo.pmtd_attributes;
   mtd_loc = sinfo.pmtd_loc;
   mtd_uid = Uid.internal_not_actually_unique;
  }

let approx_modtype env smty =
  Warnings.without_warnings
    (fun () -> approx_modtype env smty)

(* Auxiliaries for checking the validity of name shadowing in signatures and
   structures.
   If a shadowing is valid, we also record some information (its ident,
   location where it first appears, etc) about the item that gets shadowed. *)
module Signature_names : sig
  type t

  type info = [
    | `Exported
    | `From_open
    | `Shadowable of Ident.t * Location.t
    | `Substituted_away of Subst.t
  ]

  val create : unit -> t

  val check_value     : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_type      : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_typext    : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_module    : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_modtype   : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_class     : ?info:info -> t -> Location.t -> Ident.t -> unit
  val check_class_type: ?info:info -> t -> Location.t -> Ident.t -> unit

  val check_sig_item:
    ?info:info -> t -> Location.t -> Types.signature_item -> unit

  val simplify: Env.t -> t -> Types.signature -> Types.signature
end = struct

  type bound_info = [
    | `Exported
    | `Shadowable of Ident.t * Location.t
  ]

  type info = [
    | `From_open
    | `Substituted_away of Subst.t
    | bound_info
  ]

  type hide_reason =
    | From_open
    | Shadowed_by of Ident.t * Location.t

  type to_be_removed = {
    mutable subst: Subst.t;
    mutable hide: (Sig_component_kind.t * Location.t * hide_reason) Ident.Map.t;
  }

  type names_infos = (string, bound_info) Hashtbl.t

  type names = {
    values: names_infos;
    types: names_infos;
    modules: names_infos;
    modtypes: names_infos;
    typexts: names_infos;
    classes: names_infos;
    class_types: names_infos;
  }

  let new_names () = {
    values = Hashtbl.create 16;
    types = Hashtbl.create 16;
    modules = Hashtbl.create 16;
    modtypes = Hashtbl.create 16;
    typexts = Hashtbl.create 16;
    classes = Hashtbl.create 16;
    class_types = Hashtbl.create 16;
  }

  type t = {
    bound: names;
    to_be_removed: to_be_removed;
  }

  let create () = {
    bound = new_names ();
    to_be_removed = {
      subst = Subst.identity;
      hide = Ident.Map.empty;
    };
  }

  let check cl loc (tbl : names_infos) id (info : info) to_be_removed =
    match info with
    | `Substituted_away s ->
        to_be_removed.subst <- Subst.compose s to_be_removed.subst
    | `From_open ->
        to_be_removed.hide <-
          Ident.Map.add id (cl, loc, From_open) to_be_removed.hide
    | #bound_info as bound_info ->
        let name = Ident.name id in
        match Hashtbl.find_opt tbl name with
        | None -> Hashtbl.add tbl name bound_info
        | Some (`Shadowable (shadowed_id, shadowed_loc)) ->
            Hashtbl.replace tbl name bound_info;
            let reason = Shadowed_by (id, loc) in
            to_be_removed.hide <-
              Ident.Map.add shadowed_id (cl, shadowed_loc, reason)
                to_be_removed.hide
        | Some `Exported ->
            raise(Error(loc, Env.empty, Repeated_name(cl, name)))

  let check_value ?info t loc id =
    let info =
      match info with
      | Some i -> i
      | None -> `Shadowable (id, loc)
    in
    check Sig_component_kind.Value loc t.bound.values id info t.to_be_removed
  let check_type ?(info=`Exported) t loc id =
    check Sig_component_kind.Type loc t.bound.types id info t.to_be_removed
  let check_module ?(info=`Exported) t loc id =
    check Sig_component_kind.Module loc t.bound.modules id info t.to_be_removed
  let check_modtype ?(info=`Exported) t loc id =
    check Sig_component_kind.Module_type loc t.bound.modtypes id info
      t.to_be_removed
  let check_typext ?(info=`Exported) t loc id =
    check Sig_component_kind.Extension_constructor loc t.bound.typexts id info
      t.to_be_removed
  let check_class ?(info=`Exported) t loc id =
    check Sig_component_kind.Class loc t.bound.classes id info t.to_be_removed
  let check_class_type ?(info=`Exported) t loc id =
    check Sig_component_kind.Class_type loc t.bound.class_types id info
      t.to_be_removed

  let check_sig_item ?info names loc component =
    let info id loc =
      match info with
      | None -> `Shadowable (id, loc)
      | Some i -> i
    in
    match component with
    | Sig_type(id, _, _, _) ->
        check_type names loc id ~info:(info id loc)
    | Sig_module(id, _, _, _, _) ->
        check_module names loc id ~info:(info id loc)
    | Sig_modtype(id, _, _) ->
        check_modtype names loc id ~info:(info id loc)
    | Sig_typext(id, _, _, _) ->
        check_typext names loc id ~info:(info id loc)
    | Sig_value (id, _, _) ->
        check_value names loc id ~info:(info id loc)
    | Sig_class (id, _, _, _) ->
        check_class names loc id ~info:(info id loc)
    | Sig_class_type (id, _, _, _) ->
        check_class_type names loc id ~info:(info id loc)

  (* We usually require name uniqueness of signature components (e.g. types,
     modules, etc), however in some situation reusing the name is allowed: if
     the component is a value or an extension, or if the name is introduced by
     an include.
     When there are multiple specifications of a component with the same name,
     we try to keep only the last (rightmost) one, removing all references to
     the previous ones from the signature.
     If some reference cannot be removed, then we error out with
     [Cannot_hide_id].
  *)

  let simplify env t sg =
    let to_remove = t.to_be_removed in
    let ids_to_remove =
      Ident.Map.fold (fun id (kind,  _, _) lst ->
        if Sig_component_kind.can_appear_in_types kind then
          id :: lst
        else
          lst
      ) to_remove.hide []
    in
    let aux component sg =
      let user_kind, user_id, user_loc =
        let open Sig_component_kind in
        match component with
        | Sig_value(id, v, _) -> Value, id, v.val_loc
        | Sig_type (id, td, _, _) -> Type, id, td.type_loc
        | Sig_typext (id, te, _, _) -> Extension_constructor, id, te.ext_loc
        | Sig_module (id, _, md, _, _) -> Module, id, md.md_loc
        | Sig_modtype (id, mtd, _) -> Module_type, id, mtd.mtd_loc
        | Sig_class (id, c, _, _) -> Class, id, c.cty_loc
        | Sig_class_type (id, ct, _, _) -> Class_type, id, ct.clty_loc
      in
      if Ident.Map.mem user_id to_remove.hide then
        sg
      else begin
        let component =
          if to_remove.subst == Subst.identity then
            component
          else
            Subst.signature_item Keep to_remove.subst component
        in
        let component =
          match ids_to_remove with
          | [] -> component
          | ids ->
            try Mtype.nondep_sig_item env ids component with
            | Ctype.Nondep_cannot_erase removed_item_id ->
              let (removed_item_kind, removed_item_loc, reason) =
                Ident.Map.find removed_item_id to_remove.hide
              in
              let err_loc, hiding_error =
                match reason with
                | From_open ->
                  removed_item_loc,
                  Appears_in_signature {
                    opened_item_kind = removed_item_kind;
                    opened_item_id = removed_item_id;
                    user_id;
                    user_kind;
                    user_loc;
                  }
                | Shadowed_by (shadower_id, shadower_loc) ->
                  shadower_loc,
                  Illegal_shadowing {
                    shadowed_item_kind = removed_item_kind;
                    shadowed_item_id = removed_item_id;
                    shadowed_item_loc = removed_item_loc;
                    shadower_id;
                    user_id;
                    user_kind;
                    user_loc;
                  }
              in
              raise (Error(err_loc, env, Cannot_hide_id hiding_error))
        in
        component :: sg
      end
    in
    List.fold_right aux sg []
end

let has_remove_aliases_attribute attr =
  let remove_aliases =
    Attr_helper.get_no_payload_attribute
      ["remove_aliases"; "ocaml.remove_aliases"] attr
  in
  match remove_aliases with
  | None -> false
  | Some _ -> true

(* Check and translate a module type expression *)

let transl_modtype_longident loc env lid =
  let (path, _info) = Env.lookup_modtype ~loc lid env in
  path

let transl_module_alias loc env lid =
  Env.lookup_module_path ~load:false ~loc lid env

let mkmty desc typ env loc attrs =
  let mty = {
    mty_desc = desc;
    mty_type = typ;
    mty_loc = loc;
    mty_env = env;
    mty_attributes = attrs;
    } in
  Cmt_format.add_saved_type (Cmt_format.Partial_module_type mty);
  mty

let mksig desc env loc =
  let sg = { sig_desc = desc; sig_loc = loc; sig_env = env } in
  Cmt_format.add_saved_type (Cmt_format.Partial_signature_item sg);
  sg

(* let signature sg = List.map (fun item -> item.sig_type) sg *)

let rec transl_modtype env smty =
  Builtin_attributes.warning_scope smty.pmty_attributes
    (fun () -> transl_modtype_aux env smty)

and transl_modtype_functor_arg env sarg =
  let mty = transl_modtype env sarg in
  {mty with mty_type = Mtype.scrape_for_functor_arg env mty.mty_type}

and transl_modtype_aux env smty =
  let loc = smty.pmty_loc in
  match smty.pmty_desc with
    Pmty_ident lid ->
      let path = transl_modtype_longident loc env lid.txt in
      mkmty (Tmty_ident (path, lid)) (Mty_ident path) env loc
        smty.pmty_attributes
  | Pmty_alias lid ->
      let path = transl_module_alias loc env lid.txt in
      mkmty (Tmty_alias (path, lid)) (Mty_alias path) env loc
        smty.pmty_attributes
  | Pmty_signature ssg ->
      let sg = transl_signature env ssg in
      mkmty (Tmty_signature sg) (Mty_signature sg.sig_type) env loc
        smty.pmty_attributes
  | Pmty_functor(sarg_opt, sres) ->
      let t_arg, ty_arg, newenv =
        match sarg_opt with
        | Unit -> Unit, Types.Unit, env
        | Named (param, sarg) ->
          let arg = transl_modtype_functor_arg env sarg in
          let (id, newenv) =
            match param.txt with
            | None -> None, env
            | Some name ->
              let scope = Ctype.create_scope () in
              let id, newenv =
                let arg_md =
                  { md_type = arg.mty_type;
                    md_attributes = [];
                    md_loc = param.loc;
                    md_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
                  }
                in
                Env.enter_module_declaration ~scope ~arg:true name Mp_present
                  arg_md env
              in
              Some id, newenv
          in
          Named (id, param, arg), Types.Named (id, arg.mty_type), newenv
      in
      let res = transl_modtype newenv sres in
      mkmty (Tmty_functor (t_arg, res))
        (Mty_functor(ty_arg, res.mty_type)) env loc
        smty.pmty_attributes
  | Pmty_with(sbody, constraints) ->
      let body = transl_modtype env sbody in
      let init_sg = extract_sig env sbody.pmty_loc body.mty_type in
      let remove_aliases = has_remove_aliases_attribute smty.pmty_attributes in
      let (rev_tcstrs, final_sg) =
        List.fold_left
          (fun (rev_tcstrs,sg) sdecl ->
            let (tcstr, sg) =
              merge_constraint env remove_aliases smty.pmty_loc sg sdecl
            in
            (tcstr :: rev_tcstrs, sg)
        )
        ([],init_sg) constraints in
      let scope = Ctype.create_scope () in
      mkmty (Tmty_with ( body, List.rev rev_tcstrs))
        (Mtype.freshen ~scope (Mty_signature final_sg)) env loc
        smty.pmty_attributes
  | Pmty_typeof smod ->
      let env = Env.in_signature false env in
      let tmty, mty = !type_module_type_of_fwd env smod in
      mkmty (Tmty_typeof tmty) mty env loc smty.pmty_attributes
  | Pmty_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

and transl_signature env sg =
  let names = Signature_names.create () in
  let rec transl_sig env sg =
    match sg with
      [] -> [], [], env
    | item :: srem ->
        let loc = item.psig_loc in
        match item.psig_desc with
        | Psig_value sdesc ->
            let (tdesc, newenv) =
              Typedecl.transl_value_decl env item.psig_loc sdesc
            in
            Signature_names.check_value names tdesc.val_loc tdesc.val_id;
            let (trem,rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_value tdesc) env loc :: trem,
            Sig_value(tdesc.val_id, tdesc.val_val, Exported) :: rem,
              final_env
        | Psig_type (rec_flag, sdecls) ->
            let (decls, newenv) =
              Typedecl.transl_type_decl env rec_flag sdecls
            in
            List.iter (fun td ->
              Signature_names.check_type names td.typ_loc td.typ_id
            ) decls;
            let (trem, rem, final_env) = transl_sig newenv srem in
            let sg =
              map_rec_type_with_row_types ~rec_flag
                (fun rs td -> Sig_type(td.typ_id, td.typ_type, rs, Exported))
                decls rem
            in
            mksig (Tsig_type (rec_flag, decls)) env loc :: trem,
            sg,
            final_env
        | Psig_typesubst sdecls ->
            let (decls, newenv) =
              Typedecl.transl_type_decl env Nonrecursive sdecls
            in
            List.iter (fun td ->
              if td.typ_kind <> Ttype_abstract || td.typ_manifest = None ||
                 td.typ_private = Private
              then
                raise (Error (td.typ_loc, env, Invalid_type_subst_rhs));
              let params = td.typ_type.type_params in
              if params_are_constrained params
              then raise(Error(loc, env, With_cannot_remove_constrained_type));
              let info =
                  let subst =
                    Subst.add_type_function (Pident td.typ_id)
                      ~params
                      ~body:(Option.get td.typ_type.type_manifest)
                      Subst.identity
                  in
                  Some (`Substituted_away subst)
              in
              Signature_names.check_type ?info names td.typ_loc td.typ_id
            ) decls;
            let (trem, rem, final_env) = transl_sig newenv srem in
            let sg = rem
            in
            mksig (Tsig_typesubst decls) env loc :: trem,
            sg,
            final_env
        | Psig_typext styext ->
            let (tyext, newenv) =
              Typedecl.transl_type_extension false env item.psig_loc styext
            in
            let constructors = tyext.tyext_constructors in
            List.iter (fun ext ->
              Signature_names.check_typext names ext.ext_loc ext.ext_id
            ) constructors;
            let (trem, rem, final_env) = transl_sig newenv srem in
              mksig (Tsig_typext tyext) env loc :: trem,
              map_ext (fun es ext ->
                Sig_typext(ext.ext_id, ext.ext_type, es, Exported)
              ) constructors rem,
              final_env
        | Psig_exception sext ->
            let (ext, newenv) = Typedecl.transl_type_exception env sext in
            let constructor = ext.tyexn_constructor in
            Signature_names.check_typext names constructor.ext_loc
              constructor.ext_id;
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_exception ext) env loc :: trem,
            Sig_typext(constructor.ext_id,
                       constructor.ext_type,
                       Text_exception,
                       Exported) :: rem,
            final_env
        | Psig_module pmd ->
            let scope = Ctype.create_scope () in
            let tmty =
              Builtin_attributes.warning_scope pmd.pmd_attributes
                (fun () -> transl_modtype env pmd.pmd_type)
            in
            let pres =
              match tmty.mty_type with
              | Mty_alias _ -> Mp_absent
              | _ -> Mp_present
            in
            let md = {
              md_type=tmty.mty_type;
              md_attributes=pmd.pmd_attributes;
              md_loc=pmd.pmd_loc;
              md_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
            }
            in
            let id, newenv =
              match pmd.pmd_name.txt with
              | None -> None, env
              | Some name ->
                let id, newenv =
                  Env.enter_module_declaration ~scope name pres md env
                in
                Signature_names.check_module names pmd.pmd_name.loc id;
                Some id, newenv
            in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_module {md_id=id; md_name=pmd.pmd_name;
                                md_presence=pres; md_type=tmty;
                                md_loc=pmd.pmd_loc;
                                md_attributes=pmd.pmd_attributes})
              env loc :: trem,
            (match id with
             | None -> rem
             | Some id -> Sig_module(id, pres, md, Trec_not, Exported) :: rem),
            final_env
        | Psig_modsubst pms ->
            let scope = Ctype.create_scope () in
            let path, md =
              Env.lookup_module ~loc:pms.pms_manifest.loc
                pms.pms_manifest.txt env
            in
            let aliasable = not (Env.is_functor_arg path env) in
            let md =
              if not aliasable then
                md
              else
                { md_type = Mty_alias path;
                  md_attributes = pms.pms_attributes;
                  md_loc = pms.pms_loc;
                  md_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
                }
            in
            let pres =
              match md.md_type with
              | Mty_alias _ -> Mp_absent
              | _ -> Mp_present
            in
            let id, newenv =
              Env.enter_module_declaration ~scope pms.pms_name.txt pres md env
            in
            let info =
              `Substituted_away (Subst.add_module id path Subst.identity)
            in
            Signature_names.check_module ~info names pms.pms_name.loc id;
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_modsubst {ms_id=id; ms_name=pms.pms_name;
                                  ms_manifest=path; ms_txt=pms.pms_manifest;
                                  ms_loc=pms.pms_loc;
                                  ms_attributes=pms.pms_attributes})
              env loc :: trem,
            rem,
            final_env
        | Psig_recmodule sdecls ->
            let (tdecls, newenv) =
              transl_recmodule_modtypes env sdecls in
            let decls =
              List.filter_map (fun (md, uid) ->
                match md.md_id with
                | None -> None
                | Some id -> Some (id, md, uid)
              ) tdecls
            in
            List.iter (fun (id, md, _) ->
              Signature_names.check_module names md.md_loc id
            ) decls;
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_recmodule (List.map fst tdecls)) env loc :: trem,
            map_rec (fun rs (id, md, uid) ->
                let d = {Types.md_type = md.md_type.mty_type;
                         md_attributes = md.md_attributes;
                         md_loc = md.md_loc;
                         md_uid = uid;
                        } in
                Sig_module(id, Mp_present, d, rs, Exported))
              decls rem,
            final_env
        | Psig_modtype pmtd ->
            let newenv, mtd, sg = transl_modtype_decl names env pmtd in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_modtype mtd) env loc :: trem,
            sg :: rem,
            final_env
        | Psig_open sod ->
            let (od, newenv) = type_open_descr env sod in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_open od) env loc :: trem,
            rem, final_env
        | Psig_include sincl ->
            let smty = sincl.pincl_mod in
            let tmty =
              Builtin_attributes.warning_scope sincl.pincl_attributes
                (fun () -> transl_modtype env smty)
            in
            let mty = tmty.mty_type in
            let scope = Ctype.create_scope () in
            let sg, newenv = Env.enter_signature ~scope
                       (extract_sig env smty.pmty_loc mty) env in
            List.iter (Signature_names.check_sig_item names item.psig_loc) sg;
            let incl =
              { incl_mod = tmty;
                incl_type = sg;
                incl_attributes = sincl.pincl_attributes;
                incl_loc = sincl.pincl_loc;
              }
            in
            let (trem, rem, final_env) = transl_sig newenv srem  in
            mksig (Tsig_include incl) env loc :: trem,
            sg @ rem,
            final_env
        | Psig_class cl ->
            let (classes, newenv) = Typeclass.class_descriptions env cl in
            List.iter (fun cls ->
              let open Typeclass in
              let loc = cls.cls_id_loc.Location.loc in
              Signature_names.check_type names loc cls.cls_obj_id;
              Signature_names.check_class names loc cls.cls_id;
              Signature_names.check_class_type names loc cls.cls_ty_id;
              Signature_names.check_type names loc cls.cls_typesharp_id;
            ) classes;
            let (trem, rem, final_env) = transl_sig newenv srem in
            let sg =
              map_rec (fun rs cls ->
                let open Typeclass in
                [Sig_class(cls.cls_id, cls.cls_decl, rs, Exported);
                 Sig_class_type(cls.cls_ty_id, cls.cls_ty_decl, rs, Exported);
                 Sig_type(cls.cls_obj_id, cls.cls_obj_abbr, rs, Exported);
                 Sig_type(cls.cls_typesharp_id, cls.cls_abbr, rs, Exported)]
              ) classes [rem]
              |> List.flatten
            in
            let typedtree =
              mksig (Tsig_class
                       (List.map (fun decr ->
                          decr.Typeclass.cls_info) classes)) env loc
              :: trem
            in
            typedtree, sg, final_env
        | Psig_class_type cl ->
            let (classes, newenv) = Typeclass.class_type_declarations env cl in
            List.iter (fun decl ->
              let open Typeclass in
              let loc = decl.clsty_id_loc.Location.loc in
              Signature_names.check_class_type names loc decl.clsty_ty_id;
              Signature_names.check_type names loc decl.clsty_obj_id;
              Signature_names.check_type names loc decl.clsty_typesharp_id;
            ) classes;
            let (trem,rem, final_env) = transl_sig newenv srem in
            let sg =
              map_rec (fun rs decl ->
                let open Typeclass in
                [Sig_class_type(decl.clsty_ty_id, decl.clsty_ty_decl, rs,
                                Exported);
                 Sig_type(decl.clsty_obj_id, decl.clsty_obj_abbr, rs, Exported);
                 Sig_type(decl.clsty_typesharp_id, decl.clsty_abbr, rs,
                          Exported)
                ]
              ) classes [rem]
              |> List.flatten
            in
            let typedtree =
              mksig
                (Tsig_class_type
                   (List.map (fun decl -> decl.Typeclass.clsty_info) classes))
                env loc
              :: trem
            in
            typedtree, sg, final_env
        | Psig_attribute x ->
            Builtin_attributes.warning_attribute x;
            let (trem,rem, final_env) = transl_sig env srem in
            mksig (Tsig_attribute x) env loc :: trem, rem, final_env
        | Psig_extension (ext, _attrs) ->
            raise (Error_forward (Builtin_attributes.error_of_extension ext))
  in
  let previous_saved_types = Cmt_format.get_saved_types () in
  Builtin_attributes.warning_scope []
    (fun () ->
       let (trem, rem, final_env) = transl_sig (Env.in_signature true env) sg in
       let rem = Signature_names.simplify final_env names rem in
       let sg =
         { sig_items = trem; sig_type = rem; sig_final_env = final_env }
       in
       Cmt_format.set_saved_types
         ((Cmt_format.Partial_signature sg) :: previous_saved_types);
       sg
    )

and transl_modtype_decl names env pmtd =
  Builtin_attributes.warning_scope pmtd.pmtd_attributes
    (fun () -> transl_modtype_decl_aux names env pmtd)

and transl_modtype_decl_aux names env
    {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} =
  let tmty =
    Option.map (transl_modtype (Env.in_signature true env)) pmtd_type
  in
  let decl =
    {
     Types.mtd_type=Option.map (fun t -> t.mty_type) tmty;
     mtd_attributes=pmtd_attributes;
     mtd_loc=pmtd_loc;
     mtd_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
    }
  in
  let scope = Ctype.create_scope () in
  let (id, newenv) = Env.enter_modtype ~scope pmtd_name.txt decl env in
  Signature_names.check_modtype names pmtd_loc id;
  let mtd =
    {
     mtd_id=id;
     mtd_name=pmtd_name;
     mtd_type=tmty;
     mtd_attributes=pmtd_attributes;
     mtd_loc=pmtd_loc;
    }
  in
  newenv, mtd, Sig_modtype(id, decl, Exported)

and transl_recmodule_modtypes env sdecls =
  let make_env curr =
    List.fold_left
      (fun env (id, _, md, _) ->
         Option.fold ~none:env
           ~some:(fun id -> Env.add_module_declaration ~check:true ~arg:true
                              id Mp_present md env) id)
      env curr in
  let transition env_c curr =
    List.map2
      (fun pmd (id, id_loc, md, _) ->
        let tmty =
          Builtin_attributes.warning_scope pmd.pmd_attributes
            (fun () -> transl_modtype env_c pmd.pmd_type)
        in
        let md = { md with Types.md_type = tmty.mty_type } in
        (id, id_loc, md, tmty))
      sdecls curr in
  let map_mtys curr =
    List.filter_map
      (fun (id, _, md, _) -> Option.map (fun id -> (id, md)) id)
      curr
  in
  let scope = Ctype.create_scope () in
  let ids =
    List.map (fun x -> Option.map (Ident.create_scoped ~scope) x.pmd_name.txt)
      sdecls
  in
  let approx_env =
    List.fold_left
      (fun env ->
         Option.fold ~none:env ~some:(fun id -> (* cf #5965 *)
           Env.enter_unbound_module (Ident.name id)
             Mod_unbound_illegal_recursion env
         ))
      env ids
  in
  let init =
    List.map2
      (fun id pmd ->
         let md =
           { md_type = approx_modtype approx_env pmd.pmd_type;
             md_loc = pmd.pmd_loc;
             md_attributes = pmd.pmd_attributes;
             md_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) }
         in
        (id, pmd.pmd_name, md, ()))
      ids sdecls
  in
  let env0 = make_env init in
  let dcl1 =
    Warnings.without_warnings
      (fun () -> transition env0 init)
  in
  let env1 = make_env dcl1 in
  check_recmod_typedecls env1 (map_mtys dcl1);
  let dcl2 = transition env1 dcl1 in
(*
  List.iter
    (fun (id, mty) ->
      Format.printf "%a: %a@." Printtyp.ident id Printtyp.modtype mty)
    dcl2;
*)
  let env2 = make_env dcl2 in
  check_recmod_typedecls env2 (map_mtys dcl2);
  let dcl2 =
    List.map2 (fun pmd (id, id_loc, md, mty) ->
      let tmd =
        {md_id=id; md_name=id_loc; md_type=mty;
         md_presence=Mp_present;
         md_loc=pmd.pmd_loc;
         md_attributes=pmd.pmd_attributes}
      in
      tmd, md.md_uid
    ) sdecls dcl2
  in
  (dcl2, env2)

(* Try to convert a module expression to a module path. *)

exception Not_a_path

let rec path_of_module mexp =
  match mexp.mod_desc with
  | Tmod_ident (p,_) -> p
  | Tmod_apply(funct, arg, _coercion) when !Clflags.applicative_functors ->
      Papply(path_of_module funct, path_of_module arg)
  | Tmod_constraint (mexp, _, _, _) ->
      path_of_module mexp
  | _ -> raise Not_a_path

let path_of_module mexp =
 try Some (path_of_module mexp) with Not_a_path -> None

(* Check that all core type schemes in a structure are closed *)

let rec closed_modtype env = function
    Mty_ident _ -> true
  | Mty_alias _ -> true
  | Mty_signature sg ->
      let env = Env.add_signature sg env in
      List.for_all (closed_signature_item env) sg
  | Mty_functor(arg_opt, body) ->
      let env =
        match arg_opt with
        | Unit
        | Named (None, _) -> env
        | Named (Some id, param) ->
            Env.add_module ~arg:true id Mp_present param env
      in
      closed_modtype env body

and closed_signature_item env = function
    Sig_value(_id, desc, _) -> Ctype.closed_schema env desc.val_type
  | Sig_module(_id, _, md, _, _) -> closed_modtype env md.md_type
  | _ -> true

let check_nongen_scheme env sig_item =
  match sig_item with
    Sig_value(_id, vd, _) ->
      if not (Ctype.closed_schema env vd.val_type) then
        raise (Error (vd.val_loc, env, Non_generalizable vd.val_type))
  | Sig_module (_id, _, md, _, _) ->
      if not (closed_modtype env md.md_type) then
        raise(Error(md.md_loc, env, Non_generalizable_module md.md_type))
  | _ -> ()

let check_nongen_schemes env sg =
  List.iter (check_nongen_scheme env) sg

(* Helpers for typing recursive modules *)

let anchor_submodule name anchor =
  match anchor, name with
  | None, _
  | _, None ->
      None
  | Some p, Some name ->
      Some(Pdot(p, name))

let anchor_recmodule = Option.map (fun id -> Pident id)

let enrich_type_decls anchor decls oldenv newenv =
  match anchor with
    None -> newenv
  | Some p ->
      List.fold_left
        (fun e info ->
          let id = info.typ_id in
          let info' =
            Mtype.enrich_typedecl oldenv (Pdot(p, Ident.name id))
              id info.typ_type
          in
            Env.add_type ~check:true id info' e)
        oldenv decls

let enrich_module_type anchor name mty env =
  match anchor, name with
  | None, _
  | _, None ->
      mty
  | Some p, Some name ->
      Mtype.enrich_modtype env (Pdot(p, name)) mty

let check_recmodule_inclusion env bindings =
  (* PR#4450, PR#4470: consider
        module rec X : DECL = MOD  where MOD has inferred type ACTUAL
     The "natural" typing condition
        E, X: ACTUAL |- ACTUAL <: DECL
     leads to circularities through manifest types.
     Instead, we "unroll away" the potential circularities a finite number
     of times.  The (weaker) condition we implement is:
        E, X: DECL,
           X1: ACTUAL,
           X2: ACTUAL{X <- X1}/X1
           ...
           Xn: ACTUAL{X <- X(n-1)}/X(n-1)
        |- ACTUAL{X <- Xn}/Xn <: DECL{X <- Xn}
     so that manifest types rooted at X(n+1) are expanded in terms of X(n),
     avoiding circularities.  The strengthenings ensure that
     Xn.t = X(n-1).t = ... = X2.t = X1.t.
     N can be chosen arbitrarily; larger values of N result in more
     recursive definitions being accepted.  A good choice appears to be
     the number of mutually recursive declarations. *)

  let subst_and_strengthen env scope s id mty =
    let mty = Subst.modtype (Rescope scope) s mty in
    match id with
    | None -> mty
    | Some id ->
        Mtype.strengthen ~aliasable:false env mty
          (Subst.module_path s (Pident id))
  in

  let rec check_incl first_time n env s =
    let scope = Ctype.create_scope () in
    if n > 0 then begin
      (* Generate fresh names Y_i for the rec. bound module idents X_i *)
      let bindings1 =
        List.map
          (fun (id, _name, _mty_decl, _modl, mty_actual, _attrs, _loc, _uid) ->
             let ids =
               Option.map
                 (fun id -> (id, Ident.create_scoped ~scope (Ident.name id))) id
             in
             (ids, mty_actual))
          bindings in
      (* Enter the Y_i in the environment with their actual types substituted
         by the input substitution s *)
      let env' =
        List.fold_left
          (fun env (ids, mty_actual) ->
             match ids with
             | None -> env
             | Some (id, id') ->
               let mty_actual' =
                 if first_time
                 then mty_actual
                 else subst_and_strengthen env scope s (Some id) mty_actual
               in
               Env.add_module ~arg:false id' Mp_present mty_actual' env)
          env bindings1 in
      (* Build the output substitution Y_i <- X_i *)
      let s' =
        List.fold_left
          (fun s (ids, _mty_actual) ->
             match ids with
             | None -> s
             | Some (id, id') -> Subst.add_module id (Pident id') s)
          Subst.identity bindings1 in
      (* Recurse with env' and s' *)
      check_incl false (n-1) env' s'
    end else begin
      (* Base case: check inclusion of s(mty_actual) in s(mty_decl)
         and insert coercion if needed *)
      let check_inclusion
            (id, name, mty_decl, modl, mty_actual, attrs, loc, uid) =
        let mty_decl' = Subst.modtype (Rescope scope) s mty_decl.mty_type
        and mty_actual' = subst_and_strengthen env scope s id mty_actual in
        let coercion =
          try
            Includemod.modtypes ~loc:modl.mod_loc ~mark:Mark_both env
              mty_actual' mty_decl'
          with Includemod.Error msg ->
            raise(Error(modl.mod_loc, env, Not_included msg)) in
        let modl' =
            { mod_desc = Tmod_constraint(modl, mty_decl.mty_type,
                Tmodtype_explicit mty_decl, coercion);
              mod_type = mty_decl.mty_type;
              mod_env = env;
              mod_loc = modl.mod_loc;
              mod_attributes = [];
             } in
        let mb =
          {
            mb_id = id;
            mb_name = name;
            mb_presence = Mp_present;
            mb_expr = modl';
            mb_attributes = attrs;
            mb_loc = loc;
          }
        in
        mb, uid
      in
      List.map check_inclusion bindings
    end
  in check_incl true (List.length bindings) env Subst.identity

(* Helper for unpack *)

let rec package_constraints_sig env loc sg constrs =
  List.map
    (function
      | Sig_type (id, ({type_params=[]} as td), rs, priv)
        when List.mem_assoc [Ident.name id] constrs ->
          let ty = List.assoc [Ident.name id] constrs in
          Sig_type (id, {td with type_manifest = Some ty}, rs, priv)
      | Sig_module (id, pres, md, rs, priv) ->
          let rec aux = function
            | (m :: ((_ :: _) as l), t) :: rest when m = Ident.name id ->
                (l, t) :: aux rest
            | _ :: rest -> aux rest
            | [] -> []
          in
          let md =
            {md with
             md_type = package_constraints env loc md.md_type (aux constrs)
            }
          in
          Sig_module (id, pres, md, rs, priv)
      | item -> item
    )
    sg

and package_constraints env loc mty constrs =
  if constrs = [] then mty
  else begin
    match Mtype.scrape env mty with
    | Mty_signature sg ->
        Mty_signature (package_constraints_sig env loc sg constrs)
    | Mty_functor _ | Mty_alias _ -> assert false
    | Mty_ident p -> raise(Error(loc, env, Cannot_scrape_package_type p))
  end

let modtype_of_package env loc p nl tl =
  package_constraints env loc (Mty_ident p)
    (List.combine (List.map Longident.flatten nl) tl)

let package_subtype env p1 nl1 tl1 p2 nl2 tl2 =
  let mkmty p nl tl =
    let ntl =
      List.filter (fun (_n,t) -> Ctype.free_variables t = [])
        (List.combine nl tl) in
    let (nl, tl) = List.split ntl in
    modtype_of_package env Location.none p nl tl
  in
  match mkmty p1 nl1 tl1, mkmty p2 nl2 tl2 with
  | exception Error(_, _, Cannot_scrape_package_type _) -> false
  | mty1, mty2 ->
    let loc = Location.none in
    match Includemod.modtypes ~loc ~mark:Mark_both env mty1 mty2 with
    | Tcoerce_none -> true
    | _ | exception Includemod.Error _ -> false

let () = Ctype.package_subtype := package_subtype

let wrap_constraint env mark arg mty explicit =
  let mark = if mark then Includemod.Mark_both else Includemod.Mark_neither in
  let coercion =
    try
      Includemod.modtypes ~loc:arg.mod_loc env ~mark arg.mod_type mty
    with Includemod.Error msg ->
      raise(Error(arg.mod_loc, env, Not_included msg)) in
  { mod_desc = Tmod_constraint(arg, mty, explicit, coercion);
    mod_type = mty;
    mod_env = env;
    mod_attributes = [];
    mod_loc = arg.mod_loc }

(* Type a module value expression *)

let rec type_module ?(alias=false) sttn funct_body anchor env smod =
  Builtin_attributes.warning_scope smod.pmod_attributes
    (fun () -> type_module_aux ~alias sttn funct_body anchor env smod)

and type_module_aux ~alias sttn funct_body anchor env smod =
  match smod.pmod_desc with
    Pmod_ident lid ->
      let path =
        Env.lookup_module_path ~load:(not alias) ~loc:smod.pmod_loc lid.txt env
      in
      let md = { mod_desc = Tmod_ident (path, lid);
                 mod_type = Mty_alias path;
                 mod_env = env;
                 mod_attributes = smod.pmod_attributes;
                 mod_loc = smod.pmod_loc } in
      let aliasable = not (Env.is_functor_arg path env) in
      let md =
        if alias && aliasable then
          (Env.add_required_global (Path.head path); md)
        else match (Env.find_module path env).md_type with
        | Mty_alias p1 when not alias ->
            let p1 = Env.normalize_module_path (Some smod.pmod_loc) env p1 in
            let mty = Includemod.expand_module_alias env [] p1 in
            { md with
              mod_desc =
                Tmod_constraint (md, mty, Tmodtype_implicit,
                                 Tcoerce_alias (env, path, Tcoerce_none));
              mod_type =
                if sttn then Mtype.strengthen ~aliasable:true env mty p1
                else mty }
        | mty ->
            let mty =
              if sttn then Mtype.strengthen ~aliasable env mty path
              else mty
            in
            { md with mod_type = mty }
      in md
  | Pmod_structure sstr ->
      let (str, sg, names, _finalenv) =
        type_structure funct_body anchor env sstr in
      let md =
        { mod_desc = Tmod_structure str;
          mod_type = Mty_signature sg;
          mod_env = env;
          mod_attributes = smod.pmod_attributes;
          mod_loc = smod.pmod_loc }
      in
      let sg' = Signature_names.simplify _finalenv names sg in
      if List.length sg' = List.length sg then md else
      wrap_constraint env false md (Mty_signature sg')
        Tmodtype_implicit
  | Pmod_functor(arg_opt, sbody) ->
      let t_arg, ty_arg, newenv, funct_body =
        match arg_opt with
        | Unit -> Unit, Types.Unit, env, false
        | Named (param, smty) ->
          let mty = transl_modtype_functor_arg env smty in
          let scope = Ctype.create_scope () in
          let (id, newenv) =
            match param.txt with
            | None -> None, env
            | Some name ->
              let arg_md =
                { md_type = mty.mty_type;
                  md_attributes = [];
                  md_loc = param.loc;
                  md_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
                }
              in
              let id, newenv =
                Env.enter_module_declaration ~scope ~arg:true name Mp_present
                  arg_md env
              in
              Some id, newenv
          in
          Named (id, param, mty), Types.Named (id, mty.mty_type), newenv, true
      in
      let body = type_module sttn funct_body None newenv sbody in
      { mod_desc = Tmod_functor(t_arg, body);
        mod_type = Mty_functor(ty_arg, body.mod_type);
        mod_env = env;
        mod_attributes = smod.pmod_attributes;
        mod_loc = smod.pmod_loc }
  | Pmod_apply(sfunct, sarg) ->
      let arg = type_module true funct_body None env sarg in
      let path = path_of_module arg in
      let funct =
        type_module (sttn && path <> None) funct_body None env sfunct in
      begin match Env.scrape_alias env funct.mod_type with
      | Mty_functor (Unit, mty_res) ->
          if sarg.pmod_desc <> Pmod_structure [] then
            raise (Error (sfunct.pmod_loc, env, Apply_generative));
          if funct_body && Mtype.contains_type env funct.mod_type then
            raise (Error (smod.pmod_loc, env, Not_allowed_in_functor_body));
          { mod_desc = Tmod_apply(funct, arg, Tcoerce_none);
            mod_type = mty_res;
            mod_env = env;
            mod_attributes = smod.pmod_attributes;
            mod_loc = smod.pmod_loc }
      | Mty_functor (Named (param, mty_param), mty_res) as mty_functor ->
          let coercion =
            try
              Includemod.modtypes ~loc:sarg.pmod_loc ~mark:Mark_both env
                arg.mod_type mty_param
            with Includemod.Error msg ->
              raise(Error(sarg.pmod_loc, env, Not_included msg)) in
          let mty_appl =
            match path with
            | Some path ->
                let scope = Ctype.create_scope () in
                let subst =
                  match param with
                  | None -> Subst.identity
                  | Some p -> Subst.add_module p path Subst.identity
                in
                Subst.modtype (Rescope scope) subst mty_res
            | None ->
                let env, nondep_mty =
                  match param with
                  | None -> env, mty_res
                  | Some param ->
                      let env =
                        Env.add_module ~arg:true param Mp_present arg.mod_type
                          env
                      in
                      check_well_formed_module env smod.pmod_loc
                        "the signature of this functor application" mty_res;
                      try env, Mtype.nondep_supertype env [param] mty_res
                      with Ctype.Nondep_cannot_erase _ ->
                        raise(Error(smod.pmod_loc, env,
                                    Cannot_eliminate_dependency mty_functor))
                in
                begin match
                  Includemod.modtypes ~mark:Mark_neither
                    ~loc:smod.pmod_loc env mty_res nondep_mty
                with
                | Tcoerce_none -> ()
                | _ ->
                  fatal_error
                    "unexpected coercion from original module type to \
                     nondep_supertype one"
                | exception Includemod.Error _ ->
                  fatal_error
                    "nondep_supertype not included in original module type"
                end;
                nondep_mty
          in
          check_well_formed_module env smod.pmod_loc
            "the signature of this functor application" mty_appl;
          { mod_desc = Tmod_apply(funct, arg, coercion);
            mod_type = mty_appl;
            mod_env = env;
            mod_attributes = smod.pmod_attributes;
            mod_loc = smod.pmod_loc }
      | Mty_alias path ->
          raise(Error(sfunct.pmod_loc, env, Cannot_scrape_alias path))
      | _ ->
          raise(Error(sfunct.pmod_loc, env, Cannot_apply funct.mod_type))
      end
  | Pmod_constraint(sarg, smty) ->
      let arg = type_module ~alias true funct_body anchor env sarg in
      let mty = transl_modtype env smty in
      let md =
        wrap_constraint env true arg mty.mty_type (Tmodtype_explicit mty)
      in
      { md with
        mod_loc = smod.pmod_loc;
        mod_attributes = smod.pmod_attributes;
      }

  | Pmod_unpack sexp ->
      if !Clflags.principal then Ctype.begin_def ();
      let exp = Typecore.type_exp env sexp in
      if !Clflags.principal then begin
        Ctype.end_def ();
        Ctype.generalize_structure exp.exp_type
      end;
      let mty =
        match Ctype.expand_head env exp.exp_type with
          {desc = Tpackage (p, nl, tl)} ->
            if List.exists (fun t -> Ctype.free_variables t <> []) tl then
              raise (Error (smod.pmod_loc, env,
                            Incomplete_packed_module exp.exp_type));
            if !Clflags.principal &&
              not (Typecore.generalizable (Btype.generic_level-1) exp.exp_type)
            then
              Location.prerr_warning smod.pmod_loc
                (Warnings.Not_principal "this module unpacking");
            modtype_of_package env smod.pmod_loc p nl tl
        | {desc = Tvar _} ->
            raise (Typecore.Error
                     (smod.pmod_loc, env, Typecore.Cannot_infer_signature))
        | _ ->
            raise (Error(smod.pmod_loc, env, Not_a_packed_module exp.exp_type))
      in
      if funct_body && Mtype.contains_type env mty then
        raise (Error (smod.pmod_loc, env, Not_allowed_in_functor_body));
      { mod_desc = Tmod_unpack(exp, mty);
        mod_type = mty;
        mod_env = env;
        mod_attributes = smod.pmod_attributes;
        mod_loc = smod.pmod_loc }
  | Pmod_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

and type_open_decl ?used_slot ?toplevel funct_body names env sod =
  Builtin_attributes.warning_scope sod.popen_attributes
    (fun () ->
       type_open_decl_aux ?used_slot ?toplevel funct_body names env sod
    )

and type_open_decl_aux ?used_slot ?toplevel funct_body names env od =
  let loc = od.popen_loc in
  match od.popen_expr.pmod_desc with
  | Pmod_ident lid ->
    let path, newenv =
      type_open_ ?used_slot ?toplevel od.popen_override env loc lid
    in
    let md = { mod_desc = Tmod_ident (path, lid);
               mod_type = Mty_alias path;
               mod_env = env;
               mod_attributes = od.popen_expr.pmod_attributes;
               mod_loc = od.popen_expr.pmod_loc }
    in
    let open_descr = {
      open_expr = md;
      open_bound_items = [];
      open_override = od.popen_override;
      open_env = newenv;
      open_loc = loc;
      open_attributes = od.popen_attributes
    } in
    open_descr, [], newenv
  | _ ->
    let md = type_module true funct_body None env od.popen_expr in
    let scope = Ctype.create_scope () in
    let sg, newenv =
      Env.enter_signature ~scope (extract_sig_open env md.mod_loc md.mod_type)
        env
    in
    let info, visibility =
      match toplevel with
      | Some false | None -> Some `From_open, Hidden
      | Some true -> None, Exported
    in
    List.iter (Signature_names.check_sig_item ?info names loc) sg;
    let sg =
      List.map (function
        | Sig_value(id, vd, _) -> Sig_value(id, vd, visibility)
        | Sig_type(id, td, rs, _) -> Sig_type(id, td, rs, visibility)
        | Sig_typext(id, ec, et, _) -> Sig_typext(id, ec, et, visibility)
        | Sig_module(id, mp, md, rs, _) ->
            Sig_module(id, mp, md, rs, visibility)
        | Sig_modtype(id, mtd, _) -> Sig_modtype(id, mtd, visibility)
        | Sig_class(id, cd, rs, _) -> Sig_class(id, cd, rs, visibility)
        | Sig_class_type(id, ctd, rs, _) ->
            Sig_class_type(id, ctd, rs, visibility)
      ) sg
    in
    let open_descr = {
      open_expr = md;
      open_bound_items = sg;
      open_override = od.popen_override;
      open_env = newenv;
      open_loc = loc;
      open_attributes = od.popen_attributes
    } in
    open_descr, sg, newenv

and type_structure ?(toplevel = false) funct_body anchor env sstr =
  let names = Signature_names.create () in

  let type_str_item env {pstr_loc = loc; pstr_desc = desc} =
    match desc with
    | Pstr_eval (sexpr, attrs) ->
        let expr =
          Builtin_attributes.warning_scope attrs
            (fun () -> Typecore.type_expression env sexpr)
        in
        Tstr_eval (expr, attrs), [], env
    | Pstr_value(rec_flag, sdefs) ->
        let (defs, newenv) =
          Typecore.type_binding env rec_flag sdefs in
        let () = if rec_flag = Recursive then
          Typecore.check_recursive_bindings env defs
        in
        (* Note: Env.find_value does not trigger the value_used event. Values
           will be marked as being used during the signature inclusion test. *)
        Tstr_value(rec_flag, defs),
        List.map (fun (id, { Asttypes.loc; _ }, _typ)->
          Signature_names.check_value names loc id;
          Sig_value(id, Env.find_value (Pident id) newenv, Exported)
        ) (let_bound_idents_full defs),
        newenv
    | Pstr_primitive sdesc ->
        let (desc, newenv) = Typedecl.transl_value_decl env loc sdesc in
        Signature_names.check_value names desc.val_loc desc.val_id;
        Tstr_primitive desc,
        [Sig_value(desc.val_id, desc.val_val, Exported)],
        newenv
    | Pstr_type (rec_flag, sdecls) ->
        let (decls, newenv) = Typedecl.transl_type_decl env rec_flag sdecls in
        List.iter
          Signature_names.(fun td -> check_type names td.typ_loc td.typ_id)
          decls;
        Tstr_type (rec_flag, decls),
        map_rec_type_with_row_types ~rec_flag
          (fun rs info -> Sig_type(info.typ_id, info.typ_type, rs, Exported))
          decls [],
        enrich_type_decls anchor decls env newenv
    | Pstr_typext styext ->
        let (tyext, newenv) =
          Typedecl.transl_type_extension true env loc styext
        in
        let constructors = tyext.tyext_constructors in
        List.iter
          Signature_names.(fun ext -> check_typext names ext.ext_loc ext.ext_id)
          constructors;
        (Tstr_typext tyext,
         map_ext
           (fun es ext -> Sig_typext(ext.ext_id, ext.ext_type, es, Exported))
           constructors [],
         newenv)
    | Pstr_exception sext ->
        let (ext, newenv) = Typedecl.transl_type_exception env sext in
        let constructor = ext.tyexn_constructor in
        Signature_names.check_typext names constructor.ext_loc
          constructor.ext_id;
        Tstr_exception ext,
        [Sig_typext(constructor.ext_id,
                    constructor.ext_type,
                    Text_exception,
                    Exported)],
        newenv
    | Pstr_module {pmb_name = name; pmb_expr = smodl; pmb_attributes = attrs;
                   pmb_loc;
                  } ->
        let outer_scope = Ctype.get_current_level () in
        let scope = Ctype.create_scope () in
        let modl =
          Builtin_attributes.warning_scope attrs
            (fun () ->
               type_module ~alias:true true funct_body
                 (anchor_submodule name.txt anchor) env smodl
            )
        in
        let pres =
          match modl.mod_type with
          | Mty_alias _ -> Mp_absent
          | _ -> Mp_present
        in
        let md_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) in
        let md =
          { md_type = enrich_module_type anchor name.txt modl.mod_type env;
            md_attributes = attrs;
            md_loc = pmb_loc;
            md_uid;
          }
        in
        (*prerr_endline (Ident.unique_toplevel_name id);*)
        Mtype.lower_nongen outer_scope md.md_type;
        let id, newenv, sg =
          match name.txt with
          | None -> None, env, []
          | Some name ->
            let id, e = Env.enter_module_declaration ~scope name pres md env in
            Signature_names.check_module names pmb_loc id;
            Some id, e,
            [Sig_module(id, pres,
                        {md_type = modl.mod_type;
                         md_attributes = attrs;
                         md_loc = pmb_loc;
                         md_uid;
                        }, Trec_not, Exported)]
        in
        Tstr_module {mb_id=id; mb_name=name; mb_expr=modl;
                     mb_presence=pres; mb_attributes=attrs;  mb_loc=pmb_loc; },
        sg,
        newenv
    | Pstr_recmodule sbind ->
        let sbind =
          List.map
            (function
              | {pmb_name = name;
                 pmb_expr = {pmod_desc=Pmod_constraint(expr, typ)};
                 pmb_attributes = attrs;
                 pmb_loc = loc;
                } ->
                  name, typ, expr, attrs, loc
              | mb ->
                  raise (Error (mb.pmb_expr.pmod_loc, env,
                                Recursive_module_require_explicit_type))
            )
            sbind
        in
        let (decls, newenv) =
          transl_recmodule_modtypes env
            (List.map (fun (name, smty, _smodl, attrs, loc) ->
                 {pmd_name=name; pmd_type=smty;
                  pmd_attributes=attrs; pmd_loc=loc}) sbind
            ) in
        List.iter
          (fun (md, _) ->
            Option.iter Signature_names.(check_module names md.md_loc) md.md_id)
          decls;
        let bindings1 =
          List.map2
            (fun ({md_id=id; md_type=mty}, uid) (name, _, smodl, attrs, loc) ->
               let modl =
                 Builtin_attributes.warning_scope attrs
                   (fun () ->
                      type_module true funct_body (anchor_recmodule id)
                        newenv smodl
                   )
               in
               let mty' =
                 enrich_module_type anchor name.txt modl.mod_type newenv
               in
               (id, name, mty, modl, mty', attrs, loc, uid))
            decls sbind in
        let newenv = (* allow aliasing recursive modules from outside *)
          List.fold_left
            (fun env (md, uid) ->
               match md.md_id with
               | None -> env
               | Some id ->
                   let mdecl =
                     {
                       md_type = md.md_type.mty_type;
                       md_attributes = md.md_attributes;
                       md_loc = md.md_loc;
                       md_uid = uid;
                     }
                   in
                   Env.add_module_declaration ~check:true
                     id Mp_present mdecl env
            )
            env decls
        in
        let bindings2 =
          check_recmodule_inclusion newenv bindings1 in
        let mbs =
          List.filter_map (fun (mb, uid) ->
            Option.map (fun id -> id, mb, uid)  mb.mb_id
          ) bindings2
        in
        Tstr_recmodule (List.map fst bindings2),
        map_rec (fun rs (id, mb, uid) ->
            Sig_module(id, Mp_present, {
                md_type=mb.mb_expr.mod_type;
                md_attributes=mb.mb_attributes;
                md_loc=mb.mb_loc;
                md_uid = uid;
              }, rs, Exported))
           mbs [],
        newenv
    | Pstr_modtype pmtd ->
        (* check that it is non-abstract *)
        let newenv, mtd, sg = transl_modtype_decl names env pmtd in
        Tstr_modtype mtd, [sg], newenv
    | Pstr_open sod ->
        let (od, sg, newenv) =
          type_open_decl ~toplevel funct_body names env sod
        in
        Tstr_open od, sg, newenv
    | Pstr_class cl ->
        let (classes, new_env) = Typeclass.class_declarations env cl in
        List.iter (fun cls ->
          let open Typeclass in
          let loc = cls.cls_id_loc.Location.loc in
          Signature_names.check_class names loc cls.cls_id;
          Signature_names.check_class_type names loc cls.cls_ty_id;
          Signature_names.check_type names loc cls.cls_obj_id;
          Signature_names.check_type names loc cls.cls_typesharp_id;
        ) classes;
        Tstr_class
          (List.map (fun cls ->
               (cls.Typeclass.cls_info,
                cls.Typeclass.cls_pub_methods)) classes),
(* TODO: check with Jacques why this is here
      Tstr_class_type
          (List.map (fun (_,_, i, d, _,_,_,_,_,_,c) -> (i, c)) classes) ::
      Tstr_type
          (List.map (fun (_,_,_,_, i, d, _,_,_,_,_) -> (i, d)) classes) ::
      Tstr_type
          (List.map (fun (_,_,_,_,_,_, i, d, _,_,_) -> (i, d)) classes) ::
*)
        List.flatten
          (map_rec
            (fun rs cls ->
              let open Typeclass in
              [Sig_class(cls.cls_id, cls.cls_decl, rs, Exported);
               Sig_class_type(cls.cls_ty_id, cls.cls_ty_decl, rs, Exported);
               Sig_type(cls.cls_obj_id, cls.cls_obj_abbr, rs, Exported);
               Sig_type(cls.cls_typesharp_id, cls.cls_abbr, rs, Exported)])
             classes []),
        new_env
    | Pstr_class_type cl ->
        let (classes, new_env) = Typeclass.class_type_declarations env cl in
        List.iter (fun decl ->
          let open Typeclass in
          let loc = decl.clsty_id_loc.Location.loc in
          Signature_names.check_class_type names loc decl.clsty_ty_id;
          Signature_names.check_type names loc decl.clsty_obj_id;
          Signature_names.check_type names loc decl.clsty_typesharp_id;
        ) classes;
        Tstr_class_type
          (List.map (fun cl ->
               (cl.Typeclass.clsty_ty_id,
                cl.Typeclass.clsty_id_loc,
                cl.Typeclass.clsty_info)) classes),
(*  TODO: check with Jacques why this is here
           Tstr_type
             (List.map (fun (_, _, i, d, _, _) -> (i, d)) classes) ::
           Tstr_type
             (List.map (fun (_, _, _, _, i, d) -> (i, d)) classes) :: *)
        List.flatten
          (map_rec
             (fun rs decl ->
                let open Typeclass in
                [Sig_class_type(decl.clsty_ty_id, decl.clsty_ty_decl, rs,
                                Exported);
                 Sig_type(decl.clsty_obj_id, decl.clsty_obj_abbr, rs, Exported);
                 Sig_type(decl.clsty_typesharp_id, decl.clsty_abbr, rs,
                          Exported)
                ])
             classes []),
        new_env
    | Pstr_include sincl ->
        let smodl = sincl.pincl_mod in
        let modl =
          Builtin_attributes.warning_scope sincl.pincl_attributes
            (fun () -> type_module true funct_body None env smodl)
        in
        let scope = Ctype.create_scope () in
        (* Rename all identifiers bound by this signature to avoid clashes *)
        let sg, new_env = Env.enter_signature ~scope
            (extract_sig_open env smodl.pmod_loc modl.mod_type) env in
        List.iter (Signature_names.check_sig_item names loc) sg;
        let incl =
          { incl_mod = modl;
            incl_type = sg;
            incl_attributes = sincl.pincl_attributes;
            incl_loc = sincl.pincl_loc;
          }
        in
        Tstr_include incl, sg, new_env
    | Pstr_extension (ext, _attrs) ->
        raise (Error_forward (Builtin_attributes.error_of_extension ext))
    | Pstr_attribute x ->
        Builtin_attributes.warning_attribute x;
        Tstr_attribute x, [], env
  in
  let rec type_struct env sstr =
    match sstr with
    | [] -> ([], [], env)
    | pstr :: srem ->
        let previous_saved_types = Cmt_format.get_saved_types () in
        let desc, sg, new_env = type_str_item env pstr in
        let str = { str_desc = desc; str_loc = pstr.pstr_loc; str_env = env } in
        Cmt_format.set_saved_types (Cmt_format.Partial_structure_item str
                                    :: previous_saved_types);
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (str :: str_rem, sg @ sig_rem, final_env)
  in
  let previous_saved_types = Cmt_format.get_saved_types () in
  let run () =
    let (items, sg, final_env) = type_struct env sstr in
    let str = { str_items = items; str_type = sg; str_final_env = final_env } in
    Cmt_format.set_saved_types
      (Cmt_format.Partial_structure str :: previous_saved_types);
    str, sg, names, final_env
  in
  if toplevel then run ()
  else Builtin_attributes.warning_scope [] run

let type_toplevel_phrase env s =
  Env.reset_required_globals ();
  let (str, sg, to_remove_from_sg, env) =
    type_structure ~toplevel:true false None env s in
  (str, sg, to_remove_from_sg, env)

let type_module_alias = type_module ~alias:true true false None
let type_module = type_module true false None
let type_structure = type_structure false None

(* Normalize types in a signature *)

let rec normalize_modtype = function
    Mty_ident _
  | Mty_alias _ -> ()
  | Mty_signature sg -> normalize_signature sg
  | Mty_functor(_param, body) -> normalize_modtype body

and normalize_signature sg = List.iter normalize_signature_item sg

and normalize_signature_item = function
    Sig_value(_id, desc, _) -> Ctype.normalize_type desc.val_type
  | Sig_module(_id, _, md, _, _) -> normalize_modtype md.md_type
  | _ -> ()

(* Extract the module type of a module expression *)

let type_module_type_of env smod =
  let remove_aliases = has_remove_aliases_attribute smod.pmod_attributes in
  let tmty =
    match smod.pmod_desc with
    | Pmod_ident lid -> (* turn off strengthening in this case *)
        let path, md = Env.lookup_module ~loc:smod.pmod_loc lid.txt env in
          { mod_desc = Tmod_ident (path, lid);
            mod_type = md.md_type;
            mod_env = env;
            mod_attributes = smod.pmod_attributes;
            mod_loc = smod.pmod_loc }
    | _ -> type_module env smod
  in
  let mty = Mtype.scrape_for_type_of ~remove_aliases env tmty.mod_type in
  (* PR#5036: must not contain non-generalized type variables *)
  if not (closed_modtype env mty) then
    raise(Error(smod.pmod_loc, env, Non_generalizable_module mty));
  tmty, mty

(* For Typecore *)

(* Graft a longident onto a path *)
let rec extend_path path =
  fun lid ->
    match lid with
    | Lident name -> Pdot(path, name)
    | Ldot(m, name) -> Pdot(extend_path path m, name)
    | Lapply _ -> assert false

(* Lookup a type's longident within a signature *)
let lookup_type_in_sig sg =
  let types, modules =
    List.fold_left
      (fun acc item ->
         match item with
         | Sig_type(id, _, _, _) ->
             let types, modules = acc in
             let types = String.Map.add (Ident.name id) id types in
             types, modules
         | Sig_module(id, _, _, _, _) ->
             let types, modules = acc in
             let modules = String.Map.add (Ident.name id) id modules in
             types, modules
         | _ -> acc)
      (String.Map.empty, String.Map.empty) sg
  in
  let rec module_path = function
    | Lident name -> Pident (String.Map.find name modules)
    | Ldot(m, name) -> Pdot(module_path m, name)
    | Lapply _ -> assert false
  in
  fun lid ->
    match lid with
    | Lident name -> Pident (String.Map.find name types)
    | Ldot(m, name) -> Pdot(module_path m, name)
    | Lapply _ -> assert false

let type_package env m p nl =
  (* Same as Pexp_letmodule *)
  (* remember original level *)
  Ctype.begin_def ();
  let context = Typetexp.narrow () in
  let modl = type_module env m in
  let scope = Ctype.create_scope () in
  Typetexp.widen context;
  let nl', tl', env =
    match nl with
    | [] -> [], [], env
    | nl ->
      let type_path, env =
        match modl.mod_desc with
        | Tmod_ident (mp,_)
        | Tmod_constraint
            ({mod_desc=Tmod_ident (mp,_)}, _, Tmodtype_implicit, _) ->
          (* We special case these because interactions between
             strengthening of module types and packages can cause
             spurious escape errors. See examples from PR#6982 in the
             testsuite. This can be removed when such issues are
             fixed. *)
          extend_path mp, env
        | _ ->
          let sg = extract_sig_open env modl.mod_loc modl.mod_type in
          let sg, env = Env.enter_signature ~scope sg env in
          lookup_type_in_sig sg, env
      in
      let nl', tl' =
        List.fold_right
          (fun lid (nl, tl) ->
             match type_path lid with
             | exception Not_found -> (nl, tl)
             | path -> begin
                 match Env.find_type path env with
                 | exception Not_found -> (nl, tl)
                 | decl ->
                     if decl.type_arity > 0 then begin
                       (nl, tl)
                     end else begin
                       let t = Btype.newgenty (Tconstr (path,[],ref Mnil)) in
                       (lid :: nl, t :: tl)
                     end
               end)
          nl ([], [])
      in
      nl', tl', env
  in
  (* go back to original level *)
  Ctype.end_def ();
  let mty =
    if nl = [] then (Mty_ident p)
    else modtype_of_package env modl.mod_loc p nl' tl'
  in
  List.iter2
    (fun n ty ->
      try Ctype.unify env ty (Ctype.newvar ())
      with Ctype.Unify _ ->
        raise (Error(modl.mod_loc, env, Scoping_pack (n,ty))))
    nl' tl';
  let modl = wrap_constraint env true modl mty Tmodtype_implicit in
  (* Dropped exports should have produced an error above *)
  assert (List.length nl = List.length tl');
  modl, tl'

(* Fill in the forward declarations *)

let type_open_decl ?used_slot env od =
  type_open_decl ?used_slot ?toplevel:None false (Signature_names.create ()) env
    od

let type_open_descr ?used_slot env od =
  type_open_descr ?used_slot ?toplevel:None env od

let () =
  Typecore.type_module := type_module_alias;
  Typetexp.transl_modtype_longident := transl_modtype_longident;
  Typetexp.transl_modtype := transl_modtype;
  Typecore.type_open := type_open_ ?toplevel:None;
  Typecore.type_open_decl := type_open_decl;
  Typecore.type_package := type_package;
  Typeclass.type_open_descr := type_open_descr;
  type_module_type_of_fwd := type_module_type_of


(* Typecheck an implementation file *)

let gen_annot outputprefix sourcefile annots =
  Cmt2annot.gen_annot (Some (outputprefix ^ ".annot"))
    ~sourcefile:(Some sourcefile) ~use_summaries:false annots

let type_implementation sourcefile outputprefix modulename initial_env ast =
  Cmt_format.clear ();
  Misc.try_finally (fun () ->
      Typecore.reset_delayed_checks ();
      Env.reset_required_globals ();
      if !Clflags.print_types then (* #7656 *)
        Warnings.parse_options false "-32-34-37-38-60";
      let (str, sg, names, finalenv) =
        type_structure initial_env ast in
      let simple_sg = Signature_names.simplify finalenv names sg in
      if !Clflags.print_types then begin
        Typecore.force_delayed_checks ();
        Printtyp.wrap_printing_env ~error:false initial_env
          (fun () -> fprintf std_formatter "%a@."
              (Printtyp.printed_signature sourcefile) simple_sg
          );
        gen_annot outputprefix sourcefile (Cmt_format.Implementation str);
        (str, Tcoerce_none)   (* result is ignored by Compile.implementation *)
      end else begin
        let sourceintf =
          Filename.remove_extension sourcefile ^ !Config.interface_suffix in
        if Sys.file_exists sourceintf then begin
          let intf_file =
            try
              Load_path.find_uncap (modulename ^ ".cmi")
            with Not_found ->
              raise(Error(Location.in_file sourcefile, Env.empty,
                          Interface_not_compiled sourceintf)) in
          let dclsig = Env.read_signature modulename intf_file in
          let coercion =
            Includemod.compunit initial_env ~mark:Mark_positive
              sourcefile sg intf_file dclsig
          in
          Typecore.force_delayed_checks ();
          (* It is important to run these checks after the inclusion test above,
             so that value declarations which are not used internally but
             exported are not reported as being unused. *)
          let annots = Cmt_format.Implementation str in
          Cmt_format.save_cmt (outputprefix ^ ".cmt") modulename
            annots (Some sourcefile) initial_env None;
          gen_annot outputprefix sourcefile annots;
          (str, coercion)
        end else begin
          let coercion =
            Includemod.compunit initial_env ~mark:Mark_positive
              sourcefile sg "(inferred signature)" simple_sg
          in
          check_nongen_schemes finalenv simple_sg;
          normalize_signature simple_sg;
          Typecore.force_delayed_checks ();
          (* See comment above. Here the target signature contains all
             the value being exported. We can still capture unused
             declarations like "let x = true;; let x = 1;;", because in this
             case, the inferred signature contains only the last declaration. *)
          if not !Clflags.dont_write_files then begin
            let alerts = Builtin_attributes.alerts_of_str ast in
            let cmi =
              Env.save_signature ~alerts
                simple_sg modulename (outputprefix ^ ".cmi")
            in
            let annots = Cmt_format.Implementation str in
            Cmt_format.save_cmt  (outputprefix ^ ".cmt") modulename
              annots (Some sourcefile) initial_env (Some cmi);
            gen_annot outputprefix sourcefile annots
          end;
          (str, coercion)
        end
      end
    )
    ~exceptionally:(fun () ->
        let annots =
          Cmt_format.Partial_implementation
            (Array.of_list (Cmt_format.get_saved_types ()))
        in
        Cmt_format.save_cmt  (outputprefix ^ ".cmt") modulename
          annots (Some sourcefile) initial_env None;
        gen_annot outputprefix sourcefile annots
      )

let save_signature modname tsg outputprefix source_file initial_env cmi =
  Cmt_format.save_cmt  (outputprefix ^ ".cmti") modname
    (Cmt_format.Interface tsg) (Some source_file) initial_env (Some cmi)

let type_interface env ast =
  transl_signature env ast

(* "Packaging" of several compilation units into one unit
   having them as sub-modules.  *)

let package_signatures units =
  let units_with_ids =
    List.map
      (fun (name, sg) ->
        let oldid = Ident.create_persistent name in
        let newid = Ident.create_local name in
        (oldid, newid, sg))
      units
  in
  let subst =
    List.fold_left
      (fun acc (oldid, newid, _) ->
        Subst.add_module oldid (Pident newid) acc)
      Subst.identity units_with_ids
  in
  List.map
    (fun (_, newid, sg) ->
      (* This signature won't be used for anything, it'll just be saved in a cmi
         and cmt. *)
      let sg = Subst.signature Make_local subst sg in
      let md =
        { md_type=Mty_signature sg;
          md_attributes=[];
          md_loc=Location.none;
          md_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
        }
      in
      Sig_module(newid, Mp_present, md, Trec_not, Exported))
    units_with_ids

let package_units initial_env objfiles cmifile modulename =
  (* Read the signatures of the units *)
  let units =
    List.map
      (fun f ->
         let pref = chop_extensions f in
         let modname = String.capitalize_ascii(Filename.basename pref) in
         let sg = Env.read_signature modname (pref ^ ".cmi") in
         if Filename.check_suffix f ".cmi" &&
            not(Mtype.no_code_needed_sig Env.initial_safe_string sg)
         then raise(Error(Location.none, Env.empty,
                          Implementation_is_required f));
         (modname, Env.read_signature modname (pref ^ ".cmi")))
      objfiles in
  (* Compute signature of packaged unit *)
  Ident.reinit();
  let sg = package_signatures units in
  (* See if explicit interface is provided *)
  let prefix = Filename.remove_extension cmifile in
  let mlifile = prefix ^ !Config.interface_suffix in
  if Sys.file_exists mlifile then begin
    if not (Sys.file_exists cmifile) then begin
      raise(Error(Location.in_file mlifile, Env.empty,
                  Interface_not_compiled mlifile))
    end;
    let dclsig = Env.read_signature modulename cmifile in
    Cmt_format.save_cmt  (prefix ^ ".cmt") modulename
      (Cmt_format.Packed (sg, objfiles)) None initial_env  None ;
    Includemod.compunit initial_env ~mark:Mark_both
      "(obtained by packing)" sg mlifile dclsig
  end else begin
    (* Determine imports *)
    let unit_names = List.map fst units in
    let imports =
      List.filter
        (fun (name, _crc) -> not (List.mem name unit_names))
        (Env.imports()) in
    (* Write packaged signature *)
    if not !Clflags.dont_write_files then begin
      let cmi =
        Env.save_signature_with_imports ~alerts:Misc.Stdlib.String.Map.empty
          sg modulename
          (prefix ^ ".cmi") imports
      in
      Cmt_format.save_cmt (prefix ^ ".cmt")  modulename
        (Cmt_format.Packed (cmi.Cmi_format.cmi_sign, objfiles)) None initial_env
        (Some cmi)
    end;
    Tcoerce_none
  end

(* Error report *)

open Printtyp

let report_error ppf = function
    Cannot_apply mty ->
      fprintf ppf
        "@[This module is not a functor; it has type@ %a@]" modtype mty
  | Not_included errs ->
      fprintf ppf
        "@[<v>Signature mismatch:@ %a@]" Includemod.report_error errs
  | Cannot_eliminate_dependency mty ->
      fprintf ppf
        "@[This functor has type@ %a@ \
           The parameter cannot be eliminated in the result type.@ \
           Please bind the argument to a module identifier.@]" modtype mty
  | Signature_expected -> fprintf ppf "This module type is not a signature"
  | Structure_expected mty ->
      fprintf ppf
        "@[This module is not a structure; it has type@ %a" modtype mty
  | With_no_component lid ->
      fprintf ppf
        "@[The signature constrained by `with' has no component named %a@]"
        longident lid
  | With_mismatch(lid, explanation) ->
      fprintf ppf
        "@[<v>\
           @[In this `with' constraint, the new definition of %a@ \
             does not match its original definition@ \
             in the constrained signature:@]@ \
           %a@]"
        longident lid Includemod.report_error explanation
  | With_makes_applicative_functor_ill_typed(lid, path, explanation) ->
      fprintf ppf
        "@[<v>\
           @[This `with' constraint on %a makes the applicative functor @ \
             type %s ill-typed in the constrained signature:@]@ \
           %a@]"
        longident lid (Path.name path) Includemod.report_error explanation
  | With_changes_module_alias(lid, id, path) ->
      fprintf ppf
        "@[<v>\
           @[This `with' constraint on %a changes %s, which is aliased @ \
             in the constrained signature (as %s)@].@]"
        longident lid (Path.name path) (Ident.name id)
  | With_cannot_remove_constrained_type ->
      fprintf ppf
        "@[<v>Destructive substitutions are not supported for constrained @ \
              types (other than when replacing a type constructor with @ \
              a type constructor with the same arguments).@]"
  | Repeated_name(kind, name) ->
      fprintf ppf
        "@[Multiple definition of the %s name %s.@ \
         Names must be unique in a given structure or signature.@]"
        (Sig_component_kind.to_string kind) name
  | Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" type_scheme typ
  | Non_generalizable_class (id, desc) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains type variables that cannot be generalized@]"
        (class_declaration id) desc
  | Non_generalizable_module mty ->
      fprintf ppf
        "@[The type of this module,@ %a,@ \
           contains type variables that cannot be generalized@]" modtype mty
  | Implementation_is_required intf_name ->
      fprintf ppf
        "@[The interface %a@ declares values, not just types.@ \
           An implementation must be provided.@]"
        Location.print_filename intf_name
  | Interface_not_compiled intf_name ->
      fprintf ppf
        "@[Could not find the .cmi file for interface@ %a.@]"
        Location.print_filename intf_name
  | Not_allowed_in_functor_body ->
      fprintf ppf
        "@[This expression creates fresh types.@ %s@]"
        "It is not allowed inside applicative functors."
  | Not_a_packed_module ty ->
      fprintf ppf
        "This expression is not a packed module. It has type@ %a"
        type_expr ty
  | Incomplete_packed_module ty ->
      fprintf ppf
        "The type of this packed module contains variables:@ %a"
        type_expr ty
  | Scoping_pack (lid, ty) ->
      fprintf ppf
        "The type %a in this module cannot be exported.@ " longident lid;
      fprintf ppf
        "Its type contains local dependencies:@ %a" type_expr ty
  | Recursive_module_require_explicit_type ->
      fprintf ppf "Recursive modules require an explicit module type."
  | Apply_generative ->
      fprintf ppf "This is a generative functor. It can only be applied to ()"
  | Cannot_scrape_alias p ->
      fprintf ppf
        "This is an alias for module %a, which is missing"
        path p
  | Cannot_scrape_package_type p ->
      fprintf ppf
        "The type of this packed module refers to %a, which is missing"
        path p
  | Badly_formed_signature (context, err) ->
      fprintf ppf "@[In %s:@ %a@]" context Typedecl.report_error err
  | Cannot_hide_id Illegal_shadowing
      { shadowed_item_kind; shadowed_item_id; shadowed_item_loc;
        shadower_id; user_id; user_kind; user_loc } ->
      let shadowed_item_kind= Sig_component_kind.to_string shadowed_item_kind in
      fprintf ppf
        "@[<v>Illegal shadowing of included %s %a by %a@ \
         %a:@;<1 2>%s %a came from this include@ \
         %a:@;<1 2>The %s %s has no valid type if %a is shadowed@]"
        shadowed_item_kind Ident.print shadowed_item_id Ident.print shadower_id
        Location.print_loc shadowed_item_loc
        (String.capitalize_ascii shadowed_item_kind)
        Ident.print shadowed_item_id
        Location.print_loc user_loc
        (Sig_component_kind.to_string user_kind) (Ident.name user_id)
        Ident.print shadowed_item_id
  | Cannot_hide_id Appears_in_signature
      { opened_item_kind; opened_item_id; user_id; user_kind; user_loc } ->
      let opened_item_kind= Sig_component_kind.to_string opened_item_kind in
      fprintf ppf
        "@[<v>The %s %a introduced by this open appears in the signature@ \
         %a:@;<1 2>The %s %s has no valid type if %a is hidden@]"
        opened_item_kind Ident.print opened_item_id
        Location.print_loc user_loc
        (Sig_component_kind.to_string user_kind) (Ident.name user_id)
        Ident.print opened_item_id
  | Invalid_type_subst_rhs ->
      fprintf ppf "Only type synonyms are allowed on the right of :="

let report_error env ppf err =
  Printtyp.wrap_printing_env ~error:true env (fun () -> report_error ppf err)

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (Location.error_of_printer ~loc (report_error env) err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )
