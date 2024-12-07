(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*   Rodolphe Lepigre, projet Deducteam, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types

module TypeSet = Btype.TypeSet
module TypeMap = Btype.TypeMap

type surface_variance = bool * bool * bool

type variance_variable_context =
  | Type_declaration of Ident.t * type_declaration
  | Gadt_constructor of constructor_declaration
  | Extension_constructor of Ident.t * extension_constructor

type variance_variable_error =
  | No_variable
  | Variance_not_reflected
  | Variance_not_deducible

type variance_error =
  | Variance_not_satisfied of int
  | Variance_variable_error of {
       error : variance_variable_error;
       context : variance_variable_context;
       variable : type_expr
     }

type anonymous_variance_error =
  | Variable_constrained of type_expr
  | Variable_instantiated of type_expr

type error =
  | Bad_variance of variance_error * surface_variance * surface_variance
  | Varying_anonymous of int * anonymous_variance_error


exception Error of Location.t * error

(* Compute variance *)

let get_variance ty visited =
  try TypeMap.find ty !visited with Not_found -> Variance.null

let compute_variance env visited vari ty =
  let rec compute_variance_rec vari ty =
    (* Format.eprintf "%a: %x@." Printtyp.type_expr ty (Obj.magic vari); *)
    let vari' = get_variance ty visited in
    if Variance.subset vari vari' then () else
    let vari = Variance.union vari vari' in
    visited := TypeMap.add ty vari !visited;
    let compute_same = compute_variance_rec vari in
    match get_desc ty with
      Tarrow (_, ty1, ty2, _) ->
        compute_variance_rec (Variance.conjugate vari) ty1;
        compute_same ty2
    | Ttuple tl ->
        List.iter compute_same tl
    | Tconstr (path, tl, _) ->
        let open Variance in
        if tl = [] then () else begin
          try
            let decl = Env.find_type path env in
            List.iter2
              (fun ty v -> compute_variance_rec (compose vari v) ty)
              tl decl.type_variance
          with Not_found ->
            List.iter (compute_variance_rec unknown) tl
        end
    | Tobject (ty, _) ->
        compute_same ty
    | Tfield (_, _, ty1, ty2) ->
        compute_same ty1;
        compute_same ty2
    | Tsubst _ ->
        assert false
    | Tvariant row ->
        List.iter
          (fun (_,f) ->
            match row_field_repr f with
              Rpresent (Some ty) ->
                compute_same ty
            | Reither (_, tyl, _) ->
                let v = Variance.(inter vari unknown) in (* cf PR#7269 *)
                List.iter (compute_variance_rec v) tyl
            | _ -> ())
          (row_fields row);
        compute_same (row_more row)
    | Tpoly (ty, _) ->
        compute_same ty
    | Tvar _ | Tnil | Tlink _ | Tunivar _ -> ()
    | Tpackage (_, fl) ->
        let v = Variance.(compose vari full) in
        List.iter (fun (_, ty) -> compute_variance_rec v ty) fl
  in
  compute_variance_rec vari ty

let make p n i =
  let open Variance in
  set_if p May_pos (set_if n May_neg (set_if i Inj null))

let injective = Variance.(set Inj null)

let compute_variance_type env ~check (required, loc) decl tyl =
  (* Requirements *)
  let check_injectivity = Btype.type_kind_is_abstract decl in
  let required =
    List.map
      (fun (c,n,i) ->
        let i = if check_injectivity then i else false in
        if c || n then (c,n,i) else (true,true,i))
      required
  in
  (* Prepare *)
  let params = decl.type_params in
  let tvl = ref TypeMap.empty in
  (* Compute occurrences in the body *)
  let open Variance in
  List.iter
    (fun (cn,ty) ->
      compute_variance env tvl (if cn then full else covariant) ty)
    tyl;
  (* Infer injectivity of constrained parameters *)
  if check_injectivity then
    List.iter
      (fun ty ->
        if Btype.is_Tvar ty || mem Inj (get_variance ty tvl) then () else
        let visited = ref TypeSet.empty in
        let rec check ty =
          if TypeSet.mem ty !visited then () else begin
            visited := TypeSet.add ty !visited;
            if mem Inj (get_variance ty tvl) then () else
            match get_desc ty with
            | Tvar _ -> raise Exit
            | Tconstr _ ->
                let old = !visited in
                begin try
                  Btype.iter_type_expr check ty
                with Exit ->
                  visited := old;
                  let ty' = Ctype.expand_head_opt env ty in
                  if eq_type ty ty' then raise Exit else check ty'
                end
            | _ -> Btype.iter_type_expr check ty
          end
        in
        try check ty; compute_variance env tvl injective ty
        with Exit -> ())
      params;
  begin match check with
  | None -> ()
  | Some context ->
    (* Check variance of parameters *)
    let pos = ref 0 in
    List.iter2
      (fun ty (c, n, i) ->
        incr pos;
        let var = get_variance ty tvl in
        let (co,cn) = get_upper var and ij = mem Inj var in
        if Btype.is_Tvar ty && (co && not c || cn && not n) || not ij && i
        then raise (Error(loc, Bad_variance
                                (Variance_not_satisfied !pos,
                                                        (co,cn,ij),
                                                        (c,n,i)))))
      params required;
    (* Check propagation from constrained parameters *)
    let args = Btype.newgenty (Ttuple params) in
    let fvl = Ctype.free_variables args in
    let fvl =
      List.filter (fun v -> not (List.exists (eq_type v) params)) fvl in
    (* If there are no extra variables there is nothing to do *)
    if fvl = [] then () else
    let tvl2 = ref TypeMap.empty in
    List.iter2
      (fun ty (p,n,_) ->
        if Btype.is_Tvar ty then () else
        let v =
          if p then if n then full else covariant else conjugate covariant in
        compute_variance env tvl2 v ty)
      params required;
    let visited = ref TypeSet.empty in
    let rec check ty =
      if TypeSet.mem ty !visited then () else
      let visited' = TypeSet.add ty !visited in
      visited := visited';
      let v1 = get_variance ty tvl in
      let snap = Btype.snapshot () in
      let v2 =
        TypeMap.fold
          (fun t vt v ->
             if Ctype.is_equal env false [ty] [t] then union vt v else v)
          !tvl2 null in
      Btype.backtrack snap;
      let (c1,n1) = get_upper v1 and (c2,n2,i2) = get_lower v2 in
      if c1 && not c2 || n1 && not n2 then begin
        match List.find_opt (eq_type ty) fvl with
        | Some variable ->
            let error =
              if not i2 then
                No_variable
              else if c2 || n2 then
                Variance_not_reflected
              else
                Variance_not_deducible
            in
            let variance_error =
              Variance_variable_error { error; context; variable }
            in
            raise
              (Error (loc
                     , Bad_variance ( variance_error
                                    , (c1,n1,false)
                                    , (c2,n2,false))))
        | None ->
            Btype.iter_type_expr check ty
      end
    in
    List.iter (fun (_,ty) -> check ty) tyl;
  end;
  List.map2
    (fun ty (p, n, _i) ->
      let v = get_variance ty tvl in
      let tr = decl.type_private in
      (* Use required variance where relevant *)
      let concr = not (Btype.type_kind_is_abstract decl) in
      let (p, n) =
        if tr = Private || not (Btype.is_Tvar ty) then (p, n) (* set *)
        else (false, false) (* only check *)
      and i = concr in
      let v = union v (make p n i) in
      if not concr || Btype.is_Tvar ty then v else
      union v
        (if p then if n then full else covariant else conjugate covariant))
    params required

let add_false = List.map (fun ty -> false, ty)

(* A parameter is constrained if it is either instantiated,
   or it is a variable appearing in another parameter *)
let constrained vars ty =
  match get_desc ty with
  | Tvar _ ->
      begin match List.find_map (List.find_opt (eq_type ty)) vars with
      | Some var -> Some (Variable_constrained var)
      | None -> None
      end
  | _ -> Some (Variable_instantiated ty)

let for_constr = function
  | Types.Cstr_tuple l -> add_false l
  | Types.Cstr_record l ->
      List.map
        (fun {Types.ld_mutable; ld_type} -> (ld_mutable = Mutable, ld_type))
        l

let compute_variance_gadt env ~check (required, _ as rloc) decl
    (cloc, tl, ret_type_opt) =
  match ret_type_opt with
  | None ->
      compute_variance_type env ~check rloc {decl with type_private = Private}
        (for_constr tl)
  | Some ret_type ->
      match get_desc ret_type with
      | Tconstr (_, tyl, _) ->
          (* let tyl = List.map (Ctype.expand_head env) tyl in *)
          let fvl = List.map (Ctype.free_variables ?env:None) tyl in
          let _ =
            List.fold_left2
              (fun (index, fv1,fv2) ty (c,n,_) ->
                match fv2 with [] -> assert false
                | fv :: fv2 ->
                    (* fv1 @ fv2 = free_variables of other parameters *)
                    if (c || n)
                    then begin
                      match constrained (fv1 @ fv2) ty with
                      | None -> ()
                      | Some reason ->
                          raise (Error(cloc,
                                       Varying_anonymous (index, reason)))
                    end;
                    (succ index, fv :: fv1, fv2))
              (1, [], fvl) tyl required
          in
          compute_variance_type env ~check rloc
            {decl with type_params = tyl; type_private = Private}
            (for_constr tl)
      | _ -> assert false

let compute_variance_extension env decl ext rloc =
  let check =
    Some (Extension_constructor (ext.Typedtree.ext_id, ext.Typedtree.ext_type))
  in
  let ext = ext.Typedtree.ext_type in
  compute_variance_gadt env ~check rloc
    {decl with type_params = ext.ext_type_params}
    (ext.ext_loc, ext.ext_args, ext.ext_ret_type)

let compute_variance_gadt_constructor env ~check rloc decl tl =
  let check =
    match check with
    | Some _ -> Some (Gadt_constructor tl)
    | None -> None
  in
  compute_variance_gadt env ~check rloc decl
    (tl.Types.cd_loc, tl.Types.cd_args, tl.Types.cd_res)

let compute_variance_decl env ~check decl (required, _ as rloc) =
  let check =
    Option.map (fun id -> Type_declaration (id, decl)) check
  in
  let abstract = Btype.type_kind_is_abstract decl in
  if (abstract || decl.type_kind = Type_open) && decl.type_manifest = None then
    List.map
      (fun (c, n, i) -> make (not n) (not c) (not abstract || i))
      required
  else begin
    let mn =
      match decl.type_manifest with
        None -> []
      | Some ty -> [ false, ty ]
    in
    let vari =
      match decl.type_kind with
        Type_abstract _ | Type_open ->
          compute_variance_type env ~check rloc decl mn
      | Type_variant (tll,_rep) ->
          if List.for_all (fun c -> c.Types.cd_res = None) tll then
            compute_variance_type env ~check rloc decl
              (mn @ List.flatten (List.map (fun c -> for_constr c.Types.cd_args)
                                    tll))
          else begin
            let vari =
              List.map
                (fun ty ->
                   compute_variance_type env ~check rloc
                     {decl with type_private = Private}
                     (add_false [ ty ])
                )
                (Option.to_list decl.type_manifest)
            in
            let constructor_variance =
              List.map
                (compute_variance_gadt_constructor env ~check rloc decl)
                tll
            in
            match List.append vari constructor_variance with
            | vari :: rem ->
                List.fold_left (List.map2 Variance.union) vari rem
            | _ -> assert false
          end
      | Type_record (ftl, _) ->
          compute_variance_type env ~check rloc decl
            (mn @ List.map (fun {Types.ld_mutable; ld_type} ->
                 (ld_mutable = Mutable, ld_type)) ftl)
    in
    if mn = [] || not abstract then
      List.map Variance.strengthen vari
    else vari
  end

let is_hash id =
  let s = Ident.name id in
  String.length s > 0 && s.[0] = '#'

let check_variance_extension env decl ext rloc =
  (* TODO: refactorize compute_variance_extension *)
  ignore (compute_variance_extension env decl ext rloc)

let compute_decl env ~check decl req =
  compute_variance_decl env ~check decl (req, decl.type_loc)

let check_decl env id decl req =
  ignore (compute_variance_decl env ~check:(Some id) decl (req, decl.type_loc))

type prop = Variance.t list
type req = surface_variance list
let property : (prop, req) Typedecl_properties.property =
  let open Typedecl_properties in
  let eq li1 li2 =
    try List.for_all2 Variance.eq li1 li2 with _ -> false in
  let merge ~prop ~new_prop =
    List.map2 Variance.union prop new_prop in
  let default decl =
    List.map (fun _ -> Variance.null) decl.type_params in
  let compute env decl req =
    compute_decl env ~check:None decl req in
  let update_decl decl variance =
    { decl with type_variance = variance } in
  let check env id decl req =
    if is_hash id then () else check_decl env id decl req in
  {
    eq;
    merge;
    default;
    compute;
    update_decl;
    check;
  }

let transl_variance (v, i) =
  let co, cn =
    match v with
    | Covariant -> (true, false)
    | Contravariant -> (false, true)
    | NoVariance -> (false, false)
  in
  (co, cn, match i with Injective -> true | NoInjectivity -> false)

let variance_of_params ptype_params =
  List.map transl_variance (List.map snd ptype_params)

let variance_of_sdecl sdecl =
  variance_of_params sdecl.Parsetree.ptype_params

let update_decls env sdecls decls =
  let required = List.map variance_of_sdecl sdecls in
  Typedecl_properties.compute_property property env decls required

let update_class_decls env cldecls =
  let decls, required =
    List.fold_right
      (fun (obj_id, obj_abbr, _clty, _cltydef, ci) (decls, req) ->
        (obj_id, obj_abbr) :: decls,
        variance_of_params ci.Typedtree.ci_params :: req)
      cldecls ([],[])
  in
  let decls =
    Typedecl_properties.compute_property property env decls required in
  List.map2
    (fun (_,decl) (_, _, clty, cltydef, _) ->
      let variance = decl.type_variance in
      (decl, {clty with cty_variance = variance},
       {cltydef with
        clty_variance = variance;
        clty_hash_type = {cltydef.clty_hash_type with type_variance = variance}
       }))
    decls cldecls
