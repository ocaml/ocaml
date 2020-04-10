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

type error =
| Bad_variance of int * surface_variance * surface_variance
| Varying_anonymous

exception Error of Location.t * error

(* Compute variance *)

let get_variance ty visited =
  try TypeMap.find ty !visited with Not_found -> Variance.null

let compute_variance env visited vari ty =
  let rec compute_variance_rec vari ty =
    (* Format.eprintf "%a: %x@." Printtyp.type_expr ty (Obj.magic vari); *)
    let ty = Ctype.repr ty in
    let vari' = get_variance ty visited in
    if Variance.subset vari vari' then () else
    let vari = Variance.union vari vari' in
    visited := TypeMap.add ty vari !visited;
    let compute_same = compute_variance_rec vari in
    match ty.desc with
      Tarrow (_, ty1, ty2, _) ->
        let open Variance in
        let v = conjugate vari in
        let v1 =
          if mem May_pos v || mem May_neg v
          then set May_weak true v else v
        in
        compute_variance_rec v1 ty1;
        compute_same ty2
    | Ttuple tl ->
        List.iter compute_same tl
    | Tconstr (path, tl, _) ->
        let open Variance in
        if tl = [] then () else begin
          try
            let decl = Env.find_type path env in
            let cvari f = mem f vari in
            List.iter2
              (fun ty v ->
                let cv f = mem f v in
                let strict =
                  cvari Inv && cv Inj || (cvari Pos || cvari Neg) && cv Inv
                in
                if strict then compute_variance_rec full ty else
                let p1 = inter v vari
                and n1 = inter v (conjugate vari) in
                let v1 =
                  union (inter covariant (union p1 (conjugate p1)))
                    (inter (conjugate covariant) (union n1 (conjugate n1)))
                and weak =
                  cvari May_weak && (cv May_pos || cv May_neg) ||
                  (cvari May_pos || cvari May_neg) && cv May_weak
                in
                let v2 = set May_weak weak v1 in
                compute_variance_rec v2 ty)
              tl decl.type_variance
          with Not_found ->
            List.iter (compute_variance_rec may_inv) tl
        end
    | Tobject (ty, _) ->
        compute_same ty
    | Tfield (_, _, ty1, ty2) ->
        compute_same ty1;
        compute_same ty2
    | Tsubst ty ->
        compute_same ty
    | Tvariant row ->
        let row = Btype.row_repr row in
        List.iter
          (fun (_,f) ->
            match Btype.row_field_repr f with
              Rpresent (Some ty) ->
                compute_same ty
            | Reither (_, tyl, _, _) ->
                let open Variance in
                let upper =
                  List.fold_left (fun s f -> set f true s)
                    null [May_pos; May_neg; May_weak]
                in
                let v = inter vari upper in
                (* cf PR#7269:
                   if List.length tyl > 1 then upper else inter vari upper *)
                List.iter (compute_variance_rec v) tyl
            | _ -> ())
          row.row_fields;
        compute_same row.row_more
    | Tpoly (ty, _) ->
        compute_same ty
    | Tvar _ | Tnil | Tlink _ | Tunivar _ -> ()
    | Tpackage (_, _, tyl) ->
        let v =
          Variance.(if mem Pos vari || mem Neg vari then full else may_inv)
        in
        List.iter (compute_variance_rec v) tyl
  in
  compute_variance_rec vari ty

let make p n i =
  let open Variance in
  set May_pos p (set May_neg n (set May_weak n (set Inj i null)))

let compute_variance_type env ~check (required, loc) decl tyl =
  (* Requirements *)
  let required =
    List.map (fun (c,n,i) -> if c || n then (c,n,i) else (true,true,i))
      required
  in
  (* Prepare *)
  let params = List.map Btype.repr decl.type_params in
  let tvl = ref TypeMap.empty in
  (* Compute occurrences in the body *)
  let open Variance in
  List.iter
    (fun (cn,ty) ->
      compute_variance env tvl (if cn then full else covariant) ty)
    tyl;
  if check then begin
    (* Check variance of parameters *)
    let pos = ref 0 in
    List.iter2
      (fun ty (c, n, i) ->
        incr pos;
        let var = get_variance ty tvl in
        let (co,cn) = get_upper var and ij = mem Inj var in
        if Btype.is_Tvar ty && (co && not c || cn && not n || not ij && i)
        then raise (Error(loc, Bad_variance (!pos, (co,cn,ij), (c,n,i)))))
      params required;
    (* Check propagation from constrained parameters *)
    let args = Btype.newgenty (Ttuple params) in
    let fvl = Ctype.free_variables args in
    let fvl = List.filter (fun v -> not (List.memq v params)) fvl in
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
      let ty = Ctype.repr ty in
      if TypeSet.mem ty !visited then () else
      let visited' = TypeSet.add ty !visited in
      visited := visited';
      let v1 = get_variance ty tvl in
      let snap = Btype.snapshot () in
      let v2 =
        TypeMap.fold
          (fun t vt v ->
            if Ctype.equal env false [ty] [t] then union vt v else v)
          !tvl2 null in
      Btype.backtrack snap;
      let (c1,n1) = get_upper v1 and (c2,n2,_,i2) = get_lower v2 in
      if c1 && not c2 || n1 && not n2 then
        if List.memq ty fvl then
          let code = if not i2 then -2 else if c2 || n2 then -1 else -3 in
          raise (Error (loc, Bad_variance (code, (c1,n1,false), (c2,n2,false))))
        else
          Btype.iter_type_expr check ty
    in
    List.iter (fun (_,ty) -> check ty) tyl;
  end;
  List.map2
    (fun ty (p, n, i) ->
      let v = get_variance ty tvl in
      let tr = decl.type_private in
      (* Use required variance where relevant *)
      let concr = decl.type_kind <> Type_abstract (*|| tr = Type_new*) in
      let (p, n) =
        if tr = Private || not (Btype.is_Tvar ty) then (p, n) (* set *)
        else (false, false) (* only check *)
      and i = concr  || i && tr = Private in
      let v = union v (make p n i) in
      let v =
        if not concr then v else
        if mem Pos v && mem Neg v then full else
        if Btype.is_Tvar ty then v else
        union v
          (if p then if n then full else covariant else conjugate covariant)
      in
      if decl.type_kind = Type_abstract && tr = Public then v else
      set May_weak (mem May_neg v) v)
    params required

let add_false = List.map (fun ty -> false, ty)

(* A parameter is constrained if it is either instantiated,
   or it is a variable appearing in another parameter *)
let constrained vars ty =
  match ty.desc with
  | Tvar _ -> List.exists (fun tl -> List.memq ty tl) vars
  | _ -> true

let for_constr = function
  | Types.Cstr_tuple l -> add_false l
  | Types.Cstr_record l ->
      List.map
        (fun {Types.ld_mutable; ld_type} -> (ld_mutable = Mutable, ld_type))
        l

let compute_variance_gadt env ~check (required, loc as rloc) decl
    (tl, ret_type_opt) =
  match ret_type_opt with
  | None ->
      compute_variance_type env ~check rloc {decl with type_private = Private}
        (for_constr tl)
  | Some ret_type ->
      match Ctype.repr ret_type with
      | {desc=Tconstr (_, tyl, _)} ->
          (* let tyl = List.map (Ctype.expand_head env) tyl in *)
          let tyl = List.map Ctype.repr tyl in
          let fvl = List.map (Ctype.free_variables ?env:None) tyl in
          let _ =
            List.fold_left2
              (fun (fv1,fv2) ty (c,n,_) ->
                match fv2 with [] -> assert false
                | fv :: fv2 ->
                    (* fv1 @ fv2 = free_variables of other parameters *)
                    if (c||n) && constrained (fv1 @ fv2) ty then
                      raise (Error(loc, Varying_anonymous));
                    (fv :: fv1, fv2))
              ([], fvl) tyl required
          in
          compute_variance_type env ~check rloc
            {decl with type_params = tyl; type_private = Private}
            (for_constr tl)
      | _ -> assert false

let compute_variance_extension env ~check decl ext rloc =
  compute_variance_gadt env ~check rloc
    {decl with type_params = ext.ext_type_params}
    (ext.ext_args, ext.ext_ret_type)

let compute_variance_decl env ~check decl (required, _ as rloc) =
  if (decl.type_kind = Type_abstract || decl.type_kind = Type_open)
       && decl.type_manifest = None then
    List.map
      (fun (c, n, i) ->
        make (not n) (not c) (decl.type_kind <> Type_abstract || i))
      required
  else
  let mn =
    match decl.type_manifest with
      None -> []
    | Some ty -> [false, ty]
  in
  match decl.type_kind with
    Type_abstract | Type_open ->
      compute_variance_type env ~check rloc decl mn
  | Type_variant tll ->
      if List.for_all (fun c -> c.Types.cd_res = None) tll then
        compute_variance_type env ~check rloc decl
          (mn @ List.flatten (List.map (fun c -> for_constr c.Types.cd_args)
                                tll))
      else begin
        let mn =
          List.map (fun (_,ty) -> (Types.Cstr_tuple [ty],None)) mn in
        let tll =
          mn @ List.map (fun c -> c.Types.cd_args, c.Types.cd_res) tll in
        match List.map (compute_variance_gadt env ~check rloc decl) tll with
        | vari :: rem ->
            let varl = List.fold_left (List.map2 Variance.union) vari rem in
            List.map
              Variance.(fun v -> if mem Pos v && mem Neg v then full else v)
              varl
        | _ -> assert false
      end
  | Type_record (ftl, _) ->
      compute_variance_type env ~check rloc decl
        (mn @ List.map (fun {Types.ld_mutable; ld_type} ->
             (ld_mutable = Mutable, ld_type)) ftl)

let is_hash id =
  let s = Ident.name id in
  String.length s > 0 && s.[0] = '#'

let check_variance_extension env decl ext rloc =
  (* TODO: refactorize compute_variance_extension *)
  ignore (compute_variance_extension env ~check:true decl
    ext.Typedtree.ext_type rloc)

let compute_decl env ~check decl req =
  compute_variance_decl env ~check decl (req, decl.type_loc)

let check_decl env decl req =
  ignore (compute_variance_decl env ~check:true decl (req, decl.type_loc))

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
    compute_decl env ~check:false decl req in
  let update_decl decl variance =
    { decl with type_variance = variance } in
  let check env id decl req =
    if is_hash id then () else check_decl env decl req in
  {
    eq;
    merge;
    default;
    compute;
    update_decl;
    check;
  }

let transl_variance : Asttypes.variance -> _ = function
  | Covariant -> (true, false, false)
  | Contravariant -> (false, true, false)
  | Invariant -> (false, false, false)

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
      (fun (obj_id, obj_abbr, _cl_abbr, _clty, _cltydef, ci) (decls, req) ->
        (obj_id, obj_abbr) :: decls,
        variance_of_params ci.Typedtree.ci_params :: req)
      cldecls ([],[])
  in
  let decls =
    Typedecl_properties.compute_property property env decls required in
  List.map2
    (fun (_,decl) (_, _, cl_abbr, clty, cltydef, _) ->
      let variance = decl.type_variance in
      (decl, {cl_abbr with type_variance = variance},
       {clty with cty_variance = variance},
       {cltydef with clty_variance = variance}))
    decls cldecls
