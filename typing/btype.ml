(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Basic operations on core types *)

open Asttypes
open Types

open Local_store

(**** Sets, maps and hashtables of types ****)

let wrap_repr f ty = f (Transient_expr.repr ty)
let wrap_type_expr f tty = f (Transient_expr.type_expr tty)

module TransientTypeSet = Set.Make(TransientTypeOps)
module TypeSet = struct
  include TransientTypeSet
  let add = wrap_repr add
  let mem = wrap_repr mem
  let singleton = wrap_repr singleton
  let exists p = TransientTypeSet.exists (wrap_type_expr p)
  let elements set =
    List.map Transient_expr.type_expr (TransientTypeSet.elements set)
end
module TransientTypeMap = Map.Make(TransientTypeOps)
module TypeMap = struct
  include TransientTypeMap
  let add ty = wrap_repr add ty
  let find ty = wrap_repr find ty
  let singleton ty = wrap_repr singleton ty
  let fold f = TransientTypeMap.fold (wrap_type_expr f)
end
module TransientTypeHash = Hashtbl.Make(TransientTypeOps)
module TypeHash = struct
  include TransientTypeHash
  let add hash = wrap_repr (add hash)
  let find hash = wrap_repr (find hash)
  let iter f = TransientTypeHash.iter (wrap_type_expr f)
end
module TransientTypePairs =
  Hashtbl.Make (struct
    type t = transient_expr * transient_expr
    let equal (t1, t1') (t2, t2') = (t1 == t2) && (t1' == t2')
    let hash (t, t') = t.id + 93 * t'.id
 end)
module TypePairs = struct
  include TransientTypePairs
  open Transient_expr
  let wrap_repr f (t1, t2) = f (repr t1, repr t2)
  let wrap_type_expr f (tt1, tt2) = f (type_expr tt1, type_expr tt2)
  let add hash = wrap_repr (add hash)
  let find hash = wrap_repr (find hash)
  let mem hash = wrap_repr (mem hash)
  let iter f = TransientTypePairs.iter (wrap_type_expr f)
end

(**** Forward declarations ****)

let print_raw =
  ref (fun _ -> assert false : Format.formatter -> type_expr -> unit)

(**** Type level management ****)

let generic_level = Ident.highest_scope

(* Used to mark a type during a traversal. *)
let lowest_level = Ident.lowest_scope
let pivot_level = 2 * lowest_level - 1
    (* pivot_level - lowest_level < lowest_level *)

(**** Some type creators ****)

let newgenty desc      = newty2 ~level:generic_level desc
let newgenvar ?name () = newgenty (Tvar name)
let newgenstub ~scope  = newty3 ~level:generic_level ~scope (Tvar None)

(*
let newmarkedvar level =
  incr new_id; { desc = Tvar; level = pivot_level - level; id = !new_id }
let newmarkedgenvar () =
  incr new_id;
  { desc = Tvar; level = pivot_level - generic_level; id = !new_id }
*)

(**** Check some types ****)

let is_Tvar ty = match get_desc ty with Tvar _ -> true | _ -> false
let is_Tunivar ty = match get_desc ty with Tunivar _ -> true | _ -> false
let is_Tconstr ty = match get_desc ty with Tconstr _ -> true | _ -> false

let dummy_method = "*dummy method*"

(**** Representative of a type ****)

let rec commu_repr = function
    Clink r when !r <> Cunknown -> commu_repr !r
  | c -> c

let rec row_field_repr_aux tl = function
    Reither(_, tl', _, {contents = Some fi}) ->
      row_field_repr_aux (tl@tl') fi
  | Reither(c, tl', m, r) ->
      Reither(c, tl@tl', m, r)
  | Rpresent (Some _) when tl <> [] ->
      Rpresent (Some (List.hd tl))
  | fi -> fi

let row_field_repr fi = row_field_repr_aux [] fi

let rec rev_concat l ll =
  match ll with
    [] -> l
  | l'::ll -> rev_concat (l'@l) ll

let rec row_repr_aux ll row =
  match get_desc row.row_more with
  | Tvariant row' ->
      let f = row.row_fields in
      row_repr_aux (if f = [] then ll else f::ll) row'
  | _ ->
      if ll = [] then row else
      {row with row_fields = rev_concat row.row_fields ll}

let row_repr row = row_repr_aux [] row

let rec row_field tag row =
  let rec find = function
    | (tag',f) :: fields ->
        if tag = tag' then row_field_repr f else find fields
    | [] ->
        match get_desc row.row_more with
        | Tvariant row' -> row_field tag row'
        | _ -> Rabsent
  in find row.row_fields

let rec row_more row =
  match get_desc row.row_more with
  | Tvariant row' -> row_more row'
  | _ -> row.row_more

let merge_fixed_explanation fixed1 fixed2 =
  match fixed1, fixed2 with
  | Some Univar _ as x, _ | _, (Some Univar _ as x) -> x
  | Some Fixed_private as x, _ | _, (Some Fixed_private as x) -> x
  | Some Reified _ as x, _ | _, (Some Reified _ as x) -> x
  | Some Rigid as x, _ | _, (Some Rigid as x) -> x
  | None, None -> None


let fixed_explanation row =
  let row = row_repr row in
  match row.row_fixed with
  | Some _ as x -> x
  | None ->
      match get_desc row.row_more with
      | Tvar _ | Tnil -> None
      | Tunivar _ -> Some (Univar row.row_more)
      | Tconstr (p,_,_) -> Some (Reified p)
      | _ -> assert false

let is_fixed row = match row.row_fixed with
  | None -> false
  | Some _ -> true

let row_fixed row = fixed_explanation row <> None


let static_row row =
  let row = row_repr row in
  row.row_closed &&
  List.for_all
    (fun (_,f) -> match row_field_repr f with Reither _ -> false | _ -> true)
    row.row_fields

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let proxy ty =
  match get_desc ty with
  | Tvariant row when not (static_row row) ->
      row_more row
  | Tobject (ty, _) ->
      let rec proxy_obj ty =
        match get_desc ty with
          Tfield (_, _, _, ty) -> proxy_obj ty
        | Tvar _ | Tunivar _ | Tconstr _ -> ty
        | Tnil -> ty
        | _ -> assert false
      in proxy_obj ty
  | _ -> ty

(**** Utilities for fixed row private types ****)

let row_of_type t =
  match get_desc t with
    Tobject(t,_) ->
      let rec get_row t =
        match get_desc t with
          Tfield(_,_,_,t) -> get_row t
        | _ -> t
      in get_row t
  | Tvariant row ->
      row_more row
  | _ ->
      t

let has_constr_row t =
  not (is_Tconstr t) && is_Tconstr (row_of_type t)

let is_row_name s =
  let l = String.length s in
  if l < 4 then false else String.sub s (l-4) 4 = "#row"

let is_constr_row ~allow_ident t =
  match get_desc t with
    Tconstr (Path.Pident id, _, _) when allow_ident ->
      is_row_name (Ident.name id)
  | Tconstr (Path.Pdot (_, s), _, _) -> is_row_name s
  | _ -> false

(* TODO: where should this really be *)
(* Set row_name in Env, cf. GPR#1204/1329 *)
let set_row_name decl path =
  match decl.type_manifest with
    None -> ()
  | Some ty ->
      match get_desc ty with
        Tvariant row when static_row row ->
          let row = {(row_repr row) with
                     row_name = Some (path, decl.type_params)} in
          set_type_desc ty (Tvariant row)
      | _ -> ()


                  (**********************************)
                  (*  Utilities for type traversal  *)
                  (**********************************)

let rec fold_row f init row =
  let result =
    List.fold_left
      (fun init (_, fi) ->
         match row_field_repr fi with
         | Rpresent(Some ty) -> f init ty
         | Reither(_, tl, _, _) -> List.fold_left f init tl
         | _ -> init)
      init
      row.row_fields
  in
  match get_desc row.row_more with
    Tvariant row -> fold_row f result row
  | Tvar _ | Tunivar _ | Tsubst _ | Tconstr _ | Tnil ->
    begin match
      Option.map (fun (_,l) -> List.fold_left f result l) row.row_name
    with
    | None -> result
    | Some result -> result
    end
  | _ -> assert false

let iter_row f row =
  fold_row (fun () v -> f v) () row

let fold_type_expr f init ty =
  match get_desc ty with
    Tvar _              -> init
  | Tarrow (_, ty1, ty2, _) ->
      let result = f init ty1 in
      f result ty2
  | Ttuple l            -> List.fold_left f init l
  | Tconstr (_, l, _)   -> List.fold_left f init l
  | Tobject(ty, {contents = Some (_, p)}) ->
      let result = f init ty in
      List.fold_left f result p
  | Tobject (ty, _)     -> f init ty
  | Tvariant row        ->
      let result = fold_row f init row in
      f result (row_more row)
  | Tfield (_, _, ty1, ty2) ->
      let result = f init ty1 in
      f result ty2
  | Tnil                -> init
  | Tlink _
  | Tsubst _            -> assert false
  | Tunivar _           -> init
  | Tpoly (ty, tyl)     ->
    let result = f init ty in
    List.fold_left f result tyl
  | Tpackage (_, fl)  ->
    List.fold_left (fun result (_n, ty) -> f result ty) init fl

let iter_type_expr f ty =
  fold_type_expr (fun () v -> f v) () ty

let rec iter_abbrev f = function
    Mnil                   -> ()
  | Mcons(_, _, ty, ty', rem) -> f ty; f ty'; iter_abbrev f rem
  | Mlink rem              -> iter_abbrev f !rem

type type_iterators =
  { it_signature: type_iterators -> signature -> unit;
    it_signature_item: type_iterators -> signature_item -> unit;
    it_value_description: type_iterators -> value_description -> unit;
    it_type_declaration: type_iterators -> type_declaration -> unit;
    it_extension_constructor: type_iterators -> extension_constructor -> unit;
    it_module_declaration: type_iterators -> module_declaration -> unit;
    it_modtype_declaration: type_iterators -> modtype_declaration -> unit;
    it_class_declaration: type_iterators -> class_declaration -> unit;
    it_class_type_declaration: type_iterators -> class_type_declaration -> unit;
    it_functor_param: type_iterators -> functor_parameter -> unit;
    it_module_type: type_iterators -> module_type -> unit;
    it_class_type: type_iterators -> class_type -> unit;
    it_type_kind: type_iterators -> type_decl_kind -> unit;
    it_do_type_expr: type_iterators -> type_expr -> unit;
    it_type_expr: type_iterators -> type_expr -> unit;
    it_path: Path.t -> unit; }

let iter_type_expr_cstr_args f = function
  | Cstr_tuple tl -> List.iter f tl
  | Cstr_record lbls -> List.iter (fun d -> f d.ld_type) lbls

let map_type_expr_cstr_args f = function
  | Cstr_tuple tl -> Cstr_tuple (List.map f tl)
  | Cstr_record lbls ->
      Cstr_record (List.map (fun d -> {d with ld_type=f d.ld_type}) lbls)

let iter_type_expr_kind f = function
  | Type_abstract -> ()
  | Type_variant (cstrs, _) ->
      List.iter
        (fun cd ->
           iter_type_expr_cstr_args f cd.cd_args;
           Option.iter f cd.cd_res
        )
        cstrs
  | Type_record(lbls, _) ->
      List.iter (fun d -> f d.ld_type) lbls
  | Type_open ->
      ()


let type_iterators =
  let it_signature it =
    List.iter (it.it_signature_item it)
  and it_signature_item it = function
      Sig_value (_, vd, _)          -> it.it_value_description it vd
    | Sig_type (_, td, _, _)        -> it.it_type_declaration it td
    | Sig_typext (_, td, _, _)      -> it.it_extension_constructor it td
    | Sig_module (_, _, md, _, _)   -> it.it_module_declaration it md
    | Sig_modtype (_, mtd, _)       -> it.it_modtype_declaration it mtd
    | Sig_class (_, cd, _, _)       -> it.it_class_declaration it cd
    | Sig_class_type (_, ctd, _, _) -> it.it_class_type_declaration it ctd
  and it_value_description it vd =
    it.it_type_expr it vd.val_type
  and it_type_declaration it td =
    List.iter (it.it_type_expr it) td.type_params;
    Option.iter (it.it_type_expr it) td.type_manifest;
    it.it_type_kind it td.type_kind
  and it_extension_constructor it td =
    it.it_path td.ext_type_path;
    List.iter (it.it_type_expr it) td.ext_type_params;
    iter_type_expr_cstr_args (it.it_type_expr it) td.ext_args;
    Option.iter (it.it_type_expr it) td.ext_ret_type
  and it_module_declaration it md =
    it.it_module_type it md.md_type
  and it_modtype_declaration it mtd =
    Option.iter (it.it_module_type it) mtd.mtd_type
  and it_class_declaration it cd =
    List.iter (it.it_type_expr it) cd.cty_params;
    it.it_class_type it cd.cty_type;
    Option.iter (it.it_type_expr it) cd.cty_new;
    it.it_path cd.cty_path
  and it_class_type_declaration it ctd =
    List.iter (it.it_type_expr it) ctd.clty_params;
    it.it_class_type it ctd.clty_type;
    it.it_path ctd.clty_path
  and it_functor_param it = function
    | Unit -> ()
    | Named (_, mt) -> it.it_module_type it mt
  and it_module_type it = function
      Mty_ident p
    | Mty_alias p -> it.it_path p
    | Mty_signature sg -> it.it_signature it sg
    | Mty_functor (p, mt) ->
        it.it_functor_param it p;
        it.it_module_type it mt
  and it_class_type it = function
      Cty_constr (p, tyl, cty) ->
        it.it_path p;
        List.iter (it.it_type_expr it) tyl;
        it.it_class_type it cty
    | Cty_signature cs ->
        it.it_type_expr it cs.csig_self;
        it.it_type_expr it cs.csig_self_row;
        Vars.iter (fun _ (_,_,ty) -> it.it_type_expr it ty) cs.csig_vars;
        Meths.iter (fun _ (_,_,ty) -> it.it_type_expr it ty) cs.csig_meths;
        List.iter
          (fun (p, tl) -> it.it_path p; List.iter (it.it_type_expr it) tl)
          cs.csig_inher
    | Cty_arrow  (_, ty, cty) ->
        it.it_type_expr it ty;
        it.it_class_type it cty
  and it_type_kind it kind =
    iter_type_expr_kind (it.it_type_expr it) kind
  and it_do_type_expr it ty =
    iter_type_expr (it.it_type_expr it) ty;
    match get_desc ty with
      Tconstr (p, _, _)
    | Tobject (_, {contents=Some (p, _)})
    | Tpackage (p, _) ->
        it.it_path p
    | Tvariant row ->
        Option.iter (fun (p,_) -> it.it_path p) (row_repr row).row_name
    | _ -> ()
  and it_path _p = ()
  in
  { it_path; it_type_expr = it_do_type_expr; it_do_type_expr;
    it_type_kind; it_class_type; it_functor_param; it_module_type;
    it_signature; it_class_type_declaration; it_class_declaration;
    it_modtype_declaration; it_module_declaration; it_extension_constructor;
    it_type_declaration; it_value_description; it_signature_item; }

let copy_row f fixed row keep more =
  let fields = List.map
      (fun (l, fi) -> l,
        match row_field_repr fi with
        | Rpresent(Some ty) -> Rpresent(Some(f ty))
        | Reither(c, tl, m, e) ->
            let e = if keep then e else ref None in
            let m = if is_fixed row then fixed else m in
            let tl = List.map f tl in
            Reither(c, tl, m, e)
        | _ -> fi)
      row.row_fields in
  let name =
    match row.row_name with
    | None -> None
    | Some (path, tl) -> Some (path, List.map f tl) in
  let row_fixed = if fixed then row.row_fixed else None in
  { row_fields = fields; row_more = more;
    row_bound = (); row_fixed;
    row_closed = row.row_closed; row_name = name; }

let rec copy_kind = function
    Fvar{contents = Some k} -> copy_kind k
  | Fvar _   -> Fvar (ref None)
  | Fpresent -> Fpresent
  | Fabsent  -> assert false

let copy_commu c =
  if commu_repr c = Cok then Cok else Clink (ref Cunknown)

let rec copy_type_desc ?(keep_names=false) f = function
    Tvar _ as ty        -> if keep_names then ty else Tvar None
  | Tarrow (p, ty1, ty2, c)-> Tarrow (p, f ty1, f ty2, copy_commu c)
  | Ttuple l            -> Ttuple (List.map f l)
  | Tconstr (p, l, _)   -> Tconstr (p, List.map f l, ref Mnil)
  | Tobject(ty, {contents = Some (p, tl)})
                        -> Tobject (f ty, ref (Some(p, List.map f tl)))
  | Tobject (ty, _)     -> Tobject (f ty, ref None)
  | Tvariant _          -> assert false (* too ambiguous *)
  | Tfield (p, k, ty1, ty2) -> (* the kind is kept shared *)
      Tfield (p, field_kind_repr k, f ty1, f ty2)
  | Tnil                -> Tnil
  | Tlink ty            -> copy_type_desc f (get_desc ty)
  | Tsubst _            -> assert false
  | Tunivar _ as ty     -> ty (* always keep the name *)
  | Tpoly (ty, tyl)     ->
      let tyl = List.map f tyl in
      Tpoly (f ty, tyl)
  | Tpackage (p, fl)  -> Tpackage (p, List.map (fun (n, ty) -> (n, f ty)) fl)

(* Utilities for copying *)

module For_copy : sig
  type copy_scope

  val redirect_desc: copy_scope -> type_expr -> type_desc -> unit

  val dup_kind: copy_scope -> field_kind option ref -> unit

  val with_scope: (copy_scope -> 'a) -> 'a
end = struct
  type copy_scope = {
    mutable saved_desc : (transient_expr * type_desc) list;
    (* Save association of generic nodes with their description. *)

    mutable saved_kinds: field_kind option ref list;
    (* duplicated kind variables *)

    mutable new_kinds  : field_kind option ref list;
    (* new kind variables *)
  }

  let redirect_desc copy_scope ty desc =
    let ty = Transient_expr.repr ty in
    copy_scope.saved_desc <- (ty, ty.desc) :: copy_scope.saved_desc;
    Transient_expr.set_desc ty desc

  let dup_kind copy_scope r =
    assert (Option.is_none !r);
    if not (List.memq r copy_scope.new_kinds) then begin
      copy_scope.saved_kinds <- r :: copy_scope.saved_kinds;
      let r' = ref None in
      copy_scope.new_kinds <- r' :: copy_scope.new_kinds;
      r := Some (Fvar r')
    end

  (* Restore type descriptions. *)
  let cleanup { saved_desc; saved_kinds; _ } =
    List.iter (fun (ty, desc) -> Transient_expr.set_desc ty desc) saved_desc;
    List.iter (fun r -> r := None) saved_kinds

  let with_scope f =
    let scope = { saved_desc = []; saved_kinds = []; new_kinds = [] } in
    let res = f scope in
    cleanup scope;
    res
end

                  (*******************************************)
                  (*  Memorization of abbreviation expansion *)
                  (*******************************************)

(* Search whether the expansion has been memorized. *)

let lte_public p1 p2 =  (* Private <= Public *)
  match p1, p2 with
  | Private, _ | _, Public -> true
  | Public, Private -> false

let rec find_expans priv p1 = function
    Mnil -> None
  | Mcons (priv', p2, _ty0, ty, _)
    when lte_public priv priv' && Path.same p1 p2 -> Some ty
  | Mcons (_, _, _, _, rem)   -> find_expans priv p1 rem
  | Mlink {contents = rem} -> find_expans priv p1 rem

(* debug: check for cycles in abbreviation. only works with -principal
let rec check_expans visited ty =
  let ty = repr ty in
  assert (not (List.memq ty visited));
  match ty.desc with
    Tconstr (path, args, abbrev) ->
      begin match find_expans path !abbrev with
        Some ty' -> check_expans (ty :: visited) ty'
      | None -> ()
      end
  | _ -> ()
*)

let memo = s_ref []
        (* Contains the list of saved abbreviation expansions. *)

let cleanup_abbrev () =
        (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := Mnil) !memo;
  memo := []

let memorize_abbrev mem priv path v v' =
        (* Memorize the expansion of an abbreviation. *)
  mem := Mcons (priv, path, v, v', !mem);
  (* check_expans [] v; *)
  memo := mem :: !memo

let rec forget_abbrev_rec mem path =
  match mem with
    Mnil ->
      mem
  | Mcons (_, path', _, _, rem) when Path.same path path' ->
      rem
  | Mcons (priv, path', v, v', rem) ->
      Mcons (priv, path', v, v', forget_abbrev_rec rem path)
  | Mlink mem' ->
      mem' := forget_abbrev_rec !mem' path;
      raise Exit

let forget_abbrev mem path =
  try mem := forget_abbrev_rec !mem path with Exit -> ()

(* debug: check for invalid abbreviations
let rec check_abbrev_rec = function
    Mnil -> true
  | Mcons (_, ty1, ty2, rem) ->
      repr ty1 != repr ty2
  | Mlink mem' ->
      check_abbrev_rec !mem'

let check_memorized_abbrevs () =
  List.for_all (fun mem -> check_abbrev_rec !mem) !memo
*)

(* Re-export backtrack *)

let snapshot = snapshot
let backtrack = backtrack ~cleanup_abbrev

                  (**********************************)
                  (*  Utilities for labels          *)
                  (**********************************)

let is_optional = function Optional _ -> true | _ -> false

let label_name = function
    Nolabel -> ""
  | Labelled s
  | Optional s -> s

let prefixed_label_name = function
    Nolabel -> ""
  | Labelled s -> "~" ^ s
  | Optional s -> "?" ^ s

let rec extract_label_aux hd l = function
  | [] -> None
  | (l',t as p) :: ls ->
      if label_name l' = l then
        Some (l', t, hd <> [], List.rev_append hd ls)
      else
        extract_label_aux (p::hd) l ls

let extract_label l ls = extract_label_aux [] l ls

                  (**********************************)
                  (*  Utilities for level-marking   *)
                  (**********************************)

let not_marked_node ty = get_level ty >= lowest_level
    (* type nodes with negative levels are "marked" *)

let flip_mark_node ty =
  let ty = Transient_expr.repr ty in
  Transient_expr.set_level ty (pivot_level - ty.level)
let logged_mark_node ty =
  set_level ty (pivot_level - get_level ty)

let try_mark_node ty = not_marked_node ty && (flip_mark_node ty; true)
let try_logged_mark_node ty = not_marked_node ty && (logged_mark_node ty; true)

let rec mark_type ty =
  if not_marked_node ty then begin
    flip_mark_node ty;
    iter_type_expr mark_type ty
  end

let mark_type_params ty =
  iter_type_expr mark_type ty

let type_iterators =
  let it_type_expr it ty =
    if try_mark_node ty then it.it_do_type_expr it ty
  in
  {type_iterators with it_type_expr}


(* Remove marks from a type. *)
let rec unmark_type ty =
  if get_level ty < lowest_level then begin
    (* flip back the marked level *)
    flip_mark_node ty;
    iter_type_expr unmark_type ty
  end

let unmark_iterators =
  let it_type_expr _it ty = unmark_type ty in
  {type_iterators with it_type_expr}

let unmark_type_decl decl =
  unmark_iterators.it_type_declaration unmark_iterators decl

let unmark_extension_constructor ext =
  List.iter unmark_type ext.ext_type_params;
  iter_type_expr_cstr_args unmark_type ext.ext_args;
  Option.iter unmark_type ext.ext_ret_type

let unmark_class_signature sign =
  unmark_type sign.csig_self;
  unmark_type sign.csig_self_row;
  Vars.iter (fun _l (_m, _v, t) -> unmark_type t) sign.csig_vars;
  Meths.iter (fun _l (_m, _v, t) -> unmark_type t) sign.csig_meths

let unmark_class_type cty =
  unmark_iterators.it_class_type unmark_iterators cty

(**** Type information getter ****)

let cstr_type_path cstr =
  match get_desc cstr.cstr_res with
  | Tconstr (p, _, _) -> p
  | _ -> assert false
