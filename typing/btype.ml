(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Basic operations on core types *)

open Types

(**** Sets, maps and hashtables of types ****)

module TypeSet = Set.Make(TypeOps)
module TypeMap = Map.Make (TypeOps)
module TypeHash = Hashtbl.Make(TypeOps)

(**** Forward declarations ****)

let print_raw =
  ref (fun _ -> assert false : Format.formatter -> type_expr -> unit)

(**** Type level management ****)

let generic_level = 100000000

(* Used to mark a type during a traversal. *)
let lowest_level = 0
let pivot_level = 2 * lowest_level - 1
    (* pivot_level - lowest_level < lowest_level *)

(**** Some type creators ****)

let new_id = ref (-1)

let newty2 level desc  =
  incr new_id; { desc; level; id = !new_id }
let newgenty desc      = newty2 generic_level desc
let newgenvar ?name () = newgenty (Tvar name)
(*
let newmarkedvar level =
  incr new_id; { desc = Tvar; level = pivot_level - level; id = !new_id }
let newmarkedgenvar () =
  incr new_id;
  { desc = Tvar; level = pivot_level - generic_level; id = !new_id }
*)

(**** Check some types ****)

let is_Tvar = function {desc=Tvar _} -> true | _ -> false
let is_Tunivar = function {desc=Tunivar _} -> true | _ -> false

let dummy_method = "*dummy method*"

(**** Representative of a type ****)

let rec field_kind_repr =
  function
    Fvar {contents = Some kind} -> field_kind_repr kind
  | kind                        -> kind

let rec repr =
  function
    {desc = Tlink t'} ->
      (*
         We do no path compression. Path compression does not seem to
         improve notably efficiency, and it prevents from changing a
         [Tlink] into another type (for instance, for undoing a
         unification).
      *)
      repr t'
  | {desc = Tfield (_, k, _, t')} when field_kind_repr k = Fabsent ->
      repr t'
  | t -> t

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
  match (repr row.row_more).desc with
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
        match repr row.row_more with
        | {desc=Tvariant row'} -> row_field tag row'
        | _ -> Rabsent
  in find row.row_fields

let rec row_more row =
  match repr row.row_more with
  | {desc=Tvariant row'} -> row_more row'
  | ty -> ty

let row_fixed row =
  let row = row_repr row in
  row.row_fixed ||
  match (repr row.row_more).desc with
    Tvar _ | Tnil -> false
  | Tunivar _ | Tconstr _ -> true
  | _ -> assert false

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
  let ty0 = repr ty in
  match ty0.desc with
  | Tvariant row when not (static_row row) ->
      row_more row
  | Tobject (ty, _) ->
      let rec proxy_obj ty =
        match ty.desc with
          Tfield (_, _, _, ty) | Tlink ty -> proxy_obj ty
        | Tvar _ | Tunivar _ | Tconstr _ -> ty
        | Tnil -> ty0
        | _ -> assert false
      in proxy_obj ty
  | _ -> ty0

(**** Utilities for fixed row private types ****)

let has_constr_row t =
  match (repr t).desc with
    Tobject(t,_) ->
      let rec check_row t =
        match (repr t).desc with
          Tfield(_,_,_,t) -> check_row t
        | Tconstr _ -> true
        | _ -> false
      in check_row t
  | Tvariant row ->
      (match row_more row with {desc=Tconstr _} -> true | _ -> false)
  | _ ->
      false

let is_row_name s =
  let l = String.length s in
  if l < 4 then false else String.sub s (l-4) 4 = "#row"

let is_constr_row t =
  match t.desc with
    Tconstr (Path.Pident id, _, _) -> is_row_name (Ident.name id)
  | Tconstr (Path.Pdot (_, s, _), _, _) -> is_row_name s
  | _ -> false


                  (**********************************)
                  (*  Utilities for type traversal  *)
                  (**********************************)

let rec iter_row f row =
  List.iter
    (fun (_, fi) ->
      match row_field_repr fi with
      | Rpresent(Some ty) -> f ty
      | Reither(_, tl, _, _) -> List.iter f tl
      | _ -> ())
    row.row_fields;
  match (repr row.row_more).desc with
    Tvariant row -> iter_row f row
  | Tvar _ | Tunivar _ | Tsubst _ | Tconstr _ | Tnil ->
      Misc.may (fun (_,l) -> List.iter f l) row.row_name
  | _ -> assert false

let iter_type_expr f ty =
  match ty.desc with
    Tvar _              -> ()
  | Tarrow (_, ty1, ty2, _) -> f ty1; f ty2
  | Ttuple l            -> List.iter f l
  | Tconstr (_, l, _)   -> List.iter f l
  | Tobject(ty, {contents = Some (_, p)})
                        -> f ty; List.iter f p
  | Tobject (ty, _)     -> f ty
  | Tvariant row        -> iter_row f row; f (row_more row)
  | Tfield (_, _, ty1, ty2) -> f ty1; f ty2
  | Tnil                -> ()
  | Tlink ty            -> f ty
  | Tsubst ty           -> f ty
  | Tunivar _           -> ()
  | Tpoly (ty, tyl)     -> f ty; List.iter f tyl
  | Tpackage (_, _, l)  -> List.iter f l

let rec iter_abbrev f = function
    Mnil                   -> ()
  | Mcons(_, _, ty, ty', rem) -> f ty; f ty'; iter_abbrev f rem
  | Mlink rem              -> iter_abbrev f !rem

let copy_row f fixed row keep more =
  let fields = List.map
      (fun (l, fi) -> l,
        match row_field_repr fi with
        | Rpresent(Some ty) -> Rpresent(Some(f ty))
        | Reither(c, tl, m, e) ->
            let e = if keep then e else ref None in
            let m = if row.row_fixed then fixed else m in
            let tl = List.map f tl in
            Reither(c, tl, m, e)
        | _ -> fi)
      row.row_fields in
  let name =
    match row.row_name with None -> None
    | Some (path, tl) -> Some (path, List.map f tl) in
  { row_fields = fields; row_more = more;
    row_bound = (); row_fixed = row.row_fixed && fixed;
    row_closed = row.row_closed; row_name = name; }

let rec copy_kind = function
    Fvar{contents = Some k} -> copy_kind k
  | Fvar _   -> Fvar (ref None)
  | Fpresent -> Fpresent
  | Fabsent  -> assert false

let copy_commu c =
  if commu_repr c = Cok then Cok else Clink (ref Cunknown)

(* Since univars may be used as row variables, we need to do some
   encoding during substitution *)
let rec norm_univar ty =
  match ty.desc with
    Tunivar _ | Tsubst _ -> ty
  | Tlink ty           -> norm_univar ty
  | Ttuple (ty :: _)   -> norm_univar ty
  | _                  -> assert false

let rec copy_type_desc ?(keep_names=false) f = function
    Tvar _ as ty        -> if keep_names then ty else Tvar None
  | Tarrow (p, ty1, ty2, c)-> Tarrow (p, f ty1, f ty2, copy_commu c)
  | Ttuple l            -> Ttuple (List.map f l)
  | Tconstr (p, l, _)   -> Tconstr (p, List.map f l, ref Mnil)
  | Tobject(ty, {contents = Some (p, tl)})
                        -> Tobject (f ty, ref (Some(p, List.map f tl)))
  | Tobject (ty, _)     -> Tobject (f ty, ref None)
  | Tvariant row        -> assert false (* too ambiguous *)
  | Tfield (p, k, ty1, ty2) -> (* the kind is kept shared *)
      Tfield (p, field_kind_repr k, f ty1, f ty2)
  | Tnil                -> Tnil
  | Tlink ty            -> copy_type_desc f ty.desc
  | Tsubst ty           -> assert false
  | Tunivar _ as ty     -> ty (* always keep the name *)
  | Tpoly (ty, tyl)     ->
      let tyl = List.map (fun x -> norm_univar (f x)) tyl in
      Tpoly (f ty, tyl)
  | Tpackage (p, n, l)  -> Tpackage (p, n, List.map f l)

(* Utilities for copying *)

let saved_desc = ref []
  (* Saved association of generic nodes with their description. *)

let save_desc ty desc =
  saved_desc := (ty, desc)::!saved_desc

let saved_kinds = ref [] (* duplicated kind variables *)
let new_kinds = ref []   (* new kind variables *)
let dup_kind r =
  (match !r with None -> () | Some _ -> assert false);
  if not (List.memq r !new_kinds) then begin
    saved_kinds := r :: !saved_kinds;
    let r' = ref None in
    new_kinds := r' :: !new_kinds;
    r := Some (Fvar r')
  end

(* Restored type descriptions. *)
let cleanup_types () =
  List.iter (fun (ty, desc) -> ty.desc <- desc) !saved_desc;
  List.iter (fun r -> r := None) !saved_kinds;
  saved_desc := []; saved_kinds := []; new_kinds := []

(* Mark a type. *)
let rec mark_type ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr mark_type ty
  end

let mark_type_node ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
  end

let mark_type_params ty =
  iter_type_expr mark_type ty

(* Remove marks from a type. *)
let rec unmark_type ty =
  let ty = repr ty in
  if ty.level < lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr unmark_type ty
  end

let unmark_type_decl decl =
  List.iter unmark_type decl.type_params;
  begin match decl.type_kind with
    Type_abstract -> ()
  | Type_variant cstrs ->
      List.iter
        (fun (c, tl, ret_type_opt) ->
          List.iter unmark_type tl;
          Misc.may unmark_type ret_type_opt)
        cstrs
  | Type_record(lbls, rep) ->
      List.iter (fun (c, mut, t) -> unmark_type t) lbls
  end;
  begin match decl.type_manifest with
    None    -> ()
  | Some ty -> unmark_type ty
  end

let unmark_class_signature sign =
  unmark_type sign.cty_self;
  Vars.iter (fun l (m, v, t) -> unmark_type t) sign.cty_vars

let rec unmark_class_type =
  function
    Cty_constr (p, tyl, cty) ->
      List.iter unmark_type tyl; unmark_class_type cty
  | Cty_signature sign ->
      unmark_class_signature sign
  | Cty_arrow (_, ty, cty) ->
      unmark_type ty; unmark_class_type cty


                  (*******************************************)
                  (*  Memorization of abbreviation expansion *)
                  (*******************************************)

(* Search whether the expansion has been memorized. *)
let rec find_expans priv p1 = function
    Mnil -> None
  | Mcons (priv', p2, ty0, ty, _)
    when priv' >= priv && Path.same p1 p2 -> Some ty
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

let memo = ref []
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
      assert false
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

                  (**********************************)
                  (*  Utilities for labels          *)
                  (**********************************)

let is_optional l =
  String.length l > 0 && l.[0] = '?'

let label_name l =
  if is_optional l then String.sub l 1 (String.length l - 1)
                   else l

let rec extract_label_aux hd l = function
    [] -> raise Not_found
  | (l',t as p) :: ls ->
      if label_name l' = l then (l', t, List.rev hd, ls)
      else extract_label_aux (p::hd) l ls

let extract_label l ls = extract_label_aux [] l ls


                  (**********************************)
                  (*  Utilities for backtracking    *)
                  (**********************************)

type change =
    Ctype of type_expr * type_desc
  | Clevel of type_expr * int
  | Cname of
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option
  | Crow of row_field option ref * row_field option
  | Ckind of field_kind option ref * field_kind option
  | Ccommu of commutable ref * commutable
  | Cuniv of type_expr option ref * type_expr option
  | Ctypeset of TypeSet.t ref * TypeSet.t

let undo_change = function
    Ctype  (ty, desc) -> ty.desc <- desc
  | Clevel (ty, level) -> ty.level <- level
  | Cname  (r, v) -> r := v
  | Crow   (r, v) -> r := v
  | Ckind  (r, v) -> r := v
  | Ccommu (r, v) -> r := v
  | Cuniv  (r, v) -> r := v
  | Ctypeset (r, v) -> r := v

type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid

type snapshot = changes ref * int

let trail = Weak.create 1
let last_snapshot = ref 0

let log_change ch =
  match Weak.get trail 0 with None -> ()
  | Some r ->
      let r' = ref Unchanged in
      r := Change (ch, r');
      Weak.set trail 0 (Some r')

let log_type ty =
  if ty.id <= !last_snapshot then log_change (Ctype (ty, ty.desc))
let link_type ty ty' =
  log_type ty;
  let desc = ty.desc in
  ty.desc <- Tlink ty';
  (* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). *)
  match desc, ty'.desc with
    Tvar name, Tvar name' ->
      begin match name, name' with
      | Some _, None ->  log_type ty'; ty'.desc <- Tvar name
      | None, Some _ ->  ()
      | Some _, Some _ ->
          if ty.level < ty'.level then (log_type ty'; ty'.desc <- Tvar name)
      | None, None   ->  ()
      end
  | _ -> ()
  (* ; assert (check_memorized_abbrevs ()) *)
  (*  ; check_expans [] ty' *)
let set_level ty level =
  if ty.id <= !last_snapshot then log_change (Clevel (ty, ty.level));
  ty.level <- level
let set_univar rty ty =
  log_change (Cuniv (rty, !rty)); rty := Some ty
let set_name nm v =
  log_change (Cname (nm, !nm)); nm := v
let set_row_field e v =
  log_change (Crow (e, !e)); e := Some v
let set_kind rk k =
  log_change (Ckind (rk, !rk)); rk := Some k
let set_commu rc c =
  log_change (Ccommu (rc, !rc)); rc := c
let set_typeset rs s =
  log_change (Ctypeset (rs, !rs)); rs := s

let snapshot () =
  let old = !last_snapshot in
  last_snapshot := !new_id;
  match Weak.get trail 0 with Some r -> (r, old)
  | None ->
      let r = ref Unchanged in
      Weak.set trail 0 (Some r);
      (r, old)

let rec rev_log accu = function
    Unchanged -> accu
  | Invalid -> assert false
  | Change (ch, next) ->
      let d = !next in
      next := Invalid;
      rev_log (ch::accu) d

let backtrack (changes, old) =
  match !changes with
    Unchanged -> last_snapshot := old
  | Invalid -> failwith "Btype.backtrack"
  | Change _ as change ->
      cleanup_abbrev ();
      let backlog = rev_log [] change in
      List.iter undo_change backlog;
      changes := Unchanged;
      last_snapshot := old;
      Weak.set trail 0 (Some changes)
