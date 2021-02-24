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

type t =
    Pident of Ident.t
  | Pdot of t * string
  | Papply of t * t

let rec same p1 p2 =
  p1 == p2
  || match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1), Pdot(p2, s2)) -> s1 = s2 && same p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
       same fun1 fun2 && same arg1 arg2
  | (_, _) -> false

let rec compare p1 p2 =
  if p1 == p2 then 0
  else match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.compare id1 id2
  | (Pdot(p1, s1), Pdot(p2, s2)) ->
      let h = compare p1 p2 in
      if h <> 0 then h else String.compare s1 s2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
      let h = compare fun1 fun2 in
      if h <> 0 then h else compare arg1 arg2
  | ((Pident _ | Pdot _), (Pdot _ | Papply _)) -> -1
  | ((Pdot _ | Papply _), (Pident _ | Pdot _)) -> 1

let rec find_free_opt ids = function
    Pident id -> List.find_opt (Ident.same id) ids
  | Pdot(p, _s) -> find_free_opt ids p
  | Papply(p1, p2) ->
      match find_free_opt ids p1 with
      | None -> find_free_opt ids p2
      | Some _ as res -> res

let exists_free ids p =
  match find_free_opt ids p with
  | None -> false
  | _ -> true

let rec scope = function
    Pident id -> Ident.scope id
  | Pdot(p, _s) -> scope p
  | Papply(p1, p2) -> max (scope p1) (scope p2)

let rec subst_aux find = function
    Pident id -> Pident (find id)
  | Pdot(p, s) -> Pdot (subst_aux find p, s)
  | Papply(p1, p2) ->
      let p1, found1 =
        try subst_aux find p1, true
        with Not_found -> p1, false
      in
      let p2, found2 =
        try subst_aux find p2, true
        with Not_found -> p2, false
      in
      if found1 || found2 then Papply(p1, p2)
      else raise Not_found

let subst id_pairs p =
  if id_pairs = [] then p else
  try
    subst_aux (fun id ->
        snd (List.find (fun x -> Ident.same id (fst x)) id_pairs))
      p
  with Not_found -> p

let unsubst id_pairs p =
  if id_pairs = [] then p else
  try
    subst_aux (fun id ->
        fst (List.find (fun x -> Ident.same id (snd x)) id_pairs))
      p
  with Not_found -> p

let subst_id_pair id_pairs id =
  match List.find_opt (fun x -> Ident.same id (fst x)) id_pairs with
  | Some (_, id') -> id'
  | None -> id

(* Non-allocating equivalent to [scope (subst id_pairs t)]. *)
let rec scope_subst id_pairs = function
    Pident id -> Ident.scope (subst_id_pair id_pairs id)
  | Pdot(p, _s) -> scope_subst id_pairs p
  | Papply(p1, p2) -> max (scope_subst id_pairs p1) (scope_subst id_pairs p2)

let rec find_unscoped = function
  | Pident id when Ident.is_unscoped id -> Some id
  | Pident _ -> None
  | Pdot(p, _s) -> find_unscoped p
  | Papply(p1, p2) ->
      begin match find_unscoped p1 with
      | Some id -> Some id
      | None -> find_unscoped p2
      end

(* Non-allocating equivalent to [find_unscoped (subst id_pairs t)]. *)
let rec find_unscoped_subst id_pairs = function
  | Pident id when Ident.is_unscoped (subst_id_pair id_pairs id) ->
      (* Do not substitute the returned identifier; the substituted identifier
         is the one exposed to the user via the type. *)
      Some id
  | Pident _ -> None
  | Pdot(p, _s) -> find_unscoped_subst id_pairs p
  | Papply(p1, p2) ->
      begin match find_unscoped_subst id_pairs p1 with
      | Some id -> Some id
      | None -> find_unscoped_subst id_pairs p2
      end

let find_unscoped_subst id_pairs p =
  if id_pairs = [] then find_unscoped p else find_unscoped_subst id_pairs p

(* Non-allocating equivalent to
   [same (subst id_pairs1 p1) (subst id_pairs2 p2)]. *)
let rec same_subst id_pairs1 id_pairs2 p1 p2 =
  p1 == p2
  || match (p1, p2) with
    (Pident id1, Pident id2) ->
      Ident.same (subst_id_pair id_pairs1 id1) (subst_id_pair id_pairs2 id2)
  | (Pdot(p1, s1), Pdot(p2, s2)) ->
      s1 = s2 && same_subst id_pairs1 id_pairs2 p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
       same_subst id_pairs1 id_pairs2 fun1 fun2
       && same_subst id_pairs1 id_pairs2 arg1 arg2
  | (_, _) -> false

let kfalse _ = false

let rec name ?(paren=kfalse) = function
    Pident id -> Ident.name id
  | Pdot(p, s) ->
      name ~paren p ^ if paren s then ".( " ^ s ^ " )" else "." ^ s
  | Papply(p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"

let rec print ppf = function
  | Pident id -> Ident.print_with_scope ppf id
  | Pdot(p, s) -> Format.fprintf ppf "%a.%s" print p s
  | Papply(p1, p2) -> Format.fprintf ppf "%a(%a)" print p1 print p2

let rec head = function
    Pident id -> id
  | Pdot(p, _s) -> head p
  | Papply _ -> assert false

let flatten =
  let rec flatten acc = function
    | Pident id -> `Ok (id, acc)
    | Pdot (p, s) -> flatten (s :: acc) p
    | Papply _ -> `Contains_apply
  in
  fun t -> flatten [] t

let heads p =
  let rec heads p acc = match p with
    | Pident id -> id :: acc
    | Pdot (p, _s) -> heads p acc
    | Papply(p1, p2) ->
        heads p1 (heads p2 acc)
  in heads p []

let rec last = function
  | Pident id -> Ident.name id
  | Pdot(_, s) -> s
  | Papply(_, p) -> last p

let is_uident s =
  assert (s <> "");
  match s.[0] with
  | 'A'..'Z' -> true
  | _ -> false

type typath =
  | Regular of t
  | Ext of t * string
  | LocalExt of Ident.t
  | Cstr of t * string

let constructor_typath = function
  | Pident id when is_uident (Ident.name id) -> LocalExt id
  | Pdot(ty_path, s) when is_uident s ->
      if is_uident (last ty_path) then Ext (ty_path, s)
      else Cstr (ty_path, s)
  | p -> Regular p

let is_constructor_typath p =
  match constructor_typath p with
  | Regular _ -> false
  | _ -> true

module T = struct
  type nonrec t = t
  let compare = compare
end
module Set = Set.Make(T)
module Map = Map.Make(T)
