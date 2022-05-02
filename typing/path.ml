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
  | Pcstr_ty of t * string
  | Pext_ty of t
  | Papply of t * t

let rec same p1 p2 =
  p1 == p2
  || match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1), Pdot(p2, s2)) -> s1 = s2 && same p1 p2
  | (Pcstr_ty(p1, s1), Pcstr_ty(p2, s2)) ->
    s1 = s2 && same p1 p2
  | (Pext_ty p1, Pext_ty p2) -> same p1 p2
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
  | (Pcstr_ty(p1, s1), Pcstr_ty(p2, s2)) ->
      let h = compare p1 p2 in
      if h <> 0 then h else String.compare s1 s2
  | (Pext_ty p1, Pext_ty p2) -> compare p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
      let h = compare fun1 fun2 in
      if h <> 0 then h else compare arg1 arg2
  | (Pident _, (Pdot _ | Pcstr_ty _ | Pext_ty _ | Papply _))
  | (Pdot _, (Pcstr_ty _ | Pext_ty _ | Papply _ ))
  | (Pcstr_ty _, (Pext_ty _ | Papply _))
  | (Pext_ty _, Papply _)
    -> -1
  | ((Papply _ | Pext_ty _ | Pcstr_ty _ | Pdot _), Pident _)
  | ((Papply _ | Pext_ty _ | Pcstr_ty _) , Pdot _)
  | ((Papply _ | Pext_ty _), Pcstr_ty _)
  | (Papply _, Pext_ty _)
    -> 1

let rec find_free_opt ids = function
    Pident id -> List.find_opt (Ident.same id) ids
  | Pdot(p, _) | Pcstr_ty(p, _) | Pext_ty p -> find_free_opt ids p
  | Papply(p1, p2) -> begin
      match find_free_opt ids p1 with
      | None -> find_free_opt ids p2
      | Some _ as res -> res
    end

let exists_free ids p =
  match find_free_opt ids p with
  | None -> false
  | _ -> true

let rec scope = function
    Pident id -> Ident.scope id
  | Pdot(p, _) | Pcstr_ty(p, _) | Pext_ty p -> scope p
  | Papply(p1, p2) -> Int.max (scope p1) (scope p2)

let kfalse _ = false

let rec name ?(paren=kfalse) = function
    Pident id -> Ident.name id
  | Pdot(p, s) | Pcstr_ty(p, s) ->
      name ~paren p ^ if paren s then ".( " ^ s ^ " )" else "." ^ s
  | Pext_ty p -> name ~paren p
  | Papply(p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"

let rec print ppf = function
  | Pident id -> Ident.print_with_scope ppf id
  | Pdot(p, s) | Pcstr_ty(p, s) -> Format.fprintf ppf "%a.%s" print p s
  | Pext_ty p -> print ppf p
  | Papply(p1, p2) -> Format.fprintf ppf "%a(%a)" print p1 print p2

let rec head = function
    Pident id -> id
  | Pdot(p, _) | Pcstr_ty(p, _) | Pext_ty p -> head p
  | Papply _ -> assert false

let flatten =
  let rec flatten acc = function
    | Pident id -> `Ok (id, acc)
    | Pdot (p, s) | Pcstr_ty(p, s) -> flatten (s :: acc) p
    | Pext_ty p -> flatten acc p
    | Papply _ -> `Contains_apply
  in
  fun t -> flatten [] t

let heads p =
  let rec heads p acc = match p with
    | Pident id -> id :: acc
    | Pdot (p, _) | Pcstr_ty(p, _) | Pext_ty p -> heads p acc
    | Papply(p1, p2) ->
        heads p1 (heads p2 acc)
  in heads p []

let rec last = function
  | Pident id -> Ident.name id
  | Pdot(_, s) | Pcstr_ty(_, s) -> s
  | Papply(_, p) | Pext_ty p -> last p

let is_constructor_typath p =
  match p with
  | Pident _ | Pdot _ | Papply _ -> false
  | Pcstr_ty _ | Pext_ty _ -> true

module T = struct
  type nonrec t = t
  let compare = compare
end
module Set = Set.Make(T)
module Map = Map.Make(T)
