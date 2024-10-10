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
  | Pextra_ty of t * extra_ty
and extra_ty =
  | Pcstr_ty of string
  | Pext_ty

let same_aux ident_cmp p1 p2 =
  let rec aux p1 p2 =
    p1 == p2
    || match (p1, p2) with
      (Pident id1, Pident id2) -> ident_cmp id1 id2
    | (Pdot(p1, s1), Pdot(p2, s2)) ->
        s1 = s2 && aux p1 p2
    | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
        aux fun1 fun2 && aux arg1 arg2
    | (Pextra_ty (p1, t1), Pextra_ty (p2, t2)) ->
        let same_extra = match t1, t2 with
          | (Pcstr_ty s1, Pcstr_ty s2) -> s1 = s2
          | (Pext_ty, Pext_ty) -> true
          | ((Pcstr_ty _ | Pext_ty), _) -> false
        in same_extra && aux p1 p2
    | (_, _) -> false
  in aux p1 p2

let same p1 p2 = same_aux Ident.same p1 p2

let equiv p1 p2 = same_aux Ident.equiv p1 p2

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
  | (Pextra_ty (p1, t1), Pextra_ty (p2, t2)) ->
      let h = compare_extra t1 t2 in
      if h <> 0 then h else compare p1 p2
  | (Pident _, (Pdot _ | Papply _ | Pextra_ty _))
  | (Pdot _, (Papply _ | Pextra_ty _))
  | (Papply _, Pextra_ty _)
    -> -1
  | ((Pextra_ty _ | Papply _ | Pdot _), Pident _)
  | ((Pextra_ty _ | Papply _) , Pdot _)
  | (Pextra_ty _, Papply _)
    -> 1
and compare_extra t1 t2 =
  match (t1, t2) with
    Pcstr_ty s1, Pcstr_ty s2 -> String.compare s1 s2
  | (Pext_ty, Pext_ty)
    -> 0
  | (Pcstr_ty _, Pext_ty)
    -> -1
  | (Pext_ty, Pcstr_ty _)
    -> 1

let rec find_free_opt ids = function
    Pident id -> List.find_opt (Ident.same id) ids
  | Pdot(p, _) | Pextra_ty (p, _) -> find_free_opt ids p
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
  | Pdot(p, _) | Pextra_ty (p, _) -> scope p
  | Papply(p1, p2) -> Int.max (scope p1) (scope p2)

let rec contains id = function
    Pident id' -> Ident.same id' id
  | Pdot(p, _) | Pextra_ty(p, _) -> contains id p
  | Papply(p1, p2) -> contains id p1 || contains id p2

let subst id_map p =
  let exception Unchanged in
  let rec aux = function
  | Pident id ->
    begin match List.find (fun (i, _) -> Ident.same i id) id_map with
    | (_, p) -> p
    | exception Not_found -> raise Unchanged
    end
  | Pdot(p, s) -> Pdot(aux p, s)
  | Pextra_ty(p, e) -> Pextra_ty(aux p, e)
  | Papply(p1, p2) ->
    let p1, b1 = match aux p1 with
      | p -> p, false
      | exception Unchanged -> p1, true
    in
    let p2, b2 = match aux p2 with
      | p -> p, false
      | exception Unchanged -> p2, true in
    if b1 && b2
    then raise Unchanged
    else Papply(p1, p2)
  in
  try aux p with Unchanged -> p

let check_for_unbound_unscoped_idents idl p =
  let exception Escape of Ident.unscoped in
  let rec aux = function
      Pident id ->
        begin match Ident.get_unscoped id with
        | None -> ()
        | Some us ->
            if Ident.UnscopedSet.exists (Ident.same_unscoped us) idl
            then ()
            else raise (Escape us)
        end
    | Pdot (p, _) | Pextra_ty (p, _) -> aux p
    | Papply (p1, p2) -> aux p1; aux p2
  in match aux p with
    | () -> None
    | exception Escape id -> Some id

let kfalse _ = false

let maybe_escape s =
  if Lexer.is_keyword s then "\\#" ^ s else s

let rec name ?(paren=kfalse) = function
    Pident id -> maybe_escape (Ident.name id)
  | Pdot(p, s) | Pextra_ty (p, Pcstr_ty s) ->
      let s = maybe_escape s in
      name ~paren p ^ if paren s then ".( " ^ s ^ " )" else "." ^ s
  | Papply(p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"
  | Pextra_ty (p, Pext_ty) -> name ~paren p

let rec print ppf = function
  | Pident id -> Ident.print_with_scope ppf id
  | Pdot(p, s) | Pextra_ty (p, Pcstr_ty s) ->
      Format_doc.fprintf ppf "%a.%s" print p s
  | Papply(p1, p2) -> Format_doc.fprintf ppf "%a(%a)" print p1 print p2
  | Pextra_ty (p, Pext_ty) -> print ppf p

let rec head = function
    Pident id -> id
  | Pdot(p, _) | Pextra_ty (p, _) -> head p
  | Papply _ -> assert false

let flatten =
  let rec flatten acc = function
    | Pident id -> `Ok (id, acc)
    | Pdot (p, s) | Pextra_ty (p, Pcstr_ty s) -> flatten (s :: acc) p
    | Papply _ -> `Contains_apply
    | Pextra_ty (p, Pext_ty) -> flatten acc p
  in
  fun t -> flatten [] t

let heads p =
  let rec heads p acc = match p with
    | Pident id -> id :: acc
    | Pdot (p, _) | Pextra_ty (p, _) -> heads p acc
    | Papply(p1, p2) ->
        heads p1 (heads p2 acc)
  in heads p []

let rec last = function
  | Pident id -> Ident.name id
  | Pdot(_, s) | Pextra_ty (_, Pcstr_ty s) -> s
  | Papply(_, p) | Pextra_ty (p, Pext_ty) -> last p

let is_constructor_typath p =
  match p with
  | Pident _ | Pdot _ | Papply _ -> false
  | Pextra_ty _ -> true

module T = struct
  type nonrec t = t
  let compare = compare
end
module Set = Set.Make(T)
module Map = Map.Make(T)
