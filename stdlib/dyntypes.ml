(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type stype =
  | DT_int
  | DT_string
  | DT_float
  | DT_tuple of stype list
  | DT_node of node * stype list
  | DT_var of int

and record_representation =
  | Record_regular
  | Record_float

and mutable_flag =
  | Mutable
  | Immutable

and node = {
    node_id: string;
    node_definition: node_definition;
   }

and node_definition =
  | DT_record of record_definition
  | DT_variant of variant_definition

and record_definition = {
    record_representation:  record_representation;
    record_fields: (string * mutable_flag * stype) list;
   }

and variant_definition = {
    variant_constructors: (string * stype list) list;
   }


type 'a ttype = stype

let stype_of_ttype (x : 'a ttype) : stype = x

module NodePairHash = Hashtbl.Make
    (struct
      type t = node * node
      let equal (n1, n2) (n1', n2') =
        n1 == n1' && n2 == n2'
      let hash (n1, n2) =
        Hashtbl.hash (n1.node_id, n2.node_id)
    end)

module TypEq = struct
  type ('a, 'b) t = unit
  let refl = ()
  let trans () () = ()
  let sym () = ()

  let app () x = Obj.magic x
end

let stype_equality () =
  let checked = NodePairHash.create 8 in
  let list f l1 l2 =
    if List.length l1 <> List.length l2 then raise Exit;
    List.iter2 f l1 l2
  in
  let rec aux t1 t2 =
    if t1 == t2 then ()
    else
    match t1, t2 with
    | DT_tuple tyl1, DT_tuple tyl2 -> list aux tyl1 tyl2
    | DT_node (n1, tyl1), DT_node (n2, tyl2) -> node n1 n2; list aux tyl1 tyl2
    | DT_var i1, DT_var i2 when i1 = i2 -> ()
    | _ -> raise Exit
  and node n1 n2 =
    if n1 == n2 || NodePairHash.mem checked (n1, n2) then ()
    else begin
      NodePairHash.add checked (n1, n2) ();
      match n1.node_definition, n2.node_definition with
      | DT_record r1, DT_record r2 when r1.record_representation = r2.record_representation ->
          list field r1.record_fields r2.record_fields
      | DT_variant v1, DT_variant v2 ->
          list constructor v1.variant_constructors v2.variant_constructors
      | _ -> raise Exit
    end
  and constructor (c1, tl1) (c2, tl2) =
    if c1 <> c2 then raise Exit;
    list aux tl1 tl2
  and field (f1, mut1, t1) (f2, mut2, t2) =
    if f1 <> f2 || mut1 <> mut2 then raise Exit;
    aux t1 t2
  in
  fun t1 t2 ->
    try aux t1 t2; true
    with Exit -> false

let equal () =
  let eq = stype_equality () in
  fun t1 t2 -> if eq t1 t2 then Some () else None


module type DYN = sig
  type t
  val x: t
  val t: t ttype
end

type dyn = (module DYN)

let dyn (type s) t x =
  let module M = struct
    type t = s
    let x = x
    let t = t
  end
  in
  (module M : DYN)

type head =
  | DV_int of int
  | DV_string of string
  | DV_float of float
  | DV_tuple of dyn list
  | DV_record of (string * dyn) list
  | DV_constructor of string * dyn list

let rec subst s =
  let rec aux = function
    | DT_int _ | DT_string _ | DT_float _ as t -> t
    | DT_tuple tl -> DT_tuple (List.map aux tl)
    | DT_node (node, tl) -> DT_node (node, List.map aux tl)
    | DT_var i -> s.(i)
  in
  aux

let inspect d =
  let module M = (val d : DYN) in
  match M.t with
  | DT_int -> DV_int (Obj.magic M.x)
  | DT_string -> DV_string (Obj.magic M.x)
  | DT_float -> DV_float (Obj.magic M.x)
  | DT_tuple tl -> DV_tuple (List.map2 dyn tl (Array.to_list (Obj.magic M.x)))
  | DT_node (node, tyl) ->
      let s = subst (Array.of_list tyl) in
      begin match node.node_definition with
      | DT_record {record_fields = l} ->
          DV_record (List.map2 (fun (lab, _mut, t) x -> lab, dyn (s t) x) l (Array.to_list (Obj.magic M.x)))
      | DT_variant {variant_constructors = l} ->
          let x = Obj.repr M.x in
          let (cst, n) = if Obj.is_int x then true, Obj.magic x else false, Obj.tag x in
          let rec find n = function
            | ((_, tl) as c) :: rest ->
                let n = if cst = (tl == []) then n - 1 else n in
                if n < 0 then c else find n rest
            | [] ->
                assert false
          in
          let (c, tl) = find n l in
          let args =
            if cst then []
            else List.map2 (fun t x -> dyn (s t) x) tl (Array.to_list (Obj.magic M.x))
          in
          DV_constructor (c, args)
      end
  | DT_var _ -> assert false

let tuple l =
  let l = List.map (fun d -> let module M = (val d : DYN) in stype_of_ttype M.t, Obj.magic M.x) l in
  let tl, vl = List.split l in
  dyn (DT_tuple tl) (Array.of_list vl)

