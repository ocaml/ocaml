(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format

type t = { mutable stamp: int; mutable name: string; mutable global: bool }

(* A stamp of 0 denotes a persistent identifier *)

let currentstamp = ref 0

let create s =
  incr currentstamp;
  { name = s; stamp = !currentstamp; global = false }

let create_persistent s =
  { name = s; stamp = 0; global = true }

let name i = i.name

let unique_name i = i.name ^ "_" ^ string_of_int i.stamp

let persistent i = (i.stamp = 0)

let equal i1 i2 = i1.name = i2.name

let same i1 i2 = i1 = i2
  (* Possibly more efficient version (with a real compiler, at least):
       if i1.stamp <> 0
       then i1.stamp = i2.stamp
       else i2.stamp = 0 & i1.name = i2.name *)

let binding_time i = i.stamp

let current_time() = !currentstamp

let identify i1 i2 f =
  let name1 = i1.name and stamp1 = i1.stamp in
  try
    i1.name <- i2.name;
    i1.stamp <- i2.stamp;
    let res = f () in
    i1.name <- name1;
    i1.stamp <- stamp1;
    res
  with x ->
    i1.name <- name1;
    i1.stamp <- stamp1;
    raise x

let hide i =
  { stamp = -1; name = i.name; global = i.global }

let make_global i =
  i.global <- true

let global i =
  i.global

let print i =
  print_string i.name;
  match i.stamp with
    0 -> print_string "!"
  | -1 -> print_string "#"
  | n -> print_string "/"; print_int n; if i.global then print_string "g"

type 'a tbl =
    Empty
  | Node of 'a tbl * 'a data * 'a tbl * int

and 'a data =
  { ident: t;
    data: 'a;
    previous: 'a data option }

let empty = Empty

(* Inline expansion of height for better speed
 * let height = function
 *     Empty -> 0
 *   | Node(_,_,_,h) -> h
 *)

let mknode l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, d, r, (if hl >= hr then hl + 1 else hr + 1))

let balance l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 1 then
    let (Node(ll, ld, lr, _)) = l in
    if (match ll with Empty -> 0 | Node(_,_,_,h) -> h) >=
       (match lr with Empty -> 0 | Node(_,_,_,h) -> h) then
      mknode ll ld (mknode lr d r)
    else
      let (Node(lrl, lrd, lrr, _)) = lr in
      mknode (mknode ll ld lrl) lrd (mknode lrr d r)
  else if hr > hl + 1 then
    let (Node(rl, rd, rr, _)) = r in
    if (match rr with Empty -> 0 | Node(_,_,_,h) -> h) >=
       (match rl with Empty -> 0 | Node(_,_,_,h) -> h) then
      mknode (mknode l d rl) rd rr
    else
      let (Node(rll, rld, rlr, _)) = rl in
      mknode (mknode l d rll) rld (mknode rlr rd rr)
  else
    mknode l d r

let rec add id data = function
    Empty ->
      Node(Empty, {ident = id; data = data; previous = None}, Empty, 1)
  | Node(l, k, r, h) ->
      let c = compare id.name k.ident.name in
      if c = 0 then
        Node(l, {ident = id; data = data; previous = Some k}, r, h)
      else if c < 0 then
        balance (add id data l) k r
      else
        balance l k (add id data r)

let rec find_stamp s = function
    None ->
      raise Not_found
  | Some k ->
      if k.ident.stamp = s then k.data else find_stamp s k.previous

let rec find_same id = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare id.name k.ident.name in
      if c = 0 then
        if id.stamp = k.ident.stamp
        then k.data
        else find_stamp id.stamp k.previous
      else
        find_same id (if c < 0 then l else r)

let rec find_name name = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare name k.ident.name in
      if c = 0 then
        k.data
      else
        find_name name (if c < 0 then l else r)

let rec iter fn = function
    Empty -> ()
  | Node(l, k, r, _) ->
      iter fn l; iter_node fn k; iter fn r
and iter_node fn k =
  fn k.ident k.data;
  match k.previous with None -> () | Some prev_k -> iter_node fn prev_k

let print_tbl print_elt tbl =
  open_box 2;
  print_string "[[";
  iter (fun id data -> 
          open_box 2;
          print id; print_string " ->"; print_space(); print_elt data;
          print_string ";"; close_box(); print_space())
       tbl;
  print_string "]]";
  close_box()

