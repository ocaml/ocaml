(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

exception Error of string
exception Cyclic


type 'a node = int

type 'a t_node =
   {
    mutable info : 'a ;
    mutable succ : int list ;
    mutable pred : int list ;
   } 

type 'a t = ('a t_node) Extarray.t * (int, unit) Hashtbl.t

let create x = Extarray.create {info=x ; succ=[] ; pred=[]}, Hashtbl.create 17

let maxnode = (1 lsl 15) - 1

let new_node (t,_) x = 
  begin if Extarray.size t > maxnode then raise (Error "Too many nodes") end ;
  Extarray.emit t {info=x ; succ=[] ; pred=[]}

let new_edge (t,e) n1 n2 =
  try
    let key = (n1 lsl 15) lor n2 in
    if not (Hashtbl.mem e key) then begin
      Hashtbl.add e key () ;
      let t_n1 = Extarray.get t n1
      and t_n2 = Extarray.get t n2 in
      t_n1.succ <- n2 :: t_n1.succ ;
      t_n2.pred <- n1 :: t_n2.pred ;
    end
  with
  | Extarray.Error -> raise (Error "new_edge")


let rec interval i j =
  if i >= j then []
  else
    i::interval (i+1) j

let nodes (t, _) = interval 0 (Extarray.size t)

let info (t,_) n =
  try
    let t_n = Extarray.get t n in
    t_n.info
  with
  | Extarray.Error -> raise (Error "info")

let set_info (t,_) n i =
  try 
    let t_n = Extarray.get t n in
    t_n.info <- i
  with
  | Extarray.Error -> raise (Error "set_info")


let succ (t,_) n = 
  try
    let t_n = Extarray.get t n in
    t_n.succ
  with
  | Extarray.Error -> raise (Error "succ")

let prec (t,_) n =
  try
    let t_n = Extarray.get t n in
    t_n.pred
  with
  | Extarray.Error -> raise (Error "prec")

let iter (t,_) f =
  for i=0 to Extarray.size t-1 do
    f i
  done

let debug chan f (t,_) =
  for i=0 to Extarray.size t-1 do
    Printf.fprintf chan "%d ->" i ;
    begin try
      let node = Extarray.get t i in
      List.iter (fun i -> Printf.fprintf chan " %d," i) node.succ ;
    with Extarray.Error -> assert false
    end ;
    output_char chan '\n' ;
    f chan i
  done

open Printf

let print_node n = print_int n

let dump chan g s =
  let name = string_of_int in
  let pnode n =
    let succs = succ g n in
    List.iter
      (fun m -> fprintf chan "%s%s -> %s%s\n" s (name n) s (name m))
      succs in
  iter g pnode

let dump_info chan g s print_info =
  let name = string_of_int in
  let pnode n =
    let i = info g n in
    fprintf chan "%s%s: %a\n" s (name n) print_info i
  in
  iter g pnode


module Bag = struct

  type 'a t = 'a Stack.t

  exception Empty

  let create () = Stack.create ()
  let put b x = Stack.push x b
  let get b = 
    try
      Stack.pop b
    with
      Stack.Empty -> raise Empty
  let is_empty b = Stack.is_empty b

end


let top_sort t =
  let sorted = Queue.create () in
  let tbl = Hashtbl.create 10 in
  let bag = Bag.create () in
  let ns = nodes t in
  let rec init ns =
    match ns with
    | [] -> ()
    | hd::tl -> 
	let preds = prec t hd in
	(match preds with
	| [] -> Bag.put bag hd
	| _ -> Hashtbl.add tbl hd (List.length preds)
	      );
	init tl in
  init ns;
  if (Bag.is_empty bag) then raise Cyclic;
  let rec sort () =
    if not (Bag.is_empty bag)
    then(
      let node = Bag.get bag in
      Queue.add node sorted;
      let succs = succ t node in
      let rec adjust succs =
	(match succs with
	| [] -> ()
	| hd::tl -> 
	    let count = Hashtbl.find tbl hd in
	    if (count = 1)
	    then(
	      Hashtbl.remove tbl hd;
	      Bag.put bag hd
		)
	    else(
	      Hashtbl.replace tbl hd (count-1)
		);
	    adjust tl )in
      adjust succs;
      sort ()
 	)
    else(
      if not (Queue.length sorted = List.length ns)
      then raise Cyclic
	  )
  in
  sort ();
  Queue.fold 
    (fun n_ls node -> n_ls @ [node]) [] sorted
