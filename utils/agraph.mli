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

(***************************)
(* Simple directed graphs  *)
(***************************)


exception Error of string


type 'a node
and 'a t

val create : 'a -> 'a t
(*
  ``create i'' creates an empty graph
   whose nodes will hold information of type t, where
   t is the type of i
*)

val new_node : 'a t -> 'a -> 'a node
(* ``new_node g i'' add node whose information is i
   raises Graph.Error when there are too many nodes
*)

val new_edge : 'a t -> 'a node -> 'a node -> unit
(* ``new_edge g n1 n2'' add an edge from n1 to n2
   If the specified edge already exists, nothing occurs
   raises Graph.Error when some node does not exist
*)

val nodes : 'a t -> 'a node list
(* All nodes, in creation order *)

val info : 'a t -> 'a node -> 'a
val set_info : 'a t -> 'a node -> 'a -> unit
(* Read/modify the info field of a node
   raises Graph.Error when node does not exist
*)

val succ : 'a t -> 'a node -> 'a node list
val prec : 'a t -> 'a node -> 'a node list
(* Predecessors and successors of a node
   raises Graph.Error when node does not exist
*)

val iter : 'a t -> ('a node -> unit) -> unit
(* ``iter g f'' iterates f on g nodes (following creation order) *)

(***************************)
(* various debug printings *)
(***************************)

val debug : out_channel -> (out_channel -> 'a node -> unit) -> 'a t -> unit
val dump : out_channel -> 'a t -> string -> unit
val dump_info : out_channel -> 'a t -> string -> (out_channel -> 'a -> unit) -> unit

exception Cyclic
val top_sort : 'a t -> ('a node) list
(* Topological sort, raises exception Cyclic when input graph has cycle*)
