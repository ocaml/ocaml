(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

exception Empty

type 'a queue_cell =
    Nil
  | Cons of 'a * 'a queue_cell ref

type 'a t =
  { mutable head: 'a queue_cell;
    mutable tail: 'a queue_cell }

let new () =
  { head = Nil; tail = Nil }

let clear q =
  q.head <- Nil; q.tail <- Nil

let add x q =
  match q.tail with
    Nil ->                              (* if tail = Nil then head = Nil *)
      let c = Cons(x, ref Nil) in
      q.head <- c; q.tail <- c
  | Cons(_, newtailref) ->
      let c = Cons(x, ref Nil) in
      newtailref := c;
      q.tail <- c

let peek q =
  match q.head with
    Nil ->
      raise Empty
  | Cons(x, _) ->
      x

let take q =
  match q.head with
    Nil ->
      raise Empty
  | Cons(x, rest) ->
      q.head <- !rest;
      begin match !rest with
        Nil -> q.tail <- Nil
      |  _  -> ()
      end;
      x

let rec length_aux = function
    Nil -> 0
  | Cons(_, rest) -> succ (length_aux !rest)

let length q = length_aux q.head

let rec iter_aux f = function
    Nil ->
      ()
  | Cons(x, rest) ->
      f x; iter_aux f !rest

let iter f q = iter_aux f q.head
