(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Lazy]: deferred computations *)

type 'a status =
  | Delayed of (unit -> 'a)
  | Value of 'a
  | Exception of exn
;;

type 'a t = 'a status ref;;

let force l =
  match !l with
  | Value v -> v
  | Exception e -> raise e
  | Delayed f ->
      try let v = f () in l := Value v; v
      with e -> l := Exception e; raise e
;;
