(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Lazy]: deferred computations *)

type 'a status =
  | Delayed of (unit -> 'a)
  | Value of 'a
  | Exception of exn
;;

type 'a delayed = 'a status ref;;

let _lazy f = ref (Delayed f);;

let force l =
  match !l with
  | Value v -> v
  | Exception e -> raise e
  | Delayed f ->
      try let v = f () in l := Value v; v
      with e -> l := Exception e; raise e
;;
