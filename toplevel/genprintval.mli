(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Printing of values *)

open Types

module type OBJ =
  sig
    type t

    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
  end

module type S =
  sig
    type t

    val install_printer : Path.t -> Types.type_expr -> (t -> unit) -> unit
    val remove_printer : Path.t -> unit

    val print_exception : t -> unit
    val print_value :
          int -> int -> (int -> t -> Types.type_expr -> bool) ->
          Env.t -> t -> type_expr -> unit
  end

module Make(O : OBJ) : (S with type t = O.t)
