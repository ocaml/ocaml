(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Printing of values *)

open Types
open Format

module type OBJ =
  sig
    type t
    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
  end

module type EVALPATH =
  sig
    type value
    val eval_path: Path.t -> value
    exception Error
    val same_value: value -> value -> bool
  end

module type S =
  sig
    type t
    val install_printer :
          Path.t -> Types.type_expr -> (formatter -> t -> unit) -> unit
    val remove_printer : Path.t -> unit
    val print_untyped_exception : formatter -> t -> unit
    val print_value :
          int -> int -> (int -> t -> Types.type_expr -> bool) ->
          Env.t -> t -> formatter -> type_expr -> unit
  end

module Make(O : OBJ)(EVP : EVALPATH with type value = O.t) :
         (S with type t = O.t)
