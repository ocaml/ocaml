(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

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
    type valu
    val eval_path: Env.t -> Path.t -> valu
    exception Error
    val same_value: valu -> valu -> bool
  end

module type S =
  sig
    type t
    val install_printer :
          Path.t -> Types.type_expr -> (formatter -> t -> unit) -> unit
    val remove_printer : Path.t -> unit
    val outval_of_untyped_exception : t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> t -> type_expr -> Outcometree.out_value
  end

module Make(O : OBJ)(EVP : EVALPATH with type valu = O.t) :
         (S with type t = O.t)
