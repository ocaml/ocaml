(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Printing of values *)

open Types
open Format

module type OBJ =
  sig
    type t
    val repr : 'a -> t
    (* [base_obj] assumes that the value has a marshallable base type. *)
    val base_obj : t -> 'a
    val obj : t -> (Obj.t, string) result
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
    val double_array_tag : int
    val double_field : t -> int -> float
  end

module type EVALPATH =
  sig
    type valu
    val eval_address: Env.address -> valu
    exception Error
    val same_value: valu -> valu -> bool
  end

module User_printer : sig
  type ('a, 'b) gen =
    | Zero of 'b
    | Succ of ('a -> ('a, 'b) gen)

  val install_simple :
        Path.t -> Types.type_expr -> (formatter -> Obj.t -> unit) -> unit
  val install_generic_outcometree :
        Path.t -> Path.t ->
        (int -> (int -> Obj.t -> Outcometree.out_value,
                 Obj.t -> Outcometree.out_value) gen) ->
        unit
  val install_generic_format :
         Path.t -> Path.t ->
         (formatter -> Obj.t -> unit,
          formatter -> Obj.t -> unit) gen ->
         unit
  (** [install_generic_format function_path constructor_path printer]
      function_path is used to remove the printer. *)

  val remove : Path.t -> unit
end

module type S =
  sig
    type t
    val outval_of_untyped_exception : t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> t -> type_expr -> Outcometree.out_value
  end

module Make(O : OBJ)(_ : EVALPATH with type valu = O.t) :
         (S with type t = O.t)
