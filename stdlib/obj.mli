(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Operations on internal representations of values.

   Not for the casual user.
*)

type t

type raw_data = nativeint  (* @since 4.12 *)

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
val [@inline always] is_block : t -> bool
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "caml_obj_tag" [@@noalloc]
external size : t -> int = "%obj_size"
external reachable_words : t -> int = "caml_obj_reachable_words"
  (**
     Computes the total size (in words, including the headers) of all
     heap blocks accessible from the argument.  Statically
     allocated blocks are included.

     @since 4.04
  *)

external field : t -> int -> t = "%obj_field"

(** When using flambda:

    [set_field] MUST NOT be called on immutable blocks.  (Blocks allocated
    in C stubs, or with [new_block] below, are always considered mutable.)

    The same goes for [set_double_field].

    For experts only:
    [set_field] et al can be made safe by first wrapping the block in
    {!Sys.opaque_identity}, so any information about its contents will not
    be propagated.
*)
external set_field : t -> int -> t -> unit = "%obj_set_field"
external compare_and_swap_field : t -> int -> t -> t -> bool
  = "caml_obj_compare_and_swap"
external is_shared : t -> bool = "caml_obj_is_shared"

val [@inline always] double_field : t -> int -> float  (* @since 3.11.2 *)
val [@inline always] set_double_field : t -> int -> float -> unit
  (* @since 3.11.2 *)

external raw_field : t -> int -> raw_data = "caml_obj_raw_field"
  (* @since 4.12 *)
external set_raw_field : t -> int -> raw_data -> unit
                                          = "caml_obj_set_raw_field"
  (* @since 4.12 *)

external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external add_offset : t -> Int32.t -> t = "caml_obj_add_offset"
         (* @since 3.12 *)
external with_tag : int -> t -> t = "caml_obj_with_tag"
  (* @since 4.09 *)

val first_non_constant_constructor_tag : int
val last_non_constant_constructor_tag : int

val forcing_tag : int
val cont_tag : int
val lazy_tag : int
val closure_tag : int
val object_tag : int
val infix_tag : int
val forward_tag : int
val no_scan_tag : int
val abstract_tag : int
val string_tag : int   (* both [string] and [bytes] *)
val double_tag : int
val double_array_tag : int
val custom_tag : int

val int_tag : int
val out_of_heap_tag : int
val unaligned_tag : int   (* should never happen @since 3.11 *)

module Closure : sig
  type info = {
    arity: int;
    start_env: int;
  }
  val info : t -> info
end

module Extension_constructor :
sig
  type t = extension_constructor
  val of_val : 'a -> t
  val [@inline always] name : t -> string
  val [@inline always] id : t -> int
end

module Ephemeron: sig
  (** Ephemeron with arbitrary arity and untyped *)

  type obj_t = t
  (** alias for {!Obj.t} *)

  type t
  (** an ephemeron cf {!Ephemeron} *)

  val create: int -> t
  (** [create n] returns an ephemeron with [n] keys.
      All the keys and the data are initially empty.
      The argument [n] must be between zero
      and {!max_ephe_length} (limits included).
  *)

  val length: t -> int
  (** return the number of keys *)

  val get_key: t -> int -> obj_t option

  val get_key_copy: t -> int -> obj_t option

  val set_key: t -> int -> obj_t -> unit

  val unset_key: t -> int -> unit

  val check_key: t -> int -> bool

  val blit_key : t -> int -> t -> int -> int -> unit

  val get_data: t -> obj_t option

  val get_data_copy: t -> obj_t option

  val set_data: t -> obj_t -> unit

  val unset_data: t -> unit

  val check_data: t -> bool

  val blit_data : t -> t -> unit

  val max_ephe_length: int
  (** Maximum length of an ephemeron, ie the maximum number of keys an
      ephemeron could contain *)
end
