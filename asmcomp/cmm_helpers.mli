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

open Cmm

(** [bind name arg fn] is equivalent to [let name = arg in fn name],
    or simply [fn arg] if [arg] is simple enough *)
val bind :
  string -> expression -> (expression -> expression) -> expression

(** Same as [bind], but also treats loads from a variable as simple *)
val bind_load :
  string -> expression -> (expression -> expression) -> expression

(** Same as [bind], but does not treat variables as simple *)
val bind_nonvar :
  string -> expression -> (expression -> expression) -> expression

(** Headers *)

(** A null header with GC bits set to black *)
val caml_black : nativeint

(** A constant equal to the tag for float arrays *)
val floatarray_tag : Debuginfo.t -> expression

(** [block_header tag size] creates a header with tag [tag] for a
    block of size [size] *)
val block_header : int -> int -> nativeint

(** Same as block_header, but with GC bits set to black *)
val black_block_header : int -> int -> nativeint

(** Closure headers of the given size *)
val white_closure_header : int -> nativeint
val black_closure_header : int -> nativeint

(** Infix header at the given offset *)
val infix_header : int -> nativeint

(** Header for a boxed float value *)
val float_header : nativeint

(** Header for an unboxed float array of the given size *)
val floatarray_header : int -> nativeint

(** Header for a string (or bytes) of the given length *)
val string_header : int -> nativeint

(** Boxed integers headers *)
val boxedint32_header : nativeint
val boxedint64_header : nativeint
val boxedintnat_header : nativeint

(** Wrappers *)
val alloc_float_header : Debuginfo.t -> expression
val alloc_floatarray_header : int -> Debuginfo.t -> expression
val alloc_closure_header : int -> Debuginfo.t -> expression
val alloc_infix_header : int -> Debuginfo.t -> expression
val alloc_boxedint32_header : Debuginfo.t -> expression
val alloc_boxedint64_header : Debuginfo.t -> expression
val alloc_boxedintnat_header : Debuginfo.t -> expression

(** Integers *)

(** Minimal/maximal OCaml integer values whose backend representation fits
    in a regular OCaml integer *)
val max_repr_int : int
val min_repr_int : int

(** Make an integer constant from the given integer (tags the integer) *)
val int_const : Debuginfo.t -> int -> expression
val cint_const : int -> data_item
val targetint_const : int -> Targetint.t

(** Make a Cmm constant holding the given nativeint value.
    Uses [Cconst_int] instead of [Cconst_nativeint] when possible
    to preserve peephole optimisations. *)
val natint_const_untagged : Debuginfo.t -> Nativeint.t -> expression

(** Add an integer to the given expression *)
val add_const : expression -> int -> Debuginfo.t -> expression

(** Increment/decrement of integers *)
val incr_int : expression -> Debuginfo.t -> expression
val decr_int : expression -> Debuginfo.t -> expression

(** Simplify the given expression knowing its last bit will be
    irrelevant *)
val ignore_low_bit_int : expression -> expression

(** Arithmetical operations on integers *)
val add_int : expression -> expression -> Debuginfo.t -> expression
val sub_int : expression -> expression -> Debuginfo.t -> expression
val lsl_int : expression -> expression -> Debuginfo.t -> expression
val mul_int : expression -> expression -> Debuginfo.t -> expression
val lsr_int : expression -> expression -> Debuginfo.t -> expression
val asr_int : expression -> expression -> Debuginfo.t -> expression
val div_int :
  expression -> expression -> Lambda.is_safe -> Debuginfo.t -> expression
val mod_int :
  expression -> expression -> Lambda.is_safe -> Debuginfo.t -> expression

(** Integer tagging
    [tag_int] and [force_tag_int] are functionnaly equivalent, but
    produce syntactically different expressions ([tag_int] produces
    an addition, while [force_tag_int] produces a logical or).
    The difference marks the fact that the shift operation in [tag_int]
    is assumed not to overflow, and so [untag_int (tag_int i)] can be
    simplified to [i]. With [force_tag_int], the initial shift might
    overflow, so the above simplification would be wrong. *)
val tag_int : expression -> Debuginfo.t -> expression
val force_tag_int : expression -> Debuginfo.t -> expression

(** Integer untagging *)
val untag_int : expression -> Debuginfo.t -> expression

(** Specific division operations for boxed integers *)
val safe_div_bi :
  Lambda.is_safe ->
  expression ->
  expression ->
  Primitive.boxed_integer ->
  Debuginfo.t ->
  expression
val safe_mod_bi :
  Lambda.is_safe ->
  expression ->
  expression ->
  Primitive.boxed_integer ->
  Debuginfo.t ->
  expression

(** If-Then-Else expression
    [mk_if_then_else dbg cond ifso_dbg ifso ifnot_dbg ifnot] associates
    [dbg] to the global if-then-else expression, [ifso_dbg] to the
    then branch [ifso], and [ifnot_dbg] to the else branch [ifnot] *)
val mk_if_then_else :
  Debuginfo.t ->
  expression ->
  Debuginfo.t -> expression ->
  Debuginfo.t -> expression ->
  expression

(** Boolean negation *)
val mk_not : Debuginfo.t -> expression -> expression

(** Exception raising *)
val raise_regular : Debuginfo.t -> expression -> expression
val raise_symbol : Debuginfo.t -> string -> expression

(** Convert a tagged integer into a raw integer with boolean meaning *)
val test_bool : Debuginfo.t -> expression -> expression

(** Float boxing and unboxing *)
val box_float : Debuginfo.t -> expression -> expression
val unbox_float : Debuginfo.t -> expression -> expression

(** Map the given function over a Ccatch expression's handlers and body *)
val map_ccatch :
  (expression -> expression) -> rec_flag ->
  (int * (Backend_var.With_provenance.t * machtype) list * expression
   * Debuginfo.t) list ->
  expression -> expression

(** Complex number creation and access *)
val box_complex : Debuginfo.t -> expression -> expression -> expression
val complex_re : expression -> Debuginfo.t -> expression
val complex_im : expression -> Debuginfo.t -> expression

(** Make the given expression return a unit value *)
val return_unit : Debuginfo.t -> expression -> expression

(** Remove a trailing unit return if any *)
val remove_unit : expression -> expression

