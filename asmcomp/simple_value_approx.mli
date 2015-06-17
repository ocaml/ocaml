(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Simple approximations to the runtime results of computations.
    This pass is designed for speed rather than accuracy; the performance
    is important since it is used heavily during inlining. *)

open Abstract_identifiers

module Tag : sig
  type t

  val create_exn : int -> t
  val to_int : t -> int
end

type 'a boxed_int = 'a Flambdaexport.boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

(* Note about mutable blocks:

   Mutable blocks are always represented by [Value_unknown] or
   [Value_bottom]. If something else is propagated here, then
   whe know that some miscompilation could happen.
   This is probably an user using [Obj.magic] or [Obj.set_field] in
   an inappropriate situation.
   Such a situation could be
   [let x = (1,1) in
    Obj.set_field (Obj.repr x) 0 (Obj.repr 2);
    assert(fst x = 2)]
   The user would probably expect the assertion to be true, but the
   compiler could propagate the value of [x].
   This certainly won't always prevent this kind of errors, but may
   prevent most of them.

   This may not be completely correct as some correct unreachable
   code branch could also trigger it. But the likelyness seems small
   enouth to prefer to catch those errors.

   example of such a problematic pattern could be
   [type a = { a : int }
    type b = { mutable b : int }
    type _ t =
      | A : a t
      | B : b t
    let f (type x) (v:x t) (r:x) =
      match v with
      | A -> r.a
      | B -> r.b <- 2; 3

   let v =
   let r =
     ref A in
     r := A; (* Some pattern that the compiler can't understand *)
     f !r { a = 1 }]
   when inlining [f], the B branch is unreachable, yet the compiler
   can't prove it and needs to keep it.
*)

(** A value of type [t] corresponds to an approximation of a value.
    Such approximations are deduced at particular points in an expression
    tree, but may subsequently be propagated to other locations.

    At the point at which an approximation is built for some value [v], we can
    construct a set of variables (call the set [S]) that are known to alias the
    same value [v].  Each member of [S] will have the same or a more precise
    [descr] field in its approximation relative to the approximation for [v].
    (An increase in precision may currently be introduced for pattern
    matches.)  If [S] is non-empty then it is guaranteed that there is a
    unique member of [S] that was declared in a scope further out ("earlier")
    than all other members of [S].  If such a member exists then it is
    recorded in the [var] field.  Otherwise [var] is [None].

    Analogous to the construction of the set [S], we can construct a set [T]
    consisting of all symbols that are known to alias the value whose
    approximation is being constructed.  If [T] is non-empty then the
    [symbol] field is set to some member of [T]; it does not matter which
    one.  (There is no notion of scope for symbols.)
*)
type t = {
  descr : descr;
  var : Variable.t option;
  symbol : Symbol.t option;
}

and descr = private
  | Value_block of Tag.t * t array
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_set_of_closures of value_set_of_closures
  | Value_closure of value_closure
  | Value_string of Flambdaexport.value_string
  | Value_float_array of int (* size *)
  | Value_unknown
  | Value_bottom
  | Value_extern of Flambdaexport.ExportId.t
  | Value_symbol of Symbol.t
  | Value_unresolved of Symbol.t (* No description was found for this symbol *)

and value_closure = {
  closure_id : Closure_id.t;
  set_of_closures : value_set_of_closures;
  set_of_closures_var : Variable.t option;
}

and value_set_of_closures = {
  function_decls : Expr_id.t Flambda.function_declarations;
  bound_var : t Var_within_closure.Map.t;
  unchanging_params : Variable.Set.t;
  specialised_args : Variable.Set.t;
  alpha_renaming :
    Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures.t;
}

(** Smart constructors *)

val value_unknown : t
val value_int : int -> t
val value_float : float -> t
val value_boxed_int : 'i boxed_int -> 'i -> t
val value_constptr : int -> t
val value_closure : value_closure -> t
val value_set_of_closures : value_set_of_closures -> t
val value_block : Tag.t * t array -> t
val value_extern : Flambdaexport.ExportId.t -> t
val value_symbol : Symbol.t -> t
val value_bottom : t
val value_unresolved : Symbol.t -> t

val const_approx : Flambda.const -> t

val print_approx : Format.formatter -> t -> unit

val make_const_int : int -> 'a -> 'a Flambda.t * t
val make_const_ptr : int -> 'a -> 'a Flambda.t * t
val make_const_bool : bool -> 'a -> 'a Flambda.t * t
val make_const_float : float -> 'a -> 'a Flambda.t * t
val make_const_boxed_int : 'i boxed_int -> 'i -> 'a -> 'a Flambda.t * t

val meet : t -> t -> t

val descr : t -> descr

(* An approximation is "known" iff it is not [Value_unknown]. *)
val known : t -> bool

(* An approximation is "useful" iff it is neither unknown nor bottom. *)
val useful : t -> bool

(* A value is certainly immutable if its approximation is known and not bottom.
   It should have been resolved (it cannot be [Value_extern] and
   [Value_symbol] *)
val is_certainly_immutable : t -> bool

val check_constant_result
   : Expr_id.t Flambda.t
  -> t
  -> Expr_id.t Flambda.t * t

val check_var_and_constant_result
   : is_present_in_env:(Variable.t -> bool)
  -> Expr_id.t Flambda.t
  -> t
  -> Expr_id.t Flambda.t * t

val get_field : int -> t list -> t

val descrs : t list -> descr list

module Import : sig
  val really_import : descr -> descr
  val import_global : Ident.t -> t
  val import_symbol : Symbol.t -> t
end

val really_import_approx : t -> t
