(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*   Rodolphe Lepigre, projet Deducteam, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The OCaml runtime assumes for type-directed optimizations that all types
    are "separable". A type is "separable" if either all its inhabitants
    (the values of this type) are floating-point numbers, or none of them are.

    (Note: This assumption is required for the dynamic float array optimization;
    it is only made if Config.flat_float_array is set,
    otherwise the code in this module becomes trivial
    -- see {!compute_decl}.)

    This soundness requirement could be broken by type declarations mixing
    existentials and the "[@@unboxed]" annotation. Consider the declaration

    {[
       type any = Any : 'a -> any [@@unboxed]
    ]}

   which corresponds to the existential type "exists a. a". If this type is
   allowed to be unboxed, then it is inhabited by both [float] values
   and non-[float] values. On the contrary, if unboxing is disallowed, the
   inhabitants are all blocks with the [Any] constructors pointing to its
   parameter: they may point to a float, but they are not floats.

   The present module contains a static analysis ensuring that declarations
   annotated with "[@@unboxed]" can be safely unboxed. The idea is to check
   the "separability" (in the above sense) of the argument type that would
   be unboxed, and reject the unboxed declaration if it would create a
   non-separable type.

   Checking mutually-recursive type declarations is a bit subtle.
   Consider, for example, the following declarations.

   {[
      type foo = Foo : 'a t -> foo   [@@unboxed]
      and 'a t = ...
   ]}

   Deciding whether the type [foo] should be accepted requires inspecting
   the declaration of ['a t], which may itself refer to [foo] in turn.
   In general, the analysis performs a fixpoint computation. It is somewhat
   similar to what is done for inferring the variance of type parameters.

   Our analysis is defined using inference rules for our judgment
   [Def; Gamma |- t : m], in which a type expression [t] is checked
   against a "mode" [m]. This "mode" describes the separability
   requirement on the type expression (see below for
   more details). The mode [Gamma] maps type variables to modes and
   [Def] records the "mode signature" of the mutually-recursive type
   declarations that are being checked.

   The "mode signature" of a type with parameters [('a, 'b) t] is of the
   form [('a : m1, 'b : m2) t], where [m1] and [m2] are modes. Its meaning
   is the following: a concrete instance [(foo, bar) t] of the type is
   separable if [foo] has mode [m1] and [bar] has mode [m2]. *)

type error =
  | Non_separable_evar of string option
exception Error of Location.t * error
(** Exception raised when a type declaration is not separable, or when its
    separability cannot be established. *)

type mode = Types.Separability.t = Ind | Sep | Deepsep
(** The mode [Sep] ("separable") characterizes types that are indeed separable:
    either they only contain floating-point values, or none of the values
    at this type are floating-point values.
    On a type parameter, it indicates that this parameter must be
    separable for the whole type definition to be separable. For
    example, the mode signature for the type declaration [type 'a
    t = 'a] is [('a : Sep) t]. For the right-hand side to be
    separable, the parameter ['a] must be separable.

    The mode [Ind] ("indifferent") characterizes any type -- separable
    or not.
    On a type parameter, it indicates that this parameter needs not be
    separable for the whole type definition to be separable. For
    example, [type 'a t = 'a * bool] does not require its parameter
    ['a] to be separable as ['a * bool] can never contain [float]
    values. Its mode signature is thus [('a : Ind) t].

    Finally, the mode [Deepsep] ("deeply separable") characterizes
    types that are separable, and whose type sub-expressions are also
    separable. This advanced feature is only used in the presence of
    constraints.
    For example, [type 'a t = 'b   constraint 'a = 'b * bool]
    may not be separable even if ['a] is (its separately depends on 'b,
    a fragment of 'a), so its mode signature is [('a : Deepsep) t].

    The different modes are ordered as [Ind < Sep < Deepsep] (from the least
    demanding to the most demanding). *)

val compute_decl : Env.t -> Types.type_declaration -> mode list
(** [compute_decl env def] returns the signature required
    for the type definition [def] in the typing environment [env]
    -- including signatures for the current recursive block.

    The {!Error} exception is raised if no such signature exists
    -- the definition will always be invalid. This only happens
    when the definition is marked to be unboxed.

    Variant (or record) declarations that are not marked with the
    "[@@unboxed]" annotation, including those that contain several variants
    (or labels), are always separable. In particular, their mode signatures
    do not require anything of their type parameters, which are marked [Ind].

    Finally, if {!Config.flat_float_array} is not set, then separability
    is not required anymore; we just use [Ind] as the mode of each parameter
    without any check.
*)

(** Property interface (see {!Typedecl_properties}). These functions
    rely on {!compute_decl} and raise the {!Error} exception on error. *)
type prop = Types.Separability.signature
val property : (prop, unit) Typedecl_properties.property
val update_decls :
  Env.t ->
  (Ident.t * Typedecl_properties.decl) list ->
  (Ident.t * Typedecl_properties.decl) list
