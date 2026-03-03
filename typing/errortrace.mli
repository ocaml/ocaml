(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*              Antal Spector-Zabusky, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types

type position = First | Second
type order = Less | Equal | More

val swap_position : position -> position
val print_pos : position Format_doc.printer

val swap_order: order -> order

type expanded_type = { ty: type_expr; expanded: type_expr }

(** [trivial_expansion ty] creates an [expanded_type] whose expansion is also
    [ty].  Usually, you want [Ctype.expand_type] instead, since the expansion
    carries useful information; however, in certain circumstances, the error is
    about the expansion of the type, meaning that actually performing the
    expansion produces more confusing or inaccurate output. *)
val trivial_expansion : type_expr -> expanded_type

type 'a diff = { got: 'a; expected: 'a }
type ctx =
  | In_method of string
  | In_tag of string
type 'a ctx_diff = { ctx: ctx option; d: 'a diff }

(** [map_diff f {expected;got}] is [{expected=f expected; got=f got}] *)
val map_diff: ('a -> 'b) -> 'a diff -> 'b diff
val map_cdiff: ('a -> 'b) -> 'a ctx_diff -> 'b ctx_diff

(** Scope escape related errors *)
type 'a escape_kind =
  | Constructor of Path.t
  | Univ of type_expr
  (* The type_expr argument of [Univ] is always a [Tunivar _],
     we keep a [type_expr] to track renaming in {!Printtyp} *)
  | Self
  | Module_type of Path.t
  | Module of Ident.t
  | Equation of 'a
  | Constraint

type 'a escape =
  { kind : 'a escape_kind;
    context : type_expr option }

val map_escape : ('a -> 'b) -> 'a escape -> 'b escape

(** Type indices *)
type unification = private Unification
type comparison  = private Comparison

type fixed_row_case =
  | Cannot_be_closed
  | Cannot_add_tags of string list

type 'variety variant =
  (* Common *)
  | Arity_mismatch: string -> _ variant
  | No_tags : position * (Asttypes.label * row_field) list -> _ variant
  (* Unification *)
  | No_intersection : unification variant
  | Fixed_row :
      position * fixed_row_case * fixed_explanation -> unification variant
  (* Equality & Moregen *)
  | Presence_not_guaranteed_for : position * string -> comparison variant
  | Openness : position (* Always [Second] for Moregen *) -> comparison variant

type 'variety obj =
  (* Common *)
  | Missing_field : position * string -> _ obj
  | Abstract_row : position -> _ obj
  (* Unification *)
  | Self_cannot_be_closed : unification obj

type first_class_module =
    | Package_cannot_scrape of Path.t
    | Package_inclusion of Format_doc.doc
    | Package_coercion of Format_doc.doc

type univar =
  | Var_mismatch of { order:order; diff:type_expr diff }
  | Quantification_mismatch of type_expr list

type highlight_target =
  | Type of Outcometree.highlight_kind * type_expr
  | Type_constructor of Path.t

type highlight_hint = highlight_target list diff

type ('a, 'variety) root =
  (* Common *)
  | Variant : 'variety variant -> ('a, 'variety) root
  | Obj : 'variety obj -> ('a, 'variety) root
  | Escape : 'a escape -> ('a, _) root
  | Function_label_mismatch of Asttypes.arg_label diff
  | Tuple_label_mismatch of string option diff
  | First_class_module: first_class_module -> ('a,_) root
  | Univar of univar
  | Highlight_hint of highlight_hint
  (* Unification & Moregen; included in Equality for simplicity *)
  | Rec_occur : type_expr * type_expr -> ('a, _) root

type ('a, 'variety) t = {
  path: 'a ctx_diff list;
  root: ('a, 'variety) root option
}

type 'variety trace = (type_expr,     'variety) t
type 'variety error = (expanded_type, 'variety) t

val map : ('a -> 'b) -> ('a, 'variety) t -> ('b, 'variety) t

val no_ctx: 'a diff -> 'a ctx_diff
val diff:
  ?ctx:ctx -> got:'a -> expected:'a -> ('a,'variety) t -> ('a,'variety) t
val root: ('a,'variety) root -> ('a,'variety) t
val empty_root: ('a,'variety) t


val incompatible_fields:
  name:string -> got:type_expr -> expected:type_expr ->
  (type_expr, 'v) t -> (type_expr, 'v) t

val in_tag:
  l:string -> (type_expr, 'f) t -> (type_expr, 'f) t

val variant_arity_mismatch: string -> ('any, 'f) root

val highlight_type:
  Outcometree.highlight_kind -> Types.type_expr -> highlight_target list

val swap_trace : ('a, 'variety) t -> ('a, 'variety) t

(** The traces (['variety t]) are the core error types.  However, we bundle them
    up into three "top-level" error types, which are used elsewhere:
    [unification_error], [equality_error], and [moregen_error].  In the case of
    [equality_error], this has to bundle in extra information; in general, it
    distinguishes the three types of errors and allows us to distinguish traces
    that are being built (or processed) from those that are complete and have
    become the final error.  These error types have the invariants that their
    traces are nonempty; we ensure that through three smart constructors with
    matching names. *)

type unification_error = private { trace : unification error } [@@unboxed]

type equality_error = private
  { trace : comparison error;
    subst : (type_expr * type_expr) list }

type moregen_error = private { trace : comparison error } [@@unboxed]

val unification_error : trace:unification error -> unification_error

val equality_error :
  trace:comparison error -> subst:(type_expr * type_expr) list -> equality_error

val moregen_error : trace:comparison error -> moregen_error

(** Wraps up the two different kinds of [comparison] errors in one type *)
type comparison_error =
  | Equality_error of equality_error
  | Moregen_error  of moregen_error

(** Lift [swap_trace] to [unification_error] *)
val swap_unification_error : unification_error -> unification_error

module Subtype : sig

  type 'a t = 'a ctx_diff list
  val diff: ?ctx:ctx -> got:'a -> expected:'a -> 'a t -> 'a t

  (** Just as outside [Subtype], we split traces, completed traces, and complete
      errors.  However, in a minor asymmetry, the name [Subtype.error_trace]
      corresponds to the outside [error] type, and [Subtype.error] corresponds
      to the outside [*_error] types (e.g., [unification_error]).  This [error]
      type has the invariant that the subtype trace is nonempty; note that no
      such invariant is imposed on the unification trace. *)

  type trace       = type_expr t
  type error_trace = expanded_type t

  type unification_error_trace = unification error (** To avoid shadowing *)

  type nonrec error = private
    { trace             : error_trace
    ; unification_trace : unification error }

  val error :
    trace:error_trace -> unification_trace:unification_error_trace -> error

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Structured: sig
(** This module contains helper functions to parse the error trace into the more
    structured type {!Stuctured.t} *)

  (** We extend the core explanation type with promoted explanation generated
      from the main trace *)
  type 'a extended_explanation =
    | Standard of 'a
    | Promoted of highlight_hint option * Format_doc.t
    | Hint of highlight_hint

  type ('a,'b,'c) s = {
    top: ('a ctx_diff * bool) option; (* top = None => tr = [] *)
    tr: 'a ctx_diff list;
    expl: ('b, 'c) root extended_explanation option
  }
  (** The structured version of the trace is split in three parts:
      - {!top} the first element of the trace
      - {!tr} a list of meaningful context difference element
      - {!expl} a root explanation for a type error
   *)

  type printing_status =
    | Discard
    | Keep
    | Context
    (** A {!Context} element marks the entry inside a method or a polymorphic
        variant tag *)
    | Optional_refinement
    (** An [Optional_refinement] printing status is attributed to trace
        elements that are focusing on a new subpart of a structural type.
        Since the whole type should have been printed earlier in the trace,
        we only print those elements if they are the last printed element
        of a trace, and there is no explicit explanation for the
        type error.
    *)

  (** [parse ~promote ~status] builds a structured trace from an unstructured
      one. The [status] function is used to classify elements of the trace,
      whereas the [promote] function describe which kind of trace element might
      be promoted to an extended explanation in the absence of a standard
      explanation. *)
val parse:
    promote:('a diff -> Format_doc.t option) ->
    status:('a ctx_diff -> printing_status) ->
    ('a, 'b) t -> ('a, 'a, 'b) s

end
