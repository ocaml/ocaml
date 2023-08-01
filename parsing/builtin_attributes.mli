(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Support for the builtin attributes:

    - ocaml.alert
    - ocaml.boxed
    - ocaml.deprecated
    - ocaml.deprecated_mutable
    - ocaml.explicit_arity
    - ocaml.immediate
    - ocaml.immediate64
    - ocaml.inline
    - ocaml.inlined
    - ocaml.noalloc
    - ocaml.ppwarning
    - ocaml.tailcall
    - ocaml.unboxed
    - ocaml.untagged
    - ocaml.unrolled
    - ocaml.warnerror
    - ocaml.warning
    - ocaml.warn_on_literal_pattern

    {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)


(** [register_attr] must be called on the locations of all attributes that
    should be tracked for the purpose of misplaced attribute warnings.  In
    particular, it should be called on all attributes that are present in the
    source program except those that are contained in the payload of another
    attribute (because these may be left behind by a ppx and intentionally
    ignored by the compiler).

    The [attr_tracking_time] argument indicates when the attr is being added for
    tracking - either when it is created in the parser or when we see it while
    running the check in the [Ast_invariants] module.  This ensures that we
    track only attributes from the final version of the parse tree: we skip
    adding attributes at parse time if we can see that a ppx will be run later,
    because the [Ast_invariants] check is always run on the result of a ppx.

    Note that the [Ast_invariants] check is also run on parse trees created from
    marshalled ast files if no ppx is being used, ensuring we don't miss
    attributes in that case.
*)
type attr_tracking_time = Parser | Invariant_check
val register_attr : attr_tracking_time -> string Location.loc -> unit

(** Marks the attributes hiding in the payload of another attribute used, for
    the purposes of misplaced attribute warnings (see comment on
    [attr_tracking_time] above).  In the parser, it's simplest to add these to
    the table and remove them later, rather than threading through state
    tracking whether we're in an attribute payload. *)
val mark_payload_attrs_used : Parsetree.payload -> unit

(** Issue misplaced attribute warnings for all attributes created with
    [mk_internal] but not yet marked used. *)
val warn_unused : unit -> unit

val check_alerts: Location.t -> Parsetree.attributes -> string -> unit
val check_alerts_inclusion:
  def:Location.t -> use:Location.t -> Location.t -> Parsetree.attributes ->
  Parsetree.attributes -> string -> unit
val alerts_of_attrs: Parsetree.attributes -> Misc.alerts
val alerts_of_sig: Parsetree.signature -> Misc.alerts
val alerts_of_str: Parsetree.structure -> Misc.alerts

val check_deprecated_mutable:
    Location.t -> Parsetree.attributes -> string -> unit
val check_deprecated_mutable_inclusion:
  def:Location.t -> use:Location.t -> Location.t -> Parsetree.attributes ->
  Parsetree.attributes -> string -> unit

val check_no_alert: Parsetree.attributes -> unit

val error_of_extension: Parsetree.extension -> Location.error

val warning_attribute: ?ppwarning:bool -> Parsetree.attribute -> unit
  (** Apply warning settings from the specified attribute.
      "ocaml.warning"/"ocaml.warnerror" (and variants without the prefix)
      are processed and other attributes are ignored.

      Also implement ocaml.ppwarning (unless ~ppwarning:false is
      passed).
  *)

val warning_scope:
  ?ppwarning:bool ->
  Parsetree.attributes -> (unit -> 'a) -> 'a
  (** Execute a function in a new scope for warning settings.  This
      means that the effect of any call to [warning_attribute] during
      the execution of this function will be discarded after
      execution.

      The function also takes a list of attributes which are processed
      with [warning_attribute] in the fresh scope before the function
      is executed.
  *)

val warn_on_literal_pattern: Parsetree.attributes -> bool
val explicit_arity: Parsetree.attributes -> bool


val immediate: Parsetree.attributes -> bool
val immediate64: Parsetree.attributes -> bool

val has_unboxed: Parsetree.attributes -> bool
val has_boxed: Parsetree.attributes -> bool
