(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ext_types

(** Various abstract identifiers used in [Flambda] passes. *)

module Variable : sig
  (** [Variable.t] is the equivalent of a non-persistent [Ident.t] in
      the [Flambda] tree.  It wraps an [Ident.t] together with its source
      [compilation_unit].  As such, it is unique within a whole program,
      not just one compilation unit.

      Introducing a new type helps tracing the source of identifier
      when debugging the inliner; and, avoids ident renaming when
      importing cmx files.
  *)

  type t
  include Identifiable with type t := t

  val create : current_compilation_unit:Symbol.Compilation_unit.t -> string -> t

  val unwrap : t -> Ident.t (* For bytecode debugger only *)
  val unique_ident : t -> Ident.t (* For clambdagen only *)
    (* Should we propagate Variable.t into clambda ??? *)

  val rename : current_compilation_unit:Symbol.Compilation_unit.t ->
    ?append:string -> t -> t

  val in_compilation_unit : Symbol.Compilation_unit.t -> t -> bool

  val unique_name : t -> string
end

module Set_of_closures_id : sig
  (** An identifier, unique across the whole program, that identifies a set
      of a closures (viz. [Fset_of_closures]). *)

  type t
  include Identifiable with type t := t

  val create : ?name:string -> Symbol.Compilation_unit.t -> t
end

module Closure_id : sig
  (** An identifier, unique across the whole program (not just one compilation
      unit), that identifies a closure within a particular set of closures
      (viz. [Fselect_closure]). *)

  type t
  include Identifiable with type t := t

  val wrap : Variable.t -> t
  val unwrap : t -> Variable.t

  val in_compilation_unit : Symbol.Compilation_unit.t -> t -> bool
  val get_compilation_unit : t -> Symbol.Compilation_unit.t

  val unique_name : t -> string

  val output_full : out_channel -> t -> unit
end

module Var_within_closure : sig
  (** An identifier, unique across the whole program, that identifies a
      particular variable within a particular closure.  Only
      [Fvar_within_closure], and not [Fvar], nodes are tagged with these
      identifiers. *)

  type t
  include Identifiable with type t := t

  val wrap : Variable.t -> t
  val unwrap : t -> Variable.t

  val in_compilation_unit : Symbol.Compilation_unit.t -> t -> bool

  val unique_name : t -> string
end

module Expr_id : sig
  (** An identifier, unique within compilation units, that is used to tag nodes
      in an [flambda] expression tree. *)

  type t
  include Identifiable with type t := t

  val create : ?name:string -> unit -> t
end

module Static_exception : sig
  (** An identifier that is used to label static exceptions.  Its
      uniqueness properties are unspecified. *)

  type t
  include Identifiable with type t := t

  val create : unit -> t

  val to_int : t -> int
end

module Ident : sig
  include module type of Ident with type t = Ident.t
  module Map : ExtMap with module M := Ident
end

module Variable_connected_components :
  Sort_connected_components.S with module Id := Variable
