(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Operations on core types *)

open Asttypes
open Typedtree

val generic_level: int
val begin_def: unit -> unit
        (* Raise the variable level by one at the beginning of a definition. *)
val end_def: unit -> unit
        (* Lower the variable level by one at the end of a definition *)
val reset_global_level: unit -> unit
val newty: type_desc -> type_expr
val newgenty: type_desc -> type_expr
val newvar: unit -> type_expr
        (* Return a fresh variable *)
val new_global_var: unit -> type_expr
        (* Return a fresh variable, bound at toplevel
           (as type variables ['a] in type constraints). *)
val newobj: type_expr -> type_expr
val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)
val flatten_fields : type_expr -> (string * type_expr) list * type_expr
      	(* Transform a field type into a list of pairs label-type *)
val generalize: type_expr -> unit
        (* Generalize in-place the given type *)
val make_nongen: type_expr -> unit
        (* Make non-generalizable the given type *)
val instance: type_expr -> type_expr
        (* Take an instance of a type scheme *)
val instance_parameterized_type:
      	type_expr list -> type_expr -> type_expr list * type_expr
val instance_parameterized_type_2:
      	type_expr list -> type_expr list -> type_expr ->
        type_expr list * type_expr list * type_expr
val instance_constructor:
        constructor_description -> type_expr list * type_expr
        (* Same, for a constructor *)
val instance_label: label_description -> type_expr * type_expr
        (* Same, for a label *)
val instance_class:
      	class_type ->
        type_expr list * type_expr list *
        (mutable_flag * type_expr) Vars.t * type_expr
val expand_abbrev:
        Env.t -> Path.t -> type_expr list -> (Path.t * type_expr) list ref ->
	int -> type_expr
	(* Expand an abbreviation *)
val full_expand: Env.t -> type_expr -> type_expr
val expand_root: Env.t -> type_expr -> type_expr
val occur: Env.t -> type_expr -> type_expr -> unit
        (* [occur env var ty] Raise [Unify] if [var] occurs in [ty] *)
val unify: Env.t -> type_expr -> type_expr -> unit
        (* Unify the two types given. Raise [Unify] if not possible. *)
val filter_arrow: Env.t -> type_expr -> type_expr * type_expr
        (* A special case of unification (with 'a -> 'b). *)
val filter_method:
  Env.t -> string -> Typedtree.type_expr -> Typedtree.type_expr
      	(* A special case of unification (with {m : 'a; 'b}). *)
val moregeneral: Env.t -> type_expr -> type_expr -> bool
        (* Check if the first type scheme is more general than the second. *)
val equal: Env.t -> type_expr list -> type_expr ->
                       type_expr list -> type_expr -> bool
        (* [equal env [x1...xn] tau [y1...yn] sigma]
           checks whether the parameterized types
           [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)
val opened_object: type_expr -> bool
val enlarge_type: Env.t -> type_expr list -> type_expr -> type_expr
      	(* Make a type larger *)
val subtype : Env.t -> type_expr list -> type_expr -> type_expr -> unit
val closed_schema: type_expr -> bool
type closed_schema_result = Var of type_expr | Row_var of type_expr
val closed_schema_verbose: type_expr -> closed_schema_result option
        (* Check whether the given type scheme contains no non-generic
           type variables *)
val nondep_type: Env.t -> Ident.t -> type_expr -> type_expr
        (* Return a type equivalent to the given type but without
           references to the given module identifier. Raise [Not_found]
           if no such type exists. *)
val nondep_class_type: Env.t -> Ident.t -> class_type -> class_type
        (* Same for class types. *)
val substitute:
        (Path.t * Typedtree.type_expr) list -> type_expr list ->
        type_expr list -> type_expr -> type_expr
        (* [substitute [v1...vN] [t1...tN] t]
           returns a copy of [t] where the [vi] are replaced
           by the [ti]. *)
val prune:
      	type_expr -> type_expr list -> type_expr * (type_expr * type_expr) list
val prune_class_type:
        class_type ->
        (type_expr * type_expr) list * type_expr list *
      	(mutable_flag * type_expr) Vars.t * type_expr
val close_object: type_expr -> unit
val set_object_name:
      	type_expr -> type_expr list -> Ident.t -> unit
val remove_object_name: type_expr -> unit
val correct_abbrev: Env.t -> Ident.t -> type_expr list -> type_expr -> unit
val unroll_abbrev: Ident.t -> type_expr list -> type_expr -> type_expr
val is_generic: type_expr -> bool
        (* Test whether the given type variable is generic *)
val arity: type_expr -> int
        (* Return the arity (as for curried functions) of the given type. *)
val none: type_expr
        (* A dummy type expression *)

exception Unify of (type_expr * type_expr) list
exception Subtype of
        (type_expr * type_expr) list * (type_expr * type_expr) list
exception Cannot_expand
exception Nonlinear_abbrev
exception Recursive_abbrev
