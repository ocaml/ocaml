(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Operations on core types *)

open Typedtree

val begin_def: unit -> unit
        (* Raise the variable level by one at the beginning of a definition. *)
val end_def: unit -> unit
        (* Lower the variable level by one at the end of a definition *)
val reset_def: unit -> unit
        (* Reset (to 0) the variable level *)
val newvar: unit -> type_expr
        (* Return a fresh variable *)
val new_global_var: unit -> type_expr
        (* Return a fresh variable, bound at toplevel
           (as type variables ['a] in type constraints). *)
val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)
val generalize: type_expr -> unit
        (* Generalize in-place the given type *)
val make_nongen: type_expr -> unit
        (* Make non-generalizable the given type *)
val instance: type_expr -> type_expr
        (* Take an instance of a type scheme *)
val instance_constructor:
        constructor_description -> type_expr list * type_expr
        (* Same, for a constructor *)
val instance_label: label_description -> type_expr * type_expr
        (* Same, for a label *)
val unify: Env.t -> type_expr -> type_expr -> unit
        (* Unify the two types given. Raise [Unify] if not possible. *)
val filter_arrow: Env.t -> type_expr -> type_expr * type_expr
        (* A special case of unification (with 'a -> 'b). *)
val moregeneral: Env.t -> type_expr -> type_expr -> bool
        (* Check if the first type scheme is more general than the second. *)
val equal: Env.t -> type_expr list -> type_expr ->
                       type_expr list -> type_expr -> bool
        (* [equal env [x1...xn] tau [y1...yn] sigma]
           checks whether the parameterized types
           [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)
val closed_schema: type_expr -> bool
        (* Check whether the given type scheme contains no non-generic
           type variables *)
val nondep_type: Env.t -> Ident.t -> type_expr -> type_expr
        (* Return a type equivalent to the given type but without
           references to the given module identifier. Raise [Not_found]
           if no such type exists. *)
val free_type_ident: Env.t -> Ident.t list -> type_expr -> bool
        (* Test whether one of the given type identifiers occur free
           in the given type expression. *)
val is_generic: type_expr -> bool
        (* Test whether the given type variable is generic *)
val arity: type_expr -> int
        (* Return the arity (as for curried functions) of the given type. *)
val none: type_expr
        (* A dummy type expression *)
val substitute:
        type_expr list -> type_expr list -> type_expr -> type_expr
        (* [substitute [v1...vN] [t1...tN] t]
           returns a copy of [t] where the [vi] are replaced
           by the [ti]. *)

exception Unify

